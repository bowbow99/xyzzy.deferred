;;;; -*- mode: lisp; package: deferred -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-22 13:47:29
;;;;

;;;; * package
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage "deferred"
    (:use "lisp" "editor")))

(in-package "deferred")

(export '(deferred deferred-w/handlers
          fire fire*
          add-callbacks reproduce
          *deferred*))

;;;; * utilities

(defun callable-p (obj)
  "Return non-nil if obj is a function or function-name."
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))

(defmacro async (&body body)
  "Execute body asynchronously."
  `(start-timer 0 (lambda () ,@body) t))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require "symbol-macrolet"))

(defmacro with-struct ((struct &rest slots) object &body body)
  "Evaluate body in environment that symbols specified as struct and slots to
 object and its corresponding value."
  (let ((package (symbol-package struct)))
    (labels ((accessor (slot)
               (intern (apply #'concat (mapcar #'symbol-name
                                         (list struct '- slot)))
                       package))
             (gen-symbol/expansion (slot)
               `(,slot (,(accessor slot) ,struct))))
      `(let ((,struct ,object))
         (symbol-macrolet ,(mapcar #'gen-symbol/expansion slots)
           ,@body)))))

(defun values-places (object &rest places)
  (values-list (mapcar (lambda (place)
                         (funcall place object))
                 places)))

(defun uncons (object)
  (if (consp object)
      (values-places object #'car #'cdr)
    object))
;;;;
;;;; * deferred object
;;;;

(defstruct *deferred
  description body result callback condition handlers)

(defmacro with-deferred (slots d &body body)
  `(with-struct (*deferred ,@slots) ,d ,@body))

(setf (get 'with-deferred 'lisp-indent-hook) 2)

;;;; create deferred object

(defmacro deferred (args &body body)
  "Make new deferred object."
  (let ((desc (when (stringp (car body))
                (prog1 (car body) (setq body (cdr body)))))
        (handlers (let ((last (last body)))
                    (when (and (consp last)
                               (consp (car last))
                               (eq (caar last) :handlers))
                      (prog1 (cdar (last body))
                        (setq body (butlast body)))))))
    `(make-*deferred
      :description ,desc
      :body ,(if body `(lambda ,args ,@body)
               `(lambda (&rest #1=#:args) (apply ,args #1#)))
      :handlers (list ,@(mapcar (lambda (handler)
                                  `(list ',(car handler)
                                         (lambda ,@(cdr handler))))
                          handlers)))))

(defmacro deferred-w/handlers (args body &rest handlers)
  "Make new deferred object with handlers."
  (let ((d (gensym)))
    `(let ((,d (deferred ,args ,body)))
       (setf ,@(mapcan (lambda (condition/handler)
                         (multiple-value-bind (condition handler)
                             (uncons condition/handler)
                           `((handler ,d ,condition)
                             (lambda ,@handler))))
                 handlers))
       ,d)))

(setf (get 'deferred 'lisp-indent-hook) 'defun
      (get 'deferred-w/handlers 'lisp-indent-hook) '2)

;;;; execute deferred

(defun fire* (d &optional args)
  "Invoke deferred's own process."
  (with-deferred (result condition body) d
    (async
     (multiple-value-setq (result condition)
       (handler-case
           (values (let ((*deferred* d))
                     (declare (special *deferred*))
                     (multiple-value-list (apply body args)))
                   :returned)
         (condition (c)
           (values nil c))))
     (if (eql condition :returned)
         (fire-next d)
       (fire-recover d))
     d)))

(defun fire (d &rest args)
  (fire* d args))

(defun fire-next (d)
  "Invoke next callback of [d]eferred."
  (with-deferred (condition callback result) d
    (when (and (eql condition :returned)
               callback)
      (async
       (fire* callback result)))
    d))

(defun fire-recover (deferred)
  (with-deferred (condition handlers result) deferred
    (let ((handler (cadr (find-if (lambda (handler)
                                    (typep condition (car handler)))
                           handlers))))
      (when (and (typep condition 'condition) handler)
        (fire (deferred (c)
                (when (setf result (multiple-value-list (funcall handler c))
                            condition :returned)
                  (fire-next deferred)))
              condition)))))

;;;; get/set callback(s)

(defun last-callback (deferred)
  "Return last callback of deferred chain."
  (if (null (*deferred-callback deferred))
      deferred
    (last-callback (*deferred-callback deferred))))

(defun add-callback-1 (deferred callback)
  (setf (*deferred-callback deferred) callback)
  (fire-next deferred))

(defun add-callback-last (deferred callback)
  "Put callback on next of last callback of deferred chain."
  (prog1 deferred
    (add-callback-1 (last-callback deferred) callback)))

(defun add-callbacks (deferred &rest callbacks)
  "Put callbacks after last callback of deferred chain."
  (if (null callbacks)
      deferred
    (apply #'add-callbacks
           (add-callback-last deferred (car callbacks))
           (cdr callbacks))))

(defun insert-callback (deferred callback)
  (let ((rest (*deferred-callback deferred)))
    (add-callback-1 deferred (add-callback-last callback rest))))

;;;; get/set handler

(defun handler (deferred condition)
  (find condition (*deferred-handlers deferred) :key #'car))

(defsetf handler (deferred condition) (handler)
  (let ((d (gensym)))
    `(let ((,d ,deferred))
       (setf (*deferred-handlers ,d)
             (sort (cons (list ',condition ,handler)
                         (delete ',condition (*deferred-handlers ,d) :key #'car))
                   #'subtypep)))))


;;;; reproduce deferred's result

(defun reproduce (deferred)
  "Return what deferred chain returned, or signal what it signaled."
  (with-deferred (condition result) (last-callback deferred)
    (case condition
      (:returned (values-list result))
      (t (error condition)))))

;;;; deferred.l ends here
