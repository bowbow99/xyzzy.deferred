;;;; -*- mode: lisp; package: deferred/core -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-22 09:20:28
;;;;

;;;; * package
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage "deferred/core"
    (:use "lisp" "editor")))

(in-package "deferred/core")

(export '(deferred defer defer*
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

(require "symbol-macrolet")

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

(defun split-documentation (body)
  "Return documentation string and forms as multiple values."
  (if (stringp (car body))
      (values (car body) (cdr body))
    (values nil body)))

;;;;
;;;; * deferred object
;;;;

(defstruct *deferred
  description body result callback condition handlers)

(defmacro with-deferred (slots d &body body)
  `(with-struct (*deferred ,@slots) ,d ,@body))

(setf (get 'with-deferred 'lisp-indent-hook) 2)


;;;; execute deferred

(defun fire* (d &optional args)
  "Invoke [d]eferred's own process."
  (with-deferred (result condition body) d
    (async
     (multiple-value-setq (result condition)
       (handler-case (let ((*deferred* d))
                       (declare (special *deferred*))
                       (apply body args))
         (condition (c)
           (values nil c))
         (:no-error (&rest values)
          (values values :returned))))
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
  (with-deferred (condition handlers callback) deferred
    (when (typep condition 'condition)
      (let ((handler (cadr (find-if (lambda (handler)
                                      (typep condition (car handler)))
                             handlers))))
        (when handler
          (let ((d (deferred (condition)
                     (funcall handler condition))))
            (add-callback-1 d callback)
            (fire d condition)))))))


;;;; create deferred object



(defmacro deferred (args &body body)
  "Make new deferred object."
  (multiple-value-bind (doc body~)
      (split-documentation body)
    `(make-*deferred
      :description ,doc
      :body ,(if body
                 `(lambda ,args ,@body)
               `(let ((#1=#:fn ,args))
                  (lambda (&rest #2=#:args) (apply #1# #2#)))))))


(defmacro deferred (args &body body)
  "Make new deferred object."
  (let ((desc (when (stringp (car body))
                (prog1 (car body) (setq body (cdr body)))))
        (handlers (when (eq (caar (last body)) :handlers)
                    (prog1 (cdar (last body))
                      (setq body (butlast body))))))
    `(make-*deferred
      :description ,desc
      :body ,(if body `(lambda ,args ,@body)
               `(lambda (&rest #1=#:args) (apply ,args #1#)))
      :handlers (list ,@(mapcar (lambda (handler)
                                  `(list ',(car handler)
                                         (lambda ,@(cdr handler))))
                          handlers)))))


(defun defer* (fn &optional args)
  "Make and invoke new deferred that calls function with args."
  (fire* (deferred (&rest args) (apply fn args)) args))

(defun defer (fn &rest args)
  "Make and invoke new deferred that calls function with args."
  (defer* fn args))


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

;;;; reproduce deferred's result

(defun reproduce (deferred)
  "Return what deferred chain returned, or signal what it signaled."
  (with-deferred (condition result) (last-callback deferred)
    (case condition
      (:returned (values-list result))
      (t (error condition)))))

;;;; deferred.l ends here
