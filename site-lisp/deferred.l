;;;; -*- mode: lisp; package: deferred/core -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-21 10:21:41
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
  description body result callback condition)

(defmacro with-deferred (d &body body)
  `(with-struct (*deferred description body result callback condition) ,d
     ,@body))

(setf (get 'with-deferred 'lisp-indent-hook) 1)

(defun fire* (d &optional args)
  "Invoke [d]eferred's own process."
  (with-deferred d
    (async
     (setf result (multiple-value-list
                   (let ((*deferred* d))
                     (declare (special *deferred*))
                     (apply body args)))
           condition :returned)
     (fire-next d))
    d))

(defun fire (d &rest a
              rgs)
  (fire* d args))

(defun fire-next (d)
  "Invoke next callback of [d]eferred."
  (with-deferred d
    (when (and (eql condition :returned)
               callback)
      (async
       (fire* callback result)))
    d))

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

(defun defer* (fn &optional args)
  "Make and invoke new deferred that calls function with args."
  (fire* (deferred (&rest args) (apply fn args)) args))

(defun defer (fn &rest args)
  "Make and invoke new deferred that calls function with args."
  (defer* fn args))

(defun last-callback (deferred)
  "Return last callback of deferred chain."
  (if (null (*deferred-callback deferred))
      deferred
    (last-callback (*deferred-callback deferred))))

(defun add-callback-1 (deferred callback)
  "Put callback on next of last callback of deferred chain."
  (let ((last (last-callback deferred)))
    (prog1
        (setf (*deferred-callback last) callback)
      (fire-next last))))

(defun add-callbacks (deferred &rest callbacks)
  "Put callbacks after last callback of deferred chain."
  (if (null callbacks)
      deferred
    (apply #'add-callbacks
           (add-callback-1 deferred (car callbacks))
           (cdr callbacks))))

(defun reproduce (deferred)
  "Return what deferred chain returned, or signal what it signaled."
  (with-deferred (last-callback deferred)
    (case condition
      (:returned (values-list result))
      (t (error condition)))))

;;;; deferred.l ends here
