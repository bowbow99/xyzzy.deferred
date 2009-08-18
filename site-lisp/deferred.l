;;;; -*- mode: lisp; package: deferred -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-19 03:41:36
;;;;

;;;; package
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage "deferred"
    (:use "lisp" "editor")))

(in-package "deferred")

;;;; utilities

(defun partial* (fn initial)
  (lambda (&rest more)
    (apply fn (append initial more))))

(defun partial (fn &rest initial) (partial* fn initial))

(defun later (fn)
  (start-timer 0.016 fn t))

(require "symbol-macrolet")

(defmacro with-struct ((struct &rest slots) object &body body)
  (let ((package (symbol-package struct)))
    (labels ((accessor (slot)
               (intern (concat (symbol-name struct) "-"
                               (symbol-name slot))
                       package)))
      `(let ((,struct ,object))
         (symbol-macrolet
             ,(mapcar (lambda (slot)
                        `(,slot (,(accessor slot) ,struct)))
                slots)
           ,@body)))))

;;;;
;;;; * deferred object
;;;;

(defstruct deferred
  body next result)


(defun fire* (d &optional args)
  (with-struct (deferred next body result) d
    (prog1
        (setf result (multiple-value-list
                      (apply (deferred-body d) args)))
      (fire-next d))))

(defun fire (d &rest args)
  (fire* d args))

(defun fire-next (d)
  (when (deferred-next d)
    (later (partial #'fire* (deferred-next d) (deferred-result d)))))


#|
(defmacro deferred:let (bindings &body body))
(defmacro deferred:when (condition &body body))
(defmacro deferred:while (condition &body body))
(defmacro deferred:loop (&body body))

(defmacro deferred:handler-case (form &rest handlers))

;;; deferred ‚É’l‚ğ•Ô‚³‚¹‚é’j‚Ìl‚Á‚Ä¥¥¥
(defun deferred:mapcar (fn list &rest lists))
(defun deferred:mapcan ...)
(defun deferred:find-if ...)

|#
;;;; deferred.l ends here
