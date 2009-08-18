;;;; -*- mode: lisp; package: deferred -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-19 03:06:09
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
