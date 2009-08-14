;;;; -*- mode: lisp; package: deferred -*-
;;;;
;;;; deferred.l -- deferred for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-08-13 19:21:33
;;;; Updated: 2009-08-14 20:09:18
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
  (start-timer 0 fn t))


(require "symbol-macrolet")

(defmacro with-struct ((prefix &rest slots) obj &body body)
  (let ((package (symbol-package prefix))
        (template (concat (symbol-name prefix) "-~A")))
    (labels ((accessor (slot)
               (intern (format nil template (symbol-name slot)) package)))
      `(let ((,prefix ,obj))
         (symbol-macrolet ,(mapcar (lambda (slot)
                                     `(,slot (,(accessor slot) ,prefix)))
                             slots)
           ,@body)))))



;;;; deferred object
(defstruct deferred
  (id (genid))
  thunk
  values condition
  next handlers)

(defparameter *deferreds* nil)

(defun get-deferred (id &optional (no-error nil sv))
  (if (deferred-p id)
      id
    (or (find id *deferreds* :key #'deferred-id)
        (if sv no-error
          (error "deferred not found: ~S" id)))))


(defmacro build-deferred (args &body body)
  (let ((c (gensym)))
    `(with-struct (deferred thunk next id values condition handlers)
         (make-deferred)
       (setf thunk
             (lambda ,args
               (handler-case
                   (setq values (multiple-value-list (progn ,@body)))
                 (condition (,c)
                   (setq condition ,c)
                   (when handlers
                     (handle deferred condition))))
               (when next
                 (fire* next values)))
             *deferreds*
             (cons deferred (delete id *deferreds* :key #'deferred-id)))
       id)))
(setf (get 'build-deferred 'lisp-indent-hook) 'defun)


;;;; deferred id
(defparameter *id-counter* 0)

(let ((pkg (find-package "deferred")))
  (defun genid ()
    (intern (format nil "$~S" (incf *id-counter*)) pkg)))

;;;; invoke
(defun fire* (id &optional args)
  (later (partial* (deferred-thunk (get-deferred id)) args)))

(defun fire (id &rest args) (fire* id args))

(defun fire-next (id)
  (with-struct (deferred values next) (get-deferred id)
    (when (and values next)
      (fire* next values))))





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
