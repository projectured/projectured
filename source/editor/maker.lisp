;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace maker)

(def definer maker (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "MAKER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-maker ',name) ',function-name))))

;;;;;;
;;; API

(def function call-maker (filename)
  (bind ((extension (pathname-type filename)))
    (funcall (find-maker (format-symbol :projectured (string-upcase extension))))))
