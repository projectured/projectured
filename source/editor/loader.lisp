;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace loader)

(def definer loader (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "LOADER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-loader ',name) ',function-name))))

;;;;;;
;;; API

(def function call-loader (filename)
  (bind ((extension (pathname-type filename)))
    (funcall (find-loader (format-symbol :projectured (string-upcase extension))) filename)))
