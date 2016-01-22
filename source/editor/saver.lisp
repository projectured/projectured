;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace saver)

(def definer saver (name arguments &body forms)
  (bind ((function-name (format-symbol :projectured "SAVER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-saver ',name) ',function-name))))

;;;;;;
;;; API

(def function call-saver (filename document)
  (bind ((extension (pathname-type filename)))
    (funcall (find-saver (format-symbol :projectured (string-upcase extension))) filename document)))
