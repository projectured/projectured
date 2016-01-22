;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Definition

(def namespace printer)

(def definer printer (name arguments &body forms)
  (bind ((function-name (format-symbol :projectured "PRINTER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-printer ',name) ',function-name))))

;;;;;;;
;;; Printer

(def function call-printer (projection recursion input input-reference)
  (declare (type projection projection recursion)
           (type t input)
           (type reference input-reference))
  (the t (funcall (printer-of projection) projection recursion input input-reference)))

(def function recurse-printer (recursion input input-reference)
  (call-printer recursion recursion input input-reference))

(def function apply-printer (input projection &optional (recursion (make-projection/preserving)))
  (call-printer projection recursion input nil))

(def function printer-output (input projection &optional (recursion (make-projection/preserving)))
  (output-of (call-printer projection recursion input nil)))
