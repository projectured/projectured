;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace reader)

(def definer reader (name arguments &body forms)
  (bind ((function-name (format-symbol :projectured "READER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-reader ',name) ',function-name))))

;;;;;;;
;;; API

(def function call-reader (projection recursion input printer-iomap)
  (declare (type projection projection recursion)
           (type command input))
  (the command (funcall (reader-of projection) projection recursion input printer-iomap)))

(def function recurse-reader (recursion input printer-iomap)
  (call-reader recursion recursion input printer-iomap))

(def function apply-reader (input projection printer-iomap &optional (recursion (make-projection/preserving)))
  (call-reader projection recursion input printer-iomap))

(def function reader-output (input projection printer-iomap &optional (recursion (make-projection/preserving)))
  (output-of (call-reader projection recursion input printer-iomap)))
