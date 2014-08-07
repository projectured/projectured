;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text->string ()
  ())

;;;;;;
;;; Construction

(def function make-projection/text->string ()
  (make-projection 'text->string))

;;;;;;
;;; Construction

(def macro text->string ()
  `(make-projection/text->string))

;;;;;;
;;; Printer

(def printer text->string (projection recursion input input-reference)
  (bind ((output (text/as-string input)))
    (make-iomap/compound projection recursion input input-reference output
                          (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader text->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
