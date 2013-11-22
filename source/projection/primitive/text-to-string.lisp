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

(def (function e) make-projection/text->string ()
  (make-projection 'text->string))

;;;;;;
;;; Construction

(def (macro e) text->string ()
  `(make-projection/text->string))

;;;;;;
;;; Printer

(def printer text->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (text/as-string input)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)))))

;;;;;;
;;; Reader

(def reader text->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
