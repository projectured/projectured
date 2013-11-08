;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) string->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/string->tree/leaf ()
  (make-projection 'string->tree/leaf))

;;;;;;
;;; Printer

(def printer string->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-tree/leaf input)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion
                                                   input input-reference
                                                   output output-reference)
                                (make-iomap/string input input-reference 0
                                                   input `(content-of (the tree/leaf ,output-reference)) 0
                                                   (length input))))))

;;;;;;
;;; Reader

(def reader string->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
