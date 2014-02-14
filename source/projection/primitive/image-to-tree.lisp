;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection image/image->tree/leaf ()
  ())

(def (function e) make-projection/image/image->tree/leaf ()
  (make-projection 'image/image->tree/leaf))

;;;;;;
;;; Printer

(def printer image/image->tree/leaf (projection recursion input input-reference)
  (bind ((output (make-tree/leaf input)))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader image/image->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
