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

(def printer text->string (projection recursion input input-reference)
  (bind ((output (text/as-string input)))
    (make-iomap/compound projection recursion input input-reference output
                          (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader text->string (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion projection-iomap gesture-queue document))
  operation)
