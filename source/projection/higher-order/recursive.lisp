;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/recursive ()
  ((input-reference :type reference)
   (output-reference :type reference)))

(def iomap iomap/compound ()
  ((input-reference :type reference)
   (child-iomaps :type sequence)))

(def iomap iomap/content ()
  ((content-iomap :type iomap)))

;;;;;;
;;; Constuction

(def function make-iomap/recursive (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/recursive
              :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :output-reference (typed-reference (form-type output) output-reference)))

(def function make-iomap/compound (projection recursion input input-reference output child-iomaps)
  (make-iomap 'iomap/compound :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :child-iomaps child-iomaps))

(def function make-iomap/content (projection recursion input input-reference output content-iomap)
  (make-iomap 'iomap/content :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :content-iomap content-iomap))

;;;;;;
;;; Projection

(def projection recursive ()
  ((child :type projection)))

;;;;;;
;;; Construction

(def function make-projection/recursive (projection)
  (make-projection 'recursive :child projection))

;;;;;;
;;; Construction

(def macro recursive (&body forms)
  `(make-projection/recursive ,@forms))

;;;;;;
;;; Printer

(def printer recursive (projection recursion input input-reference)
  (declare (ignore recursion))
  (bind ((child (child-of projection)))
    (call-printer child projection input input-reference)))

;;;;;;
;;; Reader

(def reader recursive (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (bind ((child (child-of projection)))
    (call-reader child projection input printer-iomap)))
