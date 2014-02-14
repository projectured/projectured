;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/recursive (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)))

(def iomap iomap/compound (iomap)
  ((input-reference :type reference)
   (child-iomaps :type list)))

;;;;;;
;;; Constuction

(def (function e) make-iomap/recursive (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/recursive
              :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :output-reference (typed-reference (form-type output) output-reference)))

(def (function e) make-iomap/compound (projection recursion input input-reference output child-iomaps)
  (make-iomap 'iomap/compound :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :child-iomaps child-iomaps))

;;;;;;
;;; Projection

(def projection recursive ()
  ((child :type projection)))

;;;;;;
;;; Construction

(def (function e) make-projection/recursive (projection)
  (make-projection 'recursive :child projection))

;;;;;;
;;; Construction

(def (macro e) recursive (&body forms)
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
