;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

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
  (make-instance 'iomap/recursive
                 :projection projection :recursion recursion
                 :input input :input-reference (typed-reference (document-type input) input-reference)
                 :output output :output-reference (typed-reference (document-type output) output-reference)))

(def function make-iomap/compound (projection recursion input input-reference output child-iomaps)
  (make-instance 'iomap/compound :projection projection :recursion recursion
                 :input input :input-reference (typed-reference (document-type input) input-reference)
                 :output output :child-iomaps child-iomaps))

(def function make-iomap/content (projection recursion input input-reference output content-iomap)
  (make-instance 'iomap/content :projection projection :recursion recursion
                 :input input :input-reference (typed-reference (document-type input) input-reference)
                 :output output :content-iomap content-iomap))

;;;;;;
;;; Printer

(def printer recursive ()
  (bind ((child (child-of -projection-)))
    (call-printer child -projection- -input- -input-reference-)))

;;;;;;
;;; Reader

(def reader recursive ()
  (bind ((child (child-of -projection-)))
    (call-reader child -projection- -input- -printer-iomap-)))
