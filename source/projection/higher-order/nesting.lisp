;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection nesting ()
  ((elements :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/nesting (elements)
  (make-projection 'nesting :elements elements))

;;;;;;
;;; Construction

(def (macro e) nesting (&body forms)
  `(make-projection/nesting (list ,@forms)))

;;;;;;
;;; Printer

(def printer nesting (projection recursion iomap input input-reference output-reference)
  (bind ((elements (elements-of projection))
         (output-iomap (if elements
                           (bind ((first-element (first elements)))
                             (funcall (printer-of first-element) first-element (make-projection/nesting (rest elements)) iomap input input-reference output-reference))
                           (recurse-printer recursion iomap input input-reference output-reference))))
    (make-iomap/compound projection recursion input input-reference (output-of output-iomap) output-reference
                          (list output-iomap))))

;;;;;;
;;; Reader

(def reader nesting (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (bind ((elements (elements-of projection)))
    (if elements
        (bind ((first-element (first elements)))
          (funcall (reader-of first-element) first-element (make-projection/nesting (rest elements)) printer-iomap (the-only-element (child-iomaps-of projection-iomap)) gesture-queue operation document))
        (recurse-reader recursion printer-iomap (the-only-element (child-iomaps-of projection-iomap)) gesture-queue operation document))))
