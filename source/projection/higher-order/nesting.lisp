;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection nesting ()
  ((elements :type list)
   (recursion :type projection)))

;;;;;;
;;; Construction

(def function make-projection/nesting (elements &optional recursion)
  (make-projection 'nesting :elements elements :recursion recursion))

;;;;;;
;;; Construction

(def macro nesting (&body forms)
  `(make-projection/nesting (list ,@forms)))

;;;;;;
;;; Printer

(def printer nesting (projection recursion input input-reference)
  (bind ((elements (elements-of projection))
         (output-iomap (if elements
                           (bind ((recursion (make-projection/nesting (rest elements) (or (recursion-of projection) recursion))))
                             (call-printer (first elements) recursion input input-reference))
                           (recurse-printer (recursion-of projection) input input-reference))))
    (make-iomap/compound projection recursion input input-reference (output-of output-iomap) (list output-iomap))))

;;;;;;
;;; Reader

(def reader nesting (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (bind ((elements (elements-of projection))
         (content-iomap (the-only-element (child-iomaps-of printer-iomap))))
    (if elements
        (call-reader (first elements) (recursion-of content-iomap) input content-iomap)
        (recurse-reader (recursion-of projection) input content-iomap))))
