;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection nesting ()
  ((elements :type list)
   (rest :type nesting)))

;;;;;;
;;; Construction

(def function make-projection/nesting (elements)
  (make-projection 'nesting :elements elements :rest (when elements (make-projection/nesting (rest elements)))))

;;;;;;
;;; Construction

(def macro nesting (&body forms)
  `(make-projection/nesting (list ,@forms)))

;;;;;;
;;; Printer

(def printer nesting (projection recursion input input-reference)
  (bind ((elements (elements-of projection))
         (output-iomap (if elements
                           (bind ((first-element (first elements)))
                             (call-printer first-element (rest-of projection) input input-reference))
                           (recurse-printer recursion input input-reference))))
    (make-iomap/compound projection recursion input input-reference (output-of output-iomap) (list output-iomap))))

;;;;;;
;;; Reader

(def reader nesting (projection recursion input printer-iomap)
  (bind ((elements (elements-of projection)))
    (if elements
        (bind ((first-element (first elements)))
          (call-reader first-element (rest-of projection) input (the-only-element (child-iomaps-of printer-iomap))))
        (recurse-reader recursion input (the-only-element (child-iomaps-of printer-iomap))))))
