;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sequential ()
  ((elements :type list)))

;;;;;;
;;; Construction

(def function make-projection/sequential (projections)
  (make-projection 'sequential :elements projections))

;;;;;;
;;; Construction

(def macro sequential (&body forms)
  `(make-projection/sequential (list ,@forms)))

;;;;;;
;;; IO map

(def iomap iomap/sequential ()
  ((element-iomaps :type list)))

;;;;;;
;;; Construction

(def function make-iomap/sequential (projection recursion input input-reference output element-iomaps)
  (make-instance 'iomap/sequential
                 :projection projection :recursion recursion
                 :input input :output output
                 :input-reference (typed-reference (document-type input) input-reference)
                 :element-iomaps element-iomaps))

;;;;;;
;;; Printer

(def printer sequential (projection recursion input input-reference)
  (bind ((element-iomaps (as (iter (for output :initially input :then (output-of element-iomap))
                                   (for element :in (elements-of projection))
                                   (for element-input-reference :initially input-reference :then element-output-reference)
                                   (for element-output-reference = `((printer-output (the ,(document-type output) document) ,element ,recursion) ,@(typed-reference (document-type output) element-input-reference)))
                                   (for element-iomap = (call-printer element recursion output element-input-reference))
                                   (collect element-iomap :into element-iomaps)
                                   (finally (return element-iomaps))))))
    (make-iomap/sequential projection recursion input input-reference (as (output-of (last-elt (va element-iomaps)))) element-iomaps)))

;;;;;;
;;; Reader

(def reader sequential (projection recursion input printer-iomap)
  (iter (for element-iomap :in (reverse (element-iomaps-of printer-iomap)))
        (for element :in (reverse (elements-of projection)))
        (for current-output :initially input :then (call-reader element recursion current-output element-iomap))
        (finally (return current-output))))
