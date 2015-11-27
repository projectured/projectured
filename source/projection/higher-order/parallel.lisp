;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection parallel ()
  ((elements :type list)))

;;;;;;
;;; Construction

(def function make-projection/parallel (projections)
  (make-projection 'parallel :elements projections))

;;;;;;
;;; Construction

(def macro parallel (&body forms)
  `(make-projection/parallel (list ,@forms)))

;;;;;;
;;; IO map

(def iomap iomap/parallel ()
  ((element-iomaps :type list)))

;;;;;;
;;; Construction

(def function make-iomap/parallel (input output element-iomaps)
  (make-instance 'iomap/parallel :input input :output output :element-iomaps element-iomaps))

;;;;;;
;;; Printer

(def printer parallel (projection recursion input input-reference)
  (iter (for element :in (elements-of projection))
        (for iomap = (call-printer element recursion input input-reference))
        (collect iomap :into element-iomaps)
        (finally (return (make-iomap/parallel input (mapcar 'output-of element-iomaps) element-iomaps)))))

;;;;;;
;;; Reader

(def reader parallel (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
