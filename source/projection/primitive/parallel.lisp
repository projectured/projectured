;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def document iomap/parallel (iomap)
  ((element-iomaps :type list)))

;;;;;;
;;; Construction

(def (function e) make-iomap/parallel (input output element-iomaps)
  (make-iomap 'iomap/parallel :input input :output output :element-iomaps element-iomaps))

;;;;;;
;;; Projection

(def (projection e) parallell ()
  ((elements :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/parallell (projections)
  (make-projection 'parallell :elements projections))

;;;;;;
;;; Construction

(def (macro e) parallell (&body forms)
  `(make-projection/parallell (list ,@forms)))

;;;;;;
;;; Printer

(def printer parallell (projection recursion input input-reference output-reference)
  (iter (for element :in (elements-of projection))
        (for iomap = (funcall (printer-of element) element recursion input input-reference output-reference))
        (collect iomap :into element-iomaps)
        (finally (return (make-iomap/parallel input (mapcar 'output-of element-iomaps) element-iomaps)))))

;;;;;;
;;; Reader

(def reader parallell (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
