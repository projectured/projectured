;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection iterating ()
  ((elements :type list)
   (test :type function)))

;;;;;;
;;; Construction

(def function make-projection/iterating (projections &key (test 'eq))
  (make-projection 'iterating
                   :elements projections
                   :test test))

;;;;;;
;;; Construction

(def macro iterating (&body forms)
  `(make-projection/iterating (list ,@forms)))

;;;;;;
;;; Printer

(def printer iterating (projection recursion input input-reference)
  (iter (for output :initially input :then (output-of iomap))
        (for previous-output :previous output)
        (for iomap = (call-printer projection recursion output input-reference))
        (collect iomap :into element-iomaps)
        (until (funcall (test-of projection) previous-output output))
        (finally (return (make-iomap/sequential projection recursion input output element-iomaps)))))

;;;;;;
;;; Reader

(def reader iterating (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)
