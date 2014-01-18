;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
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

(def (function e) make-projection/iterating (projections &key (test 'eq))
  (make-projection 'iterating
                   :elements projections
                   :test test))

;;;;;;
;;; Construction

(def (macro e) iterating (&body forms)
  `(make-projection/iterating (list ,@forms)))

;;;;;;
;;; Printer

(def printer iterating (projection recursion input input-reference)
  (iter (for output :initially input :then (output-of iomap))
        (for previous-output :previous output)
        (for iomap = (funcall (printer-of projection) projection recursion output input-reference output-reference))
        (collect iomap :into element-iomaps)
        (until (funcall (test-of projection) previous-output output))
        (finally (return (make-iomap/sequential projection recursion input output element-iomaps)))))

;;;;;;
;;; Reader

(def reader iterating (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion projection-iomap gesture-queue operation))
  nil)
