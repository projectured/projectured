;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) reference-dispatching ()
  ((default-projection :type projection)
   (reference-projection-pairs :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/reference-dispatching (default-projection reference-projection-pairs)
  (make-projection 'reference-dispatching
                   :default-projection default-projection
                   :reference-projection-pairs reference-projection-pairs))

;;;;;;
;;; Construction

(def (macro e) reference-dispatching (default-projection &body reference-projection-pairs)
  `(make-projection/reference-dispatching
    ,default-projection
    (list ,@(iter (for (reference projection) :in reference-projection-pairs)
                  (collect `(list ',reference ,projection))))))
;;;;;;
;;; Printer

(def printer reference-dispatching (projection recursion input input-reference output-reference)
  (declare (ignore recursion))
  (iter (with default-projection = (default-projection-of projection))
        (with reference-projection-pairs = (reference-projection-pairs-of projection))
        (for (reference projection) :in-sequence reference-projection-pairs)
        (when (equal reference input-reference)
          (return (funcall (printer-of projection) projection projection input input-reference output-reference)))
        (finally (return (funcall (printer-of default-projection) default-projection projection input input-reference output-reference)))))

;;;;;;
;;; Reader

(def reader reference-dispatching (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore recursion))
  (iter (with default-projection = (default-projection-of projection))
        (with reference-projection-pairs = (reference-projection-pairs-of projection))
        (for (reference projection) :in-sequence reference-projection-pairs)
        (when (equal reference (input-reference-of projection-iomap))
          (return (funcall (reader-of projection) projection projection printer-iomap projection-iomap gesture-queue operation document)))
        (finally (return (funcall (reader-of default-projection) default-projection projection printer-iomap projection-iomap gesture-queue operation document)))))
