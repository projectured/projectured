;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/recursive (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)))

(def iomap iomap/compound (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)
   (child-iomaps :type list)))

;;;;;;
;;; Constuction

(def (function e) make-iomap/recursive (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/recursive
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference `(the ,(form-type input) ,input-reference))
              :output output :output-reference (when output-reference `(the ,(form-type output) ,output-reference))))

(def (function e) make-iomap/recursive* (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/recursive
              :projection projection :recursion recursion
              :input input :input-reference input-reference
              :output output :output-reference output-reference))

(def (function e) make-iomap/compound (projection recursion input input-reference output output-reference child-iomaps)
  (assert (notany 'null child-iomaps))
  (make-iomap 'iomap/compound
              :projection projection :recursion recursion
              :input input :input-reference input-reference
              :output output :output-reference output-reference
              :child-iomaps child-iomaps))

;;;;;;
;;; Reference applier

(def reference-applier iomap/recursive (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

(def reference-applier iomap/compound (iomap reference function)
  (iter (for child-iomap :in (child-iomaps-of iomap))
        (apply-reference child-iomap reference function)))

;;;;;;
;;; Forwar mapper

(def forward-mapper iomap/recursive (iomap input-reference function)
  (when (tree-search input-reference (input-reference-of iomap))
    (funcall function iomap (tree-replace input-reference (input-reference-of iomap) (output-reference-of iomap)))))

(def forward-mapper iomap/compound (iomap input-reference function)
  (iter (for child-iomap :in (child-iomaps-of iomap))
        (map-forward child-iomap input-reference function)))

;;;;;;
;;; Backward mapper

(def backward-mapper iomap/recursive (iomap output-reference function)
  (when (tree-search output-reference (output-reference-of iomap))
    (funcall function iomap (tree-replace output-reference (output-reference-of iomap) (input-reference-of iomap)))))

(def backward-mapper iomap/compound (iomap output-reference function)
  (iter (for child-iomap :in (child-iomaps-of iomap))
        (map-backward child-iomap output-reference function)))

;;;;;;
;;; Projection

(def projection recursive ()
  ((child :type projection)))

;;;;;;
;;; Construction

(def (function e) make-projection/recursive (projection)
  (make-projection 'recursive :child projection))

;;;;;;
;;; Construction

(def (macro e) recursive (&body forms)
  `(make-projection/recursive ,@forms))

;;;;;;
;;; Printer

(def printer recursive (projection recursion iomap input input-reference output-reference)
  (declare (ignore recursion))
  (bind ((child (child-of projection)))
    (funcall (printer-of child) child projection iomap input input-reference output-reference)))

;;;;;;
;;; Reader

(def reader recursive (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore recursion))
  (bind ((child (child-of projection)))
    (funcall (reader-of child) child projection printer-iomap projection-iomap gesture-queue operation document)))
