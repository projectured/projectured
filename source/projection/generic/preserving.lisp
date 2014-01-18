;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/preserving (iomap)
  ())

;;;;;;
;;; Reference applier

(def reference-applier iomap/preserving (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

;;;;;;
;;; Forward mapper

(def forward-mapper iomap/preserving (iomap input-reference function)
  (funcall function iomap input-reference))

;;;;;;
;;; Backward mapper

(def backward-mapper iomap/preserving (iomap output-reference function)
  (funcall function iomap output-reference))

;;;;;;
;;; Projection

(def projection preserving ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/preserving ()
  (make-projection 'preserving))

;;;;;;
;;; Construction

(def (macro e) preserving ()
  '(make-projection/preserving))

;;;;;;
;;; Printer

(def printer preserving (projection recursion input input-reference)
  (declare (ignore projection recursion input-reference))
  (make-iomap 'iomap/preserving :input input :output input))

;;;;;;
;;; Reader

(def reader preserving (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion projection-iomap gesture-queue document))
  operation)
