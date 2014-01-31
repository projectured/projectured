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

#+nil
(def reference-applier iomap/preserving (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

;;;;;;
;;; Forward mapper

#+nil
(def forward-mapper iomap/preserving (iomap input-reference function)
  (funcall function iomap input-reference))

;;;;;;
;;; Backward mapper

#+nil
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

(def reader preserving (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
