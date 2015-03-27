;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection preserving ()
  ())

;;;;;;
;;; Construction

(def function make-projection/preserving ()
  (make-projection 'preserving))

;;;;;;
;;; Construction

(def macro preserving ()
  '(make-projection/preserving))

;;;;;;
;;; Printer

(def printer preserving (projection recursion input input-reference)
  (make-iomap/object projection recursion input input-reference input))

;;;;;;
;;; Reader

(def reader preserving (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
