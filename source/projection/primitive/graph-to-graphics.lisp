;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection graph->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/graph->graphics ()
  (make-projection 'graph->graphics))

;;;;;;
;;; Construction

(def (macro e) graph->graphics ()
  '(make-projection/graph->graphics))

;;;;;;
;;; Printer

(def printer graph->graphics (projection recursion input input-reference)
  nil)

;;;;;;
;;; Reader

(def reader graph->graphics (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion projection-iomap gesture-queue document))
  operation)
