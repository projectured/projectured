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

(def function make-projection/graph->graphics ()
  (make-projection 'graph->graphics))

;;;;;;
;;; Construction

(def macro graph->graphics ()
  '(make-projection/graph->graphics))

;;;;;;
;;; Printer

(def printer graph->graphics (projection recursion input input-reference)
  nil)

;;;;;;
;;; Reader

(def reader graph->graphics (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
