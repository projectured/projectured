;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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

(def printer preserving ()
  (make-iomap -projection- -recursion- -input- -input-reference- -input-))

;;;;;;
;;; Reader

(def reader preserving ()
  -input-)
