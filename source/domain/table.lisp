;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Table domain provides:
;;;;  - table
;;;;  - row
;;;;  - cell

;;;;;;
;;; Document

(def document table/base ()
  ())

(def document table/table (table/base)
  ((rows :type sequence)))

(def document table/row (table/base)
  ((cells :type sequence)))

(def document table/cell (table/base)
  ((content :type t)))

;;;;;;
;;; Construction

(def function make-table/table (rows)
  (make-instance 'table/table :rows rows))

(def function make-table/row (cells)
  (make-instance 'table/row :cells cells))

(def function make-table/cell (content)
  (make-instance 'table/cell :content content))

;;;;;;
;;; Construction

(def macro table/table (() &body rows)
  `(make-table/table (list ,@rows)))

(def macro table/row (() &body cells)
  `(make-table/row (list ,@cells)))

(def macro table/cell (() &body content)
  `(make-table/cell ,(first content)))
