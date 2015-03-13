;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document sql/base ()
  ())

(def document sql/column (sql/base)
  ((name :type string)
   (type :type string :accessor nil)))

(def document sql/column-reference (sql/base)
  ((target :type sql/column)))

(def document sql/table (sql/base)
  ((name :type string)
   (columns :type sequence)))

(def document sql/table-reference (sql/base)
  ((target :type sql/table)))

(def document sql/select (sql/base)
  ((columns :type sequence)
   (tables :type sequence)))

;;;;;;
;;; Construction

(def function make-sql/column (name type &key selection)
  (make-instance 'sql/column :name name :type type :selection selection))

(def function make-sql/column-reference (target &key selection)
  (make-instance 'sql/column-reference :target target :selection selection))

(def function make-sql/table (name columns &key selection)
  (make-instance 'sql/table :name name :columns (ll columns) :selection selection))

(def function make-sql/table-reference (target &key selection)
  (make-instance 'sql/table-reference :target target :selection selection))

(def function make-sql/select (columns tables &key selection)
  (make-instance 'sql/select :columns (ll columns) :tables (ll tables) :selection selection))

;;;;;;
;;; Construction

(def macro sql/column ((&key selection) name type)
  `(make-sql/column ,name ,type :selection ,selection))

(def macro sql/column-reference ((&key selection) &body target)
  `(make-sql/column-reference ,(first target) :selection ,selection))

(def macro sql/table ((&key selection) name &body columns)
  `(make-sql/table ,name (list-ll ,@columns) :selection ,selection))

(def macro sql/table-reference ((&key selection) &body target)
  `(make-sql/table-reference ,(first target) :selection ,selection))

(def macro sql/select ((&key selection) columns tables)
  `(make-sql/select ,columns ,tables :selection ,selection))
