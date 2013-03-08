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
;;; Data structure

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

(def (function e) make-table/table (rows)
  (make-instance 'table/table :rows rows))

(def (function e) make-table/row (cells)
  (make-instance 'table/row :cells cells))

(def (function e) make-table/cell (content)
  (make-instance 'table/cell :content content))

;;;;;;
;;; Construction

(def (macro e) table/table (() &body rows)
  `(make-table/table (list ,@rows)))

(def (macro e) table/row (() &body cells)
  `(make-table/row (list ,@cells)))

(def (macro e) table/cell (() &body content)
  `(make-table/cell ,(first content)))

;;;;;;
;;; API

(def (function e) table/row-heights (iomap table projection)
  (bind ((rows (rows-of table)))
    (when rows
      (iter (for index :from 0 :below (length rows))
            (collect (iter (for cell :in-sequence (cells-of (elt rows index)))
                           (for content = (content-of cell))
                           (unless (stringp content)
                             (setf content (output-of (recurse-printer projection iomap content nil nil))))
                           (maximizing (+ (funcall 'count #\NewLine content)
                                          (if (or (string= content "")
                                                  (char= #\NewLine (last-elt content)))
                                              0
                                              1)))))))))

(def (function e) table/column-widths (iomap table projection)
  (bind ((rows (rows-of table)))
    (when rows
      (iter (for index :from 0 :below (length (cells-of (first-elt rows))))
            (collect (iter (for row :in-sequence rows)
                           (for content = (content-of (elt (cells-of row) index)))
                           (unless (stringp content)
                             (setf content (output-of (recurse-printer projection iomap content nil nil))))
                           (maximizing (iter (for line :in (split-sequence #\NewLine content))
                                             (maximizing (length line))))))))))

;;;;;;
;;; Provider

(def (function e) table-font-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (border-of ?a)) ?b))
                     (return-from table-font-color-provider
                       (make-style/color 255 196 196 196)))))))
