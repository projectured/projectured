;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sequence->list ()
  ())

;;;;;;
;;; Construction

(def function make-projection/sequence->list ()
  (make-projection 'sequence->list))

;;;;;;
;;; Construction

(def macro sequence->list ()
  '(make-projection/sequence->list))

;;;;;;
;;; Printer

(def printer sequence->list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-list/list (iter (for index :from 0)
                                       (for element :in-sequence input)
                                       (for iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,index)
                                                                       ,@(typed-reference (form-type input) input-reference))))
                                       (collect (make-list/element (output-of iomap)))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader sequence->list (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  input)
