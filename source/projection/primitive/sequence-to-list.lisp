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

(def (function e) make-projection/sequence->list ()
  (make-projection 'sequence->list))

;;;;;;
;;; Construction

(def (macro e) sequence->list ()
  '(make-projection/sequence->list))

;;;;;;
;;; Printer

(def printer sequence->list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-list/list (iter (for index :from 0)
                                       (for element :in-sequence input)
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt ,typed-input-reference ,index)
                                                                     `(elt (the list (elements-of (the list/list ,output-reference))) ,index)))
                                       (collect (make-list/element (output-of iomap)))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

;;;;;;
;;; Reader

(def reader sequence->list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
