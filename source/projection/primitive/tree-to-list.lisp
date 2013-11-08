;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)


;;;;;;
;;; Projection

(def (projection e) tree/node->list ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/tree/node->list ()
  (make-projection 'tree/node->list))

;;;;;;
;;; Construction

(def (macro e) tree/node->list ()
  '(make-projection/tree/node->list))

;;;;;;
;;; Printer

(def printer tree/node->list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (iter (for child :in-sequence (children-of input))
                       (for index :from 0)
                       (for iomap = (recurse-printer recursion iomap child `(elt (the list (children-of ,typed-input-reference)) ,index) `(elt (the list ,output-reference) ,index)))
                       (push iomap child-iomaps)
                       (collect (output-of iomap)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader tree/node->list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
