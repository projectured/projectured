;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection tree/node->list ()
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

(def printer tree/node->list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (iter (for child :in-sequence (children-of input))
                       (for index :from 0)
                       (for iomap = (recurse-printer recursion child `(elt (the list (children-of ,typed-input-reference)) ,index)))
                       (push iomap child-iomaps)
                       (collect (output-of iomap)))))
    (make-iomap/compound projection recursion input input-reference output
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader tree/node->list (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion projection-iomap gesture-queue operation))
  nil)
