;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection removing ()
  ((element :type t)
   (predicate :type function)
   (key :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/removing (element predicate key)
  (make-projection 'removing
                   :element element
                   :predicate predicate
                   :key key))

;;;;;;
;;; Construction

(def (macro e) removing (element predicate key)
  `(make-projection/removing ,element ,predicate ,key))

;;;;;
;;; IO map

(def iomap iomap/removing (iomap)
  ((element-iomaps :type sequence)
   (input-indices :type sequence)))

;;;;;;
;;; Printer

(def printer removing (projection recursion input input-reference)
  (bind ((input-indices nil)
         (element-iomaps (iter (for index :from 0)
                               (for element :in-sequence input)
                               (collect (recurse-printer recursion element `((elt (the list document) ,index)
                                                                             ,@(typed-reference (form-type input) input-reference))))))
         (key (key-of projection))
         (predicate (predicate-of projection))
         (output (coerce (iter (for input-index :from 0)
                               (for input-element :in-sequence input)
                               (for output-element = (output-of (elt element-iomaps input-index)))
                               (unless (funcall predicate (element-of projection) (funcall key input-element))
                                 (collect output-element)
                                 (push input-index input-indices)))
                         (cond ((stringp input)
                                'string)
                               ((listp input)
                                'list)
                               ((vectorp input)
                                'vector)))))
    (make-iomap 'iomap/removing
                :projection projection :recursion recursion
                :input input :output output
                :element-iomaps element-iomaps
                :input-indices (nreverse input-indices))))

;;;;;;
;;; Reader

(def reader removing (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (labels ((recurse (operation)
             (typecase operation
               (operation/quit operation)
               (operation/replace-selection
                (pattern-case (reverse (selection-of operation))
                  (((the ?type (elt (the list document) ?output-element-index))
                    . ?rest)
                   (bind ((input-element-index (elt (input-indices-of projection-iomap) ?output-element-index)))
                     (make-operation/replace-selection (input-of projection-iomap) (reverse (append `((the ,?type (elt (the list document) ,input-element-index))) ?rest)))))))
               (operation/sequence/replace-element-range)
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations)))))))
    (recurse operation)))
