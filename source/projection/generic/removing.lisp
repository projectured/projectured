;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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

(def function make-projection/removing (element predicate key)
  (make-projection 'removing
                   :element element
                   :predicate predicate
                   :key key))

;;;;;;
;;; Construction

(def macro removing (element predicate key)
  `(make-projection/removing ,element ,predicate ,key))

;;;;;
;;; IO map

(def iomap iomap/removing ()
  ((element-iomaps :type sequence)
   (input-indices :type sequence)))

;;;;;;
;;; Printer

(def printer removing ()
  (bind ((input-indices nil)
         (element-iomaps (iter (for index :from 0)
                               (for element :in-sequence -input-)
                               (collect (recurse-printer -recursion- element `((elt (the sequence document) ,index)
                                                                               ,@(typed-reference (document-type -input-) -input-reference-))))))
         (key (key-of -projection-))
         (predicate (predicate-of -projection-))
         (output-elements (iter (for input-index :from 0)
                                (for input-element :in-sequence -input-)
                                (for output-element = (output-of (elt element-iomaps input-index)))
                                (unless (funcall predicate (element-of -projection-) (funcall key input-element))
                                  (collect output-element)
                                  (push input-index input-indices))
                                (finally (nreversef input-indices))))
         (output (etypecase -input-
                   (collection/sequence
                       (bind ((output-selection (reference-case (selection-of -input-)
                                                  (((the ?element-type (elt (the sequence document) ?element-index))
                                                    . ?rest)
                                                   (append `((the ,?element-type (elt (the sequence document) ,(position ?element-index input-indices)))) ?rest)))))
                         (make-collection/sequence output-elements :selection output-selection)))
                   (sequence output-elements))))
    (make-instance 'iomap/removing
                   :projection -projection- :recursion -recursion-
                   :input -input- :output output
                   :element-iomaps element-iomaps
                   :input-indices input-indices)))

;;;;;;
;;; Reader

(def reader removing ()
  (make-command -gesture-
                (labels ((recurse (operation)
                           (typecase operation
                             (operation/quit operation)
                             (operation/functional operation)
                             (operation/replace-selection
                              (awhen (when (typep -printer-input- 'collection/sequence)
                                       (reference-case (selection-of operation)
                                         (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                          (append `((the ,?element-type (elt (the sequence document) ,(elt (input-indices-of -printer-iomap-) ?element-index)))) ?rest))))
                                (make-operation/replace-selection it)))
                             (operation/sequence/replace-range
                              (awhen (when (typep -printer-input- 'collection/sequence)
                                       (reference-case (selection-of operation)
                                         (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                          (append `((the ,?element-type (elt (the sequence document) ,(elt (input-indices-of -printer-iomap-) ?element-index)))) ?rest))))
                                (make-operation/sequence/replace-range it (replacement-of operation))))
                             (operation/compound
                              (bind ((operations (mapcar #'recurse (elements-of operation))))
                                (unless (some 'null operations)
                                  (make-operation/compound operations)))))))
                  (recurse (operation-of -input-)))
                :domain (domain-of -input-)
                :description (description-of -input-)))
