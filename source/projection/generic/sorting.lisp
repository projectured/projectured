;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sorting ()
  ((key :type function)
   (predicate :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/sorting (key predicate)
  (make-projection 'sorting :key key :predicate predicate))

;;;;;;
;;; Construction

(def (macro e) sorting (key predicate)
  `(make-projection/sorting ,key ,predicate))

;;;;;
;;; IO map

(def iomap iomap/sorting (iomap)
  ((element-iomaps :type sequence)
   (input-indices :type sequence)))

;;;;;;
;;; Printer

(def printer sorting (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence input)
                               (collect (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                             ,@(typed-reference (form-type input) input-reference))))))
         (key (key-of projection))
         (predicate (predicate-of projection))
         (indices (iter (for index :from 0 :below (length input))
                        (collect index)))
         (input-indices (stable-sort indices predicate :key (lambda (index) (funcall key (elt input index)))))
         (output-elements (iter (for input-index :in input-indices)
                                (collect (output-of (elt element-iomaps input-index)))))
         (output (etypecase input
                   (t ;; KLUDGE: typechecking fails in SBCL sequence/sequence
                       (bind ((output-selection (pattern-case (reverse (selection-of input))
                                                  (((the ?element-type (elt (the sequence document) ?element-index))
                                                    . ?rest)
                                                   (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(position ?element-index input-indices)))))))))
                         (make-sequence/sequence output-elements :selection output-selection)))
                   (sequence output-elements))))
    (make-iomap 'iomap/sorting
                :projection projection :recursion recursion
                :input input :output output
                :element-iomaps element-iomaps
                :input-indices input-indices)))

;;;;;;
;;; Reader

(def reader sorting (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/replace-selection
                                (awhen (when (typep printer-input 'sequence/sequence)
                                         (pattern-case (reverse (selection-of operation))
                                           (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                            (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(elt (input-indices-of printer-iomap) ?element-index))))))))
                                  (make-operation/replace-selection printer-input it)))
                               (operation/sequence/replace-element-range
                                (awhen (when (typep printer-input 'sequence/sequence)
                                         (pattern-case (reverse (target-of operation))
                                           (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                            (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(elt (input-indices-of printer-iomap) ?element-index))))))))
                                  (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (recurse (operation-of input))))))
