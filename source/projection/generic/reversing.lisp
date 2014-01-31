;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection reversing ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/reversing ()
  (make-projection 'reversing))

;;;;;;
;;; Construction

(def (macro e) reversing ()
  `(make-projection/reversing))

;;;;;;
;;; Printer

(def printer reversing (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence input)
                               (collect (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                             ,@(typed-reference (form-type input) input-reference))))))
         (output (etypecase input
                   (sequence/sequence
                       (bind ((output-selection (pattern-case (reverse (selection-of input))
                                                  (((the ?element-type (elt (the sequence document) ?element-index))
                                                    . ?rest)
                                                   (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(- (length (elements-of input)) ?element-index 1)))))))))
                         (make-sequence/sequence (reverse (mapcar 'output-of element-iomaps)) :selection output-selection)))
                   (sequence (reverse (mapcar 'output-of element-iomaps))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader reversing (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (awhen (labels ((recurse (operation)
                      (typecase operation
                        (operation/quit operation)
                        (operation/replace-selection
                         (awhen (when (typep printer-input 'sequence/sequence)
                                  (pattern-case (reverse (selection-of operation))
                                    (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                     (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(- (length (elements-of printer-input)) ?element-index 1))))))))
                           (make-operation/replace-selection printer-input it)))
                        (operation/sequence/replace-element-range
                         (awhen (when (typep printer-input 'sequence/sequence)
                                  (pattern-case (reverse (target-of operation))
                                    (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                                     (append (reverse ?rest) `((the ,?element-type (elt (the sequence document) ,(- (length (elements-of printer-input)) ?element-index 1))))))))
                           (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                        (operation/compound
                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                           (unless (some 'null operations)
                             (make-operation/compound operations)))))))
             (recurse (operation-of input)))
      (make-command (gesture-of input) it))))
