;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text/text->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text/text->tree/leaf ()
  (make-projection 'text/text->tree/leaf))

;;;;;;
;;; Construction

(def (macro e) text/text->tree/leaf ()
  '(make-projection/text/text->tree/leaf))

;;;;;;
;;; Printer

(def printer text/text->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (selection-of input)
                             (((the sequence-position (text/pos (the text/text document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))))))
         (output (tree/leaf (:selection output-selection)
                   input)))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader text/text->tree/leaf (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (selection-of operation)
                                                      (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                        (the text/text (content-of (the tree/leaf document))))
                                                       `((the sequence-position (text/pos (the text/text document) ,?character-index)))))))
                 (operation/sequence/replace-element-range
                  (awhen (pattern-case (target-of operation)
                           (((the sequence-position (text/pos (the text/text document) ?character-index))
                             (the text/text (content-of (the tree/leaf document))))
                            `((the sequence-position (text/pos (the text/text document) ,?character-index)))))
                    (make-operation/sequence/replace-element-range input it (replacement-of operation))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))
