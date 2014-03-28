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
                             (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))))))
         (output (tree/leaf (:selection output-selection)
                   input)))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader text/text->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (make-operation/replace-selection printer-input
                                                                           (pattern-case (selection-of operation)
                                                                             (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                                                               (the text/text (content-of (the tree/leaf document))))
                                                                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index)))))))
                                        (operation/sequence/replace-element-range
                                         (awhen (pattern-case (target-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                    (the text/text (content-of (the tree/leaf document))))
                                                   `((the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index)))))
                                           (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))
