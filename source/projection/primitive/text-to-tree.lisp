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

(def function make-projection/text/text->tree/leaf ()
  (make-projection 'text/text->tree/leaf))

;;;;;;
;;; Construction

(def macro text/text->tree/leaf ()
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
    (merge-commands (make-command/nothing (gesture-of input)))))
