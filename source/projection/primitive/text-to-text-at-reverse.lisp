;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text/text->text/text@reverse ()
  ())

;;;;;;
;;; Construction

(def function make-projection/text/text->text/text@reverse ()
  (make-projection 'text/text->text/text@reverse))

;;;;;;
;;; Construction

(def macro text/text->text/text@reverse ()
  '(make-projection/text/text->text/text@reverse))

;;;;;;
;;; Printer

(def printer text/text->text/text@reverse (projection recursion input input-reference)
  (bind ((output-selection (as (pattern-case (selection-of input)
                                 (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                  (bind ((length (text/length input)))
                                    `((the text/text (text/subseq (the text/text document) ,(- length ?end-index) ,(- length ?start-index)))))))))
         (output (text/make-text (as (reverse (iter (for element :in-sequence (elements-of input))
                                                    (etypecase element
                                                      (text/newline (collect element))
                                                      (text/string (collect (text/string (reverse (content-of element))
                                                                                         :font (font-of element)
                                                                                         :font-color (font-color-of element)
                                                                                         :fill-color (fill-color-of element)
                                                                                         :line-color (line-color-of element))))))))
                                 :selection output-selection)))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader text/text->text/text@reverse (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   (bind ((length (text/length printer-input)))
                                                     `((the text/text (text/subseq (the text/text document) ,(- length ?end-index) ,(- length ?start-index)))))))
                                           (make-operation/replace-selection (input-of printer-iomap) it)))
                                        (operation/sequence/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   (bind ((length (text/length printer-input)))
                                                     `((the text/text (text/subseq (the text/text document) ,(- length ?end-index) ,(- length ?start-index)))))))
                                           (make-operation/sequence/replace-range (input-of printer-iomap) it (replacement-of operation))))
                                        (operation/show-context-sensitive-help
                                         (make-instance 'operation/show-context-sensitive-help
                                                        :commands (iter (for command :in (commands-of operation))
                                                                        (awhen (recurse (operation-of command))
                                                                          (collect (make-instance 'command
                                                                                                  :gesture (gesture-of command)
                                                                                                  :domain (domain-of command)
                                                                                                  :description (description-of command)
                                                                                                  :operation it))))))
                                        (operation/compound
                                         (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null child-operations)
                                             (make-operation/compound child-operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))
