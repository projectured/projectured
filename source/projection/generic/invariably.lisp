;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection invariably ()
  ((output :type t)))

;;;;;;
;;; Construction

(def function make-projection/invariably (output)
  (make-projection 'invariably :output output))

;;;;;;
;;; Construction

(def macro invariably (&body output)
  `(make-projection/invariably ,(first output)))

;;;;;;
;;; Printer

(def printer invariably ()
  (bind ((output (output-of -projection-)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader invariably ()
  (merge-commands (awhen (labels ((recurse (operation)
                                    (typecase operation
                                      (operation/quit operation)
                                      (operation/functional operation)
                                      (operation/replace-selection
                                       (make-operation/replace-selection -printer-input-
                                                                         (append (selection-of operation)
                                                                                 `((the ,(document-type -printer-output-) (printer-output (the ,(document-type -printer-input-) document) ,-projection- ,-recursion-))))))
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
                           (recurse (operation-of -input-)))
                    (make-command -gesture- it
                                  :domain (domain-of -input-)
                                  :description (description-of -input-)))
                  (make-nothing-command -gesture-)))
