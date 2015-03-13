;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
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

(def printer invariably (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (selection-of input)
                             (((the ?output-type (printer-output (the ?input-type document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                ?rest))))
         (output (output-of projection)))
    (set-selection output output-selection)
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader invariably (projection recursion input printer-iomap)
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap))
         (printer-output (output-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (make-operation/replace-selection printer-input
                                                                           (append (selection-of operation)
                                                                                   `((the ,(form-type printer-output) (printer-output (the ,(form-type printer-input) document) ,projection ,recursion))))))
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
                      (make-command gesture it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))
