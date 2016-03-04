;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection workbench/document->t ()
  ())

;;;;;;
;;; Construction

(def function make-projection/workbench/document->t ()
  (make-projection 'workbench/document->t))

;;;;;;
;;; Construction

(def macro workbench/document->t ()
  '(make-projection/workbench/document->t))

;;;;;;
;;; Printer

(def printer workbench/document->t ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) `((content-of (the workbench/document document))
                                                                                ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (make-graphics/canvas (as (list (output-of (va content-iomap))))
                                       (make-2d 0 0))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader workbench/document->t ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-s :control)
                     :domain "Workbench" :description "Saves the currently edited document."
                     :operation (make-operation/save-document -printer-input- (filename-of -printer-input-)))
                    ((make-key-press-gesture :scancode-l :control)
                     :domain "Workbench" :description "Loads the previously saved document."
                     :operation (make-operation/load-document -printer-input- (filename-of -printer-input-)))
                    #+nil
                    ((make-key-press-gesture :scancode-e :control)
                     :domain "Document" :description "Exports the currently edited document as text."
                     :operation (make-operation/export-document (content-of printer-input) (filename-of printer-input))))
                  (bind ((content-iomap (elt (child-iomaps-of -printer-iomap-) 0))
                         (content-command (recurse-reader -recursion- -input- content-iomap)))
                    (make-command -gesture-
                                  (operation/extend -printer-input- `((the ,(document-type (content-of -printer-input-)) (content-of (the workbench/document document)))) (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (make-nothing-command -gesture-)))
