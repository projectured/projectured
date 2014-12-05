;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/document->t ()
  ())

(def projection document/clipboard->t ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/document->t ()
  (make-projection 'document/document->t))

(def function make-projection/document/clipboard->t ()
  (make-projection 'document/clipboard->t))

;;;;;;
;;; Construction

(def macro document/document->t ()
  '(make-projection/document/document->t))

(def macro document/clipboard->t ()
  `(make-projection/document/clipboard->t))

;;;;;;
;;; Printer

(def printer document/document->t (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) `((content-of (the document/document document))
                                                                            ,@(typed-reference (form-type input) input-reference))))))
    (make-iomap/compound projection recursion input input-reference (make-graphics/canvas (as (list (output-of (va content-iomap)))) (make-2d 0 0)) (as (list (va content-iomap))))))

(def printer document/clipboard->t (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) `((content-of (the document/clipboard document))
                                                                            ,@(typed-reference (form-type input) input-reference))))))
    (make-iomap/compound projection recursion input input-reference (make-graphics/canvas (as (list (output-of (va content-iomap)))) (make-2d 0 0)) (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader document/document->t (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-s :control)
                       :domain "Document" :description "Saves the currently edited document."
                       :operation (make-operation/save-document (content-of printer-input) (filename-of printer-input)))
                      ((gesture/keyboard/key-press :sdl-key-l :control)
                       :domain "Document" :description "Loads the previously saved document."
                       :operation (make-operation/load-document (content-of printer-input) (filename-of printer-input)))
                      #+nil
                      ((gesture/keyboard/key-press :sdl-key-e :control)
                       :domain "Document" :description "Exports the currently edited document as text."
                       :operation (make-operation/export-document (content-of printer-input) (filename-of printer-input))))
                    (bind ((content-iomap (elt (child-iomaps-of printer-iomap) 0))
                           (content-command (recurse-reader recursion input content-iomap)))
                      (make-command (gesture-of input)
                                    (operation/extend printer-input `((the ,(form-type (content-of printer-input)) (content-of (the document/document document)))) (operation-of content-command))
                                    :domain (domain-of content-command)
                                    :description (description-of content-command)))
                    (make-command/nothing (gesture-of input)))))

(def reader document/clipboard->t (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-c :control)
                       :domain "Document" :description "Copies the selected object to the clipboard"
                       :operation (bind ((slice (deep-copy (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input)))))))
                                    (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)))
                      ((gesture/keyboard/key-press :sdl-key-x :control)
                       :domain "Document" :description "Cuts the selected object and moves it to the clipboard"
                       :operation (bind ((slice (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input))))))
                                    (make-operation/compound (list (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)
                                                                   (make-operation/replace-target printer-input (selection-of printer-input) (document/nothing))))))
                      ((gesture/keyboard/key-press :sdl-key-n :control)
                       :domain "Document" :description "Notes selected object into the clipboard"
                       :operation (bind ((slice (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input))))))
                                    (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)))
                      ((gesture/keyboard/key-press :sdl-key-v :control)
                       :domain "Document" :description "Pastes the object from the clipboard to the selection"
                       :operation (if (slice-of printer-input)
                                      (make-operation/replace-target printer-input (selection-of printer-input) (slice-of printer-input))
                                      #+nil
                                      (make-operation/sequence/replace-element-range printer-input (selection-of printer-input)
                                                                                     (with-output-to-string (stream)
                                                                                       (sb-ext:run-program "/usr/bin/xclip" (list "-o") :output stream)))))
                      ((gesture/keyboard/key-press :sdl-key-v '(:shift :control))
                       :domain "Document" :description "Pastes a new copy of the object from the clipboard to the selection"
                       :operation (when (slice-of printer-input)
                                    (make-operation/replace-target printer-input (selection-of printer-input) (deep-copy (slice-of printer-input))))))
                    (bind ((content-iomap (elt (child-iomaps-of printer-iomap) 0))
                           (content-command (recurse-reader recursion input content-iomap)))
                      (make-command (gesture-of input)
                                    (operation/extend printer-input `((the ,(form-type (content-of printer-input)) (content-of (the document/clipboard document)))) (operation-of content-command))
                                    :domain (domain-of content-command)
                                    :description (description-of content-command)))
                    (make-command/nothing (gesture-of input)))))
