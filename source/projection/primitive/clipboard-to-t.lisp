;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection clipboard/slice->t ()
  ((display-slice :type boolean)))

(def projection clipboard/collection->t ()
  ((display-collection :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/clipboard/slice->t (&key display-slice)
  (make-projection 'clipboard/slice->t :display-slice display-slice))

(def function make-projection/clipboard/collection->t (&key display-collection)
  (make-projection 'clipboard/collection->t :display-collection display-collection))

;;;;;;
;;; Construction

(def macro clipboard/slice->t (&key display-slice)
  `(make-projection/clipboard/slice->t :display-slice ,display-slice))

(def macro clipboard/collection->t (&key display-collection)
  `(make-projection/clipboard/collection->t :display-collection ,display-collection))

;;;;;;
;;; Printer

(def printer clipboard/slice->t ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the clipboard/slice document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (slice-iomap (as (recurse-printer -recursion- (slice-of -input-)
                                           `((slice-of (the clipboard/slice document))
                                             ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (make-graphics/canvas (as (list (output-of (if (display-slice-p -projection-)
                                                                (va slice-iomap)
                                                                (va content-iomap)))))
                                       (make-2d 0 0))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

(def printer clipboard/collection->t ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) `((content-of (the clipboard/collection document))
                                                                                ,@(typed-reference (document-type -input-) -input-reference-)))))
         (element-iomaps (as (map-ll* (ll (elements-of -input-))
                                      (lambda (element index)
                                        (recurse-printer -recursion- (value-of element) `((elt (the sequence document) ,index)
                                                                                          (the sequence (elements-of (the clipboard/collection document)))
                                                                                          ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output (make-graphics/canvas (as (if (display-collection-p -projection-)
                                               (map-ll (va element-iomaps) 'output-of)
                                               (list (output-of (va content-iomap)))))
                                       (make-2d 0 0))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader clipboard/slice->t ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-kp-divide :control)
                     :domain "Clipboard" :description "Toggles between displaying the content or the clipboard"
                     :operation (make-operation/functional (lambda () (notf (display-slice-p -projection-)))))
                    ((make-key-press-gesture :scancode-c :control)
                     :domain "Clipboard" :description "Copies the selected object to the clipboard"
                     :operation (bind ((slice (deep-copy (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-))))))
                                  (make-operation/replace-target `((the ,(document-type slice) (slice-of (the clipboard/slice document)))) slice)))
                    ((make-key-press-gesture :scancode-x :control)
                     :domain "Clipboard" :description "Cuts the selected object and moves it to the clipboard"
                     :operation (bind ((slice (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-)))))
                                  (make-operation/compound (list (make-operation/replace-target `((the ,(document-type slice) (slice-of (the clipboard/slice document)))) slice)
                                                                 (make-operation/replace-target (selection-of -printer-input-) (document/nothing ()))))))
                    ((make-key-press-gesture :scancode-n :control)
                     :domain "Clipboard" :description "Notes the selected object in the clipboard"
                     :operation (bind ((slice (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-)))))
                                  (make-operation/replace-target `((the ,(document-type slice) (slice-of (the clipboard/slice document)))) slice)))
                    ((make-key-press-gesture :scancode-v :control)
                     :domain "Clipboard" :description "Pastes the object from the clipboard to the selection"
                     :operation (if (slice-of -printer-input-)
                                    (make-operation/replace-target (selection-of -printer-input-) (slice-of -printer-input-))
                                    #+nil
                                    (make-operation/sequence/replace-range (selection-of printer-input)
                                                                           (with-output-to-string (stream)
                                                                             (uiop:run-program "/usr/bin/xclip" (list "-o") :output stream)))))
                    ((make-key-press-gesture :scancode-v '(:shift :control))
                     :domain "Clipboard" :description "Pastes a new copy of the object from the clipboard to the selection"
                     :operation (when (slice-of -printer-input-)
                                  (make-operation/replace-target (selection-of -printer-input-) (deep-copy (slice-of -printer-input-))))))
                  (bind ((content-iomap (elt (child-iomaps-of -printer-iomap-) 0))
                         (content-command (recurse-reader -recursion- -input- content-iomap)))
                    (make-command -gesture-
                                  (operation/extend -printer-input- `((the ,(document-type (content-of -printer-input-)) (content-of (the clipboard/slice document)))) (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (make-nothing-command -gesture-)))

(def reader clipboard/collection->t ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-kp-multiply :control)
                     :domain "Clipboard" :description "Toggles between displaying the content or the collection"
                     :operation (make-operation/functional (lambda () (notf (display-collection-p -projection-)))))
                    ((make-key-press-gesture :scancode-kp-plus :control)
                     :domain "Clipboard" :description "Adds the currently selected object to the collection"
                     :operation (make-operation/functional (lambda () (push (eval-reference -printer-input- (flatten-reference (get-selection -printer-input-))) (elements-of -printer-input-)))))
                    ((make-key-press-gesture :scancode-kp-minus :control)
                     :domain "Clipboard" :description "Removes the currently selected element from the collection"
                     :operation (make-operation/functional (lambda () (removef (elements-of -printer-input-) (eval-reference -printer-input- (flatten-reference (get-selection -printer-input-))))))))
                  (bind ((content-iomap (elt (child-iomaps-of -printer-iomap-) 0))
                         (content-command (recurse-reader -recursion- -input- content-iomap)))
                    (make-command -gesture-
                                  (operation/extend -printer-input- `((the ,(document-type (content-of -printer-input-)) (content-of (the clipboard/collection document)))) (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (make-nothing-command -gesture-)))
