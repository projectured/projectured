;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/document->t ()
  ())

(def projection document/clipboard->t ()
  ())

(def projection document/number->number ()
  ())

(def projection document/number->string ()
  ())

(def projection document/string->string ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/document->t ()
  (make-projection 'document/document->t))

(def function make-projection/document/clipboard->t ()
  (make-projection 'document/clipboard->t))

(def function make-projection/document/number->number ()
  (make-projection 'document/number->number))

(def function make-projection/document/number->string ()
  (make-projection 'document/number->string))

(def function make-projection/document/string->string ()
  (make-projection 'document/string->string))

;;;;;;
;;; Construction

(def macro document/document->t ()
  '(make-projection/document/document->t))

(def macro document/clipboard->t ()
  `(make-projection/document/clipboard->t))

(def macro document/number->number ()
  `(make-projection/document/number->number))

(def macro document/number->string ()
  `(make-projection/document/number->string))

(def macro document/string->string ()
  `(make-projection/document/string->string))

;;;;;;
;;; Forward mapper

(def forward-mapper document/number->number ()
  (pattern-case -reference-
    (((the document/number document))
     '((the number document)))
    (((the number (value-of (the document/number document)))
      (the string (write-to-string (the number document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the string (write-to-string (the number document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    (((the number (printer-output (the document/number document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper document/number->string ()
  (pattern-case -reference-
    (((the document/number document))
     '((the string document)))
    (((the number (value-of (the document/number document)))
      (the string (write-to-string (the number document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the string (printer-output (the document/number document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper document/number->number ()
  (pattern-case -reference-
    (((the number document))
     '((the document/number document)))
    (((the string (write-to-string (the number document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the number (value-of (the document/number document)))
       (the string (write-to-string (the number document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    (?a
     (append `((the number (printer-output (the document/number document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper document/number->string ()
  (pattern-case -reference-
    (((the string document))
     '((the document/number document)))
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the number (value-of (the document/number document)))
       (the string (write-to-string (the number document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    (?a
     (append `((the string (printer-output (the document/number document) ,-projection- ,-recursion-))) -reference-))))

;;;;;;
;;; Printer

(def printer document/document->t ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) `((content-of (the document/document document))
                                                                                ,@(typed-reference (document-type -input-) -input-reference-))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- (make-graphics/canvas (as (list (output-of (va content-iomap)))) 0) (as (list (va content-iomap))))))

(def printer document/clipboard->t ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) `((content-of (the document/clipboard document))
                                                                                ,@(typed-reference (document-type -input-) -input-reference-))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- (make-graphics/canvas (as (list (output-of (va content-iomap)))) 0) (as (list (va content-iomap))))))

(def printer document/number->number ()
  (bind ((output (as (value-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer document/number->string ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (selection-of -input-)
                                                'forward-mapper/document/number->string)))
         (output (as ;; KLUDGE: for demo
                   (text/make-default-text (aif (value-of -input-) (write-to-string it) "") "enter json number" :selection output-selection :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*)
                     #+nil
                     (text/text (:selection output-selection)
                       (text/string (write-to-string (value-of input)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer document/string->string ()
  (bind ((output (as (value-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader document/document->t ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-s :control)
                     :domain "Document" :description "Saves the currently edited document."
                     :operation (make-operation/save-document -printer-input- (filename-of -printer-input-)))
                    ((make-key-press-gesture :scancode-l :control)
                     :domain "Document" :description "Loads the previously saved document."
                     :operation (make-operation/load-document -printer-input- (filename-of -printer-input-)))
                    #+nil
                    ((make-key-press-gesture :scancode-e :control)
                     :domain "Document" :description "Exports the currently edited document as text."
                     :operation (make-operation/export-document (content-of printer-input) (filename-of printer-input))))
                  (bind ((content-iomap (elt (child-iomaps-of -printer-iomap-) 0))
                         (content-command (recurse-reader -recursion- -input- content-iomap)))
                    (make-command -gesture-
                                  (operation/extend -printer-input- `((the ,(document-type (content-of -printer-input-)) (content-of (the document/document document)))) (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (make-nothing-command -gesture-)))

(def reader document/clipboard->t ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-c :control)
                     :domain "Document" :description "Copies the selected object to the clipboard"
                     :operation (bind ((slice (deep-copy (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-))))))
                                  (make-operation/replace-target -printer-input- `((the ,(document-type slice) (slice-of (the document/clipboard document)))) slice)))
                    ((make-key-press-gesture :scancode-x :control)
                     :domain "Document" :description "Cuts the selected object and moves it to the clipboard"
                     :operation (bind ((slice (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-)))))
                                  (make-operation/compound (list (make-operation/replace-target -printer-input- `((the ,(document-type slice) (slice-of (the document/clipboard document)))) slice)
                                                                 (make-operation/replace-target -printer-input- (selection-of -printer-input-) (document/nothing ()))))))
                    ((make-key-press-gesture :scancode-n :control)
                     :domain "Document" :description "Notes the selected object in the clipboard"
                     :operation (bind ((slice (eval-reference -printer-input- (flatten-reference (selection-of -printer-input-)))))
                                  (make-operation/replace-target -printer-input- `((the ,(document-type slice) (slice-of (the document/clipboard document)))) slice)))
                    ((make-key-press-gesture :scancode-v :control)
                     :domain "Document" :description "Pastes the object from the clipboard to the selection"
                     :operation (if (slice-of -printer-input-)
                                    (make-operation/replace-target -printer-input- (selection-of -printer-input-) (slice-of -printer-input-))
                                    #+nil
                                    (make-operation/sequence/replace-range printer-input (selection-of printer-input)
                                                                           (with-output-to-string (stream)
                                                                             (uiop:run-program "/usr/bin/xclip" (list "-o") :output stream)))))
                    ((make-key-press-gesture :scancode-v '(:shift :control))
                     :domain "Document" :description "Pastes a new copy of the object from the clipboard to the selection"
                     :operation (when (slice-of -printer-input-)
                                  (make-operation/replace-target -printer-input- (selection-of -printer-input-) (deep-copy (slice-of -printer-input-))))))
                  (bind ((content-iomap (elt (child-iomaps-of -printer-iomap-) 0))
                         (content-command (recurse-reader -recursion- -input- content-iomap)))
                    (make-command -gesture-
                                  (operation/extend -printer-input- `((the ,(document-type (content-of -printer-input-)) (content-of (the document/clipboard document)))) (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (make-nothing-command -gesture-)))

(def reader document/number->number ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the number (value-of (the document/number document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input- selection (replacement-of operation))))
                                  (((the number (printer-output (the document/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input-
                                                                          '((the number (value-of (the document/number document)))
                                                                            (the string (write-to-string (the number document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation))))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/document/number->number operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader document/number->string ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the number (value-of (the document/number document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input- selection (replacement-of operation))))
                                  (((the number (printer-output (the document/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input-
                                                                          '((the number (value-of (the document/number document)))
                                                                            (the string (write-to-string (the number document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation))))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/document/number->string operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader document/string->string ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/document/string->string nil)
                  (make-nothing-command -gesture-)))
