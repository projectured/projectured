;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

;; TODO: rename these to ->tree/leaf
(def projection lisp-form/comment->tree/node ()
  ())

(def projection lisp-form/number->tree/leaf ()
  ())

(def projection lisp-form/symbol->tree/leaf ()
  ())

(def projection lisp-form/string->tree/leaf ()
  ())

(def projection lisp-form/quote->tree/node ()
  ())

(def projection lisp-form/list->tree/node ()
  ())

(def projection lisp-form/object->tree/leaf ()
  ())

(def projection lisp-form/top-level->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/lisp-form/comment->tree/node ()
  (make-projection 'lisp-form/comment->tree/node))

(def (function e) make-projection/lisp-form/number->tree/leaf ()
  (make-projection 'lisp-form/number->tree/leaf))

(def (function e) make-projection/lisp-form/symbol->tree/leaf ()
  (make-projection 'lisp-form/symbol->tree/leaf))

(def (function e) make-projection/lisp-form/string->tree/leaf ()
  (make-projection 'lisp-form/string->tree/leaf))

(def (function e) make-projection/lisp-form/quote->tree/node ()
  (make-projection 'lisp-form/quote->tree/node))

(def (function e) make-projection/lisp-form/list->tree/node ()
  (make-projection 'lisp-form/list->tree/node))

(def (function e) make-projection/lisp-form/object->tree/leaf ()
  (make-projection 'lisp-form/object->tree/leaf))

(def (function e) make-projection/lisp-form/top-level->tree/node ()
  (make-projection 'lisp-form/top-level->tree/node))

;;;;;;
;;; Construction

(def (macro e) lisp-form/comment->tree/node ()
  '(make-projection/lisp-form/comment->tree/node))

(def (macro e) lisp-form/number->tree/leaf ()
  '(make-projection/lisp-form/number->tree/leaf))

(def (macro e) lisp-form/symbol->tree/leaf ()
  '(make-projection/lisp-form/symbol->tree/leaf))

(def (macro e) lisp-form/string->tree/leaf ()
  '(make-projection/lisp-form/string->tree/leaf))

(def (macro e) lisp-form/quote->tree/node ()
  '(make-projection/lisp-form/quote->tree/node))

(def (macro e) lisp-form/list->tree/node ()
  '(make-projection/lisp-form/list->tree/node))

(def (macro e) lisp-form/object->tree/node ()
  '(make-projection/lisp-form/object->tree/leaf))

(def (macro e) lisp-form/top-level->tree/node ()
  '(make-projection/lisp-form/top-level->tree/node))

;;;;;;
;;; Printer

(def printer lisp-form/comment->tree/node (projection recursion input input-reference)
  (bind ((content (content-of input))
         ;; TODO:
         (output (make-tree/node (list (output-of (recurse-printer recursion content input-reference)))
                                 #+nil(make-text/string content :font *font/ubuntu/regular/18* :font-color *color/solarized/gray*)
                                 :opening-delimiter (text/text () (text/string ";; " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/number->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (reverse (selection-of input))
                             (((the number (value-of (the lisp-form/number document)))
                               (the string (write-to-string (the number document)))
                               (the string (subseq (the string document) ?start-index ?end-index)))
                              `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
                                (the text/text (content-of (the tree/leaf document)))))
                             (((the tree/leaf (printer-output (the lisp-form/number document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection))
                     (text/string (write-to-string (value-of input)) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/symbol->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (reverse (selection-of input))
                             (((the string (name-of (the lisp-form/symbol document)))
                               (the string (subseq (the string document) ?start-index ?end-index)))
                              `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
                                (the text/text (content-of (the tree/leaf document)))))
                             (((the tree/leaf (printer-output (the lisp-form/symbol document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (font-color (or (font-color-of input) *color/solarized/violet*))
         (name (name-of input))
         (name-string (string-downcase (if (string= "KEYWORD" (package-of input))
                                           (string+ ":" name)
                                           name)))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection))
                     (text/string name-string :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color font-color))
                   ;; TODO: parameter
                   #+nil
                   (text/text (:selection (butlast output-selection))
                     (text/string (string+ (string-downcase (package-of input)) "::" ) :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color (color/lighten font-color 0.5))
                     (text/string name-string :font (or (font-of input) *font/ubuntu/monospace/bold/18*) :font-color font-color)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/string->tree/leaf (projection recursion input input-reference)
  (bind ((value (write-to-string (value-of input)))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the string (value-of (the lisp-form/string document)))
                               (the string (subseq (the string document) ?start-index ?end-index)))
                              `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
                                (the text/text (content-of (the tree/leaf document)))))
                             (((the tree/leaf (printer-output (the lisp-form/string document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (output (tree/leaf (:selection output-selection
                             :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                             :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                   (text/text (:selection (butlast output-selection)) (text/string (subseq value 1 (1- (length value))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/quote->tree/node (projection recursion input input-reference)
  (bind ((value-iomap (recurse-printer recursion (value-of input)
                                       `((value-of (the sequence document))
                                         ,@(typed-reference (form-type input) input-reference))))
         (output (tree/node (:opening-delimiter (text/text () (text/string "'" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                   (output-of value-iomap))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/list->tree/node (projection recursion input input-reference)
  (bind ((deep-list (find-if (of-type 'lisp-form/list) (elements-of input)))
         (element-iomaps (iter (for index :from 0)
                               (for element :in (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,index)
                                                                       (the sequence (elements-of (the document list-form/list)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               (for element-output = (output-of element-iomap))
                               (setf (indentation-of element-output)
                                     (typecase element
                                       (tree/base (indentation-of element))
                                       (lisp-form/base (indentation-of element))
                                       (t (when (and deep-list (not (first-iteration-p)))
                                            2))))
                               (collect element-iomap)))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the sequence (elements-of (the lisp-form/list document)))
                               (the ?element-type (elt (the sequence document) ?element-index))
                               . ?rest)
                              (bind ((element-iomap (elt element-iomaps ?element-index))
                                     (element-output (output-of element-iomap)))
                                (append (selection-of element-output)
                                        `((the ,(form-type element-output) (elt (the sequence document) ,?element-index))
                                          (the sequence (children-of (the tree/node document)))))))
                             (((the tree/node (printer-output (the lisp-form/list document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (separator-font-color (color/darken/selection *color/solarized/gray* output-selection))
         (separator-fill-color (color/lighten/selection (color/lighten *color/solarized/cyan* 0.75) output-selection nil))
         (output (make-tree/node (mapcar 'output-of element-iomaps)
                                 :selection output-selection
                                 :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color separator-font-color :fill-color separator-fill-color))
                                 :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color separator-font-color :fill-color separator-fill-color))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer lisp-form/object->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (reverse (selection-of input))
                             (((the tree/leaf (printer-output (the lisp-form/object document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection))
                     (text/string (write-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/top-level->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,index)
                                                                       (the sequence (elements-of (the lisp-form/top-level document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               (for element-output = (output-of element-iomap))
                               (setf (indentation-of element-output)
                                     (typecase element
                                       (lisp-form/base (indentation-of element))
                                       (t 0)))
                               (collect element-iomap)))
         (output (make-tree/node (mapcar 'output-of element-iomaps)
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :indentation 2)))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

;;;;;;
;;; Reader

(def reader lisp-form/comment->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/number->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (write-to-string (the number document)))
                                                     (the number (value-of (the lisp-form/number document)))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/leaf (printer-output (the lisp-form/number document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range
                                         (awhen (and (every 'digit-char-p (replacement-of operation))
                                                     (pattern-case (reverse (target-of operation))
                                                       (((the text/text (content-of (the tree/leaf document)))
                                                         (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                        `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                          (the string (write-to-string (the number document)))
                                                          (the number (value-of (the lisp-form/number document)))))))
                                           (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/symbol->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (name-of (the lisp-form/symbol document)))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/leaf (printer-output (the lisp-form/symbol document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range
                                         (awhen (pattern-case (reverse (target-of operation))
                                                  (((the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (name-of (the lisp-form/symbol document))))))
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

(def reader lisp-form/string->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (value-of (the lisp-form/string document)))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/leaf (printer-output (the lisp-form/string document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range
                                         (awhen (pattern-case (reverse (target-of operation))
                                                  (((the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (value-of (the lisp-form/string document))))))
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

(def reader lisp-form/quote->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/list->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) ?child-index))
                                                    . ?rest)
                                                   (bind ((child (elt (elements-of printer-input) ?child-index))
                                                          (input-operation (make-operation/replace-selection child (reverse ?rest)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) ?child-index)))))
                                                     (append (selection-of output-operation)
                                                             `((the ,(form-type child) (elt (the sequence document) ,?child-index))
                                                               (the sequence (elements-of (the lisp-form/list document)))))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/node (printer-output (the lisp-form/list document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range
                                         (pattern-case (reverse (target-of operation))
                                           (((the sequence (children-of (the tree/node document)))
                                             (the ?type (elt (the sequence document) ?child-index))
                                             . ?rest)
                                            (bind ((child (elt (elements-of printer-input) ?child-index))
                                                   (input-operation (make-operation/sequence/replace-element-range child (reverse ?rest) (replacement-of operation)))
                                                   (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) ?child-index)))))
                                              (typecase output-operation
                                                (operation/sequence/replace-element-range
                                                 (make-operation/sequence/replace-element-range printer-input
                                                                                                (append (target-of output-operation)
                                                                                                        `((the ,(form-type child) (elt (the sequence document) ,?child-index))
                                                                                                          (the sequence (elements-of (the lisp-form/list document)))))
                                                                                                (replacement-of operation)))
                                                (operation/number/replace-range
                                                 (make-operation/number/replace-range printer-input
                                                                                      (append (target-of output-operation)
                                                                                              `((the ,(form-type child) (elt (the sequence document) ,?child-index))
                                                                                                (the sequence (elements-of (the lisp-form/list document)))))
                                                                                      (replacement-of operation))))))))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/object->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/leaf (printer-output (the lisp-form/object document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range
                                         (awhen (pattern-case (reverse (target-of operation))
                                                  )
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

(def reader lisp-form/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
