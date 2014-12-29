;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/tree/leaf->text/text ()
  ((content-iomap :type iomap)))

(def iomap iomap/tree/node->text/text ()
  ((child-iomaps :type sequence)))

(def iomap iomap/tree/node->text/text/child ()
  ((content-iomap :type iomap)
   (indented-child :type text/text)
   (first-line-indentation :type integer)
   (other-lines-indentation :type integer)
   (first-character-index :type integer)
   (last-character-index :type integer)))

;;;;;;
;;; Forward mapper

(def function find-parent-character-index (child-iomap child-origin-relative-character-index)
  (bind ((child-output (output-of (content-iomap-of child-iomap)))
         (child-character-index (+ child-origin-relative-character-index (text/length child-output (text/first-position child-output) (text/origin-position child-output))))
         (first-character-index (first-character-index-of child-iomap))
         (first-line-indentation (first-line-indentation-of child-iomap))
         (other-lines-indentation (other-lines-indentation-of child-iomap))
         (output-string (text/as-string child-output))
         (child-line-index (count #\NewLine output-string :end child-character-index)))
    (+ child-character-index
       first-character-index
       (if first-line-indentation (1+ first-line-indentation) 0)
       (* child-line-index other-lines-indentation))))

;;;;;;
;;; Backward mapper

(def function find-child-character-index (iomap parent-character-index)
  (iter (for child-index :from 0)
        (for child-iomap :in-sequence (child-iomaps-of iomap))
        (for first-character-index = (first-character-index-of child-iomap))
        (for last-character-index = (last-character-index-of child-iomap))
        (when (<= first-character-index parent-character-index last-character-index)
          (bind ((first-line-indentation (first-line-indentation-of child-iomap))
                 (other-lines-indentation (other-lines-indentation-of child-iomap))
                 (indented-output-string (text/as-string (indented-child-of child-iomap)))
                 (indented-child-character-index (- parent-character-index first-character-index))
                 (child-line-index (funcall 'count #\NewLine indented-output-string :end indented-child-character-index))
                 (child-line-character-index (- indented-child-character-index
                                                (or (funcall 'position #\NewLine indented-output-string
                                                             :end indented-child-character-index
                                                             :from-end #t)
                                                    -1)
                                                1))
                 (child-line-indentation (if first-line-indentation
                                             (if (= child-line-index 0)
                                                 0
                                                 (if (= child-line-index 1)
                                                     first-line-indentation
                                                     other-lines-indentation))
                                             (if (= child-line-index 0)
                                                 0
                                                 other-lines-indentation)))
                 (child-character-index (- parent-character-index
                                           first-character-index
                                           (if first-line-indentation
                                               (1+ first-line-indentation)
                                               0)
                                           (if (> child-line-index 0)
                                               (* (if first-line-indentation
                                                      (1- child-line-index)
                                                      child-line-index)
                                                  other-lines-indentation)
                                               0)))
                 (child-output (output-of (content-iomap-of child-iomap)))
                 (child-origin-relative-character-index (- child-character-index (text/length child-output (text/first-position child-output) (text/origin-position child-output)))))
            (when (and (<= child-line-indentation child-line-character-index)
                       (<= 0 child-character-index))
              (assert (= parent-character-index (find-parent-character-index child-iomap child-origin-relative-character-index)))
              (return (values child-index child-origin-relative-character-index)))))))
;;;;;;
;;; Projection

(def projection tree/leaf->text/text ()
  ((output-delimiters :type boolean)))

(def projection tree/node->text/text ()
  ((output-delimiters :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/tree/leaf->text/text ()
  (make-projection 'tree/leaf->text/text :output-delimiters #t))

(def function make-projection/tree/node->text/text ()
  (make-projection 'tree/node->text/text :output-delimiters #t))

;;;;;;
;;; Construction

(def macro tree/leaf->text/text ()
  `(make-projection/tree/leaf->text/text))

(def macro tree/node->text/text ()
  `(make-projection/tree/node->text/text))

;;;;;;
;;; Printer

(def printer tree/leaf->text/text (projection recursion input input-reference)
  (bind ((content-reference `(((content-of (the ,(form-type input) document))) ,@(typed-reference (form-type input) input-reference)))
         (content-iomap (as (recurse-printer recursion (content-of input) content-reference)))
         (output-selection (as (pattern-case (reverse (selection-of input))
                                 (((the ?content-type (content-of (the tree/leaf document))) . ?rest)
                                  (selection-of (output-of (va content-iomap))))
                                 (((the text/text (opening-delimiter-of (the tree/leaf document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((content-output (output-of (va content-iomap)))
                                         (opening-delimiter (opening-delimiter-of input))
                                         (character-index (- (+ (text/length opening-delimiter
                                                                             (text/relative-position opening-delimiter (text/origin-position opening-delimiter) ?character-index)
                                                                             (text/last-position opening-delimiter))
                                                                (text/length content-output (text/first-position content-output) (text/origin-position content-output))))))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the text/text (closing-delimiter-of (the tree/leaf document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((content-output (output-of (va content-iomap)))
                                         (closing-delimiter (closing-delimiter-of input))
                                         (character-index (+ (text/length closing-delimiter
                                                                          (text/first-position closing-delimiter)
                                                                          (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?character-index))
                                                             (text/length content-output (text/origin-position content-output) (text/last-position content-output)))))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p projection))
                                     (opening-delimiter (opening-delimiter-of input))
                                     (closing-delimiter (closing-delimiter-of input)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (elements-of (output-of (va content-iomap)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap 'iomap/tree/leaf->text/text :input input :output output :content-iomap content-iomap)))

(def printer tree/node->text/text (projection recursion input input-reference)
  (bind ((child-iomaps (as (map-ll* (ll (children-of input))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt-ll (the sequence document) ,index)
                                                                (the sequence (children-of (the tree/node document)))
                                                                ,@(typed-reference (form-type input) input-reference))))
                                        (recurse-printer recursion (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child (input-of child-iomap))
                                                  (child-length (text/length (output-of child-iomap)))
                                                  (separator-length (aif (separator-of input) (text/length it) 0))
                                                  (last-line-length (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                          (while current-element)
                                                                          (for text = (output-of (value-of current-element)))
                                                                          (for string = (text/as-string text))
                                                                          ;; TODO: text API
                                                                          (for position = (position #\NewLine string :from-end #t))
                                                                          (if position
                                                                              (return (- (text/length text) position))
                                                                              (summing (text/length text) :into result))
                                                                          (summing separator-length :into result)
                                                                          (awhen (indentation-of (input-of (value-of current-element)))
                                                                            (return (+ result it)))
                                                                          (finally (return result))))
                                                  (first-line-indentation (indentation-of child))
                                                  (other-lines-indentation (or first-line-indentation last-line-length))
                                                  (indentation-length (bind ((indentation (indentation-of (input-of (value-of element))))
                                                                             (line-count (text/count-lines (output-of (value-of element)))))
                                                                        (if indentation
                                                                            (* line-count (1+ indentation))
                                                                            (* (1- line-count) other-lines-indentation)))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of (output-of child-iomap)))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element)))
                                                                                                                                      (if (and first-line-indentation
                                                                                                                                               (not (previous-element-of child-element-element)))
                                                                                                                                          (ll (list (text/newline)
                                                                                                                                                    (text/string (make-string-of-spaces first-line-indentation))
                                                                                                                                                    child-element))
                                                                                                                                          (if (and (not (zerop other-lines-indentation))
                                                                                                                                                   (text/newline? child-element)
                                                                                                                                                   (previous-element-of child-element-element))
                                                                                                                                              (ll (list child-element (text/string (make-string-of-spaces other-lines-indentation))))
                                                                                                                                              (ll (list child-element))))))))))))
                                                                     (make-iomap 'iomap/tree/node->text/text/child
                                                                                 :content-iomap child-iomap
                                                                                 :indented-child indented-child
                                                                                 :first-line-indentation first-line-indentation
                                                                                 :other-lines-indentation other-lines-indentation
                                                                                 :first-character-index (or child-first-character-index
                                                                                                            (- child-last-character-index child-length indentation-length))
                                                                                 :last-character-index (or child-last-character-index
                                                                                                           (+ child-first-character-index child-length indentation-length)))))
                                                               (as (or previous-element
                                                                       (awhen (previous-element-of element)
                                                                         (recurse it nil -self-
                                                                                  nil
                                                                                  (- (or child-first-character-index
                                                                                         (- child-last-character-index child-length indentation-length))
                                                                                     separator-length)))))
                                                               (as (or next-element
                                                                       (awhen (next-element-of element)
                                                                         (recurse it -self- nil
                                                                                  (+ (or child-last-character-index
                                                                                         (+ child-first-character-index child-length indentation-length))
                                                                                     separator-length)
                                                                                  nil))))))))
                                  (awhen (va child-iomaps)
                                    (recurse it nil nil 0 nil)))))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p projection))
                                     (opening-delimiter (opening-delimiter-of input))
                                     (closing-delimiter (closing-delimiter-of input)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (append-ll (bind ((indented-children (map-ll (va node-child-iomaps)
                                                                                             (lambda (node-child-iomap)
                                                                                               (elements-of (indented-child-of node-child-iomap))))))
                                                             (aif (separator-of input)
                                                                  (separate-elements-ll indented-children (ll (elements-of it)))
                                                                  indented-children)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output-selection (as (pattern-case (reverse (selection-of input))
                                 (((the tree/node document))
                                  `((the text/text (text/subbox (the text/text document) 0 ,(text/length (text/make-text output-elements))))))
                                 (((the text/text (printer-output (the tree/node document) ?projection ?recursion)) . ?rest)
                                  (when (and (eq projection ?projection) (eq recursion ?recursion))
                                    (reverse ?rest)))
                                 (((the sequence (children-of (the tree/node document)))
                                   (the ?child-type (elt (the sequence document) ?child-index))
                                   . ?rest)
                                  (bind ((child-iomap (elt (va node-child-iomaps) ?child-index)))
                                    (pattern-case (selection-of (output-of (content-iomap-of child-iomap)))
                                      (((the text/text (text/subseq (the text/text document) ?child-character-index ?child-character-index)))
                                       (bind ((character-index (find-parent-character-index child-iomap ?child-character-index)))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                      (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                       `((the text/text (text/subbox (the text/text document)
                                                                     ,(find-parent-character-index child-iomap ?start-character-index)
                                                                     ,(find-parent-character-index child-iomap ?end-character-index))))))))
                                 (?a
                                  (pattern-case (selection-of input)
                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                      (the text/text (opening-delimiter-of (the tree/node document))))
                                     `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                      (the text/text (closing-delimiter-of (the tree/node document))))
                                     (bind ((total-length (text/length (text/make-text output-elements)))
                                            (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0))
                                            (character-index (+ (- total-length closing-delimiter-length) ?character-index)))
                                       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap 'iomap/tree/node->text/text
                :input input :input-reference input-reference :output output
                :projection projection :recursion recursion
                :child-iomaps node-child-iomaps)))

;;;;;;
;;; Reader

(def reader tree/leaf->text/text (projection recursion input printer-iomap)
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap))
         (content-iomap (content-iomap-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text/reference? selection))
         (tree-selection? (tree/reference? selection)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                   (bind ((content-output (output-of content-iomap))
                                                          (content-output-origin-position (text/origin-position content-output))
                                                          (content-output-position (text/relative-position content-output content-output-origin-position ?character-index)))
                                                     (if content-output-position
                                                         (bind ((content-command (recurse-reader recursion input content-iomap))
                                                                (content-operation (operation-of content-command)))
                                                           (when (typep content-operation 'operation/replace-selection)
                                                             `(,@(selection-of content-operation) (the ,(form-type (content-of printer-input)) (content-of (the tree/leaf document))))))
                                                         (if (< ?character-index 0)
                                                             (bind ((distance (+ ?character-index
                                                                                 (text/length (opening-delimiter-of printer-input))
                                                                                 (text/length content-output (text/first-position content-output) content-output-origin-position))))
                                                               `((the text/text (text/subseq (the text/text document) ,distance ,distance))
                                                                 (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                             (bind ((distance (- ?character-index (text/length content-output content-output-origin-position (text/last-position content-output)))))
                                                               `((the text/text (text/subseq (the text/text document) ,distance ,distance))
                                                                 (the text/text (closing-delimiter-of (the tree/leaf document))))))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/text/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                                                   (bind ((content-output (output-of content-iomap))
                                                          (content-output-origin-position (text/origin-position content-output))
                                                          (content-output-start-position (text/relative-position content-output content-output-origin-position ?start-character-index))
                                                          (content-output-end-position (text/relative-position content-output content-output-origin-position ?end-character-index)))
                                                     (when (and content-output-start-position content-output-end-position)
                                                       (bind ((content-command (recurse-reader recursion input content-iomap))
                                                              (content-operation (operation-of content-command)))
                                                         (when (typep content-operation 'operation/text/replace-range)
                                                           `(,@(selection-of content-operation) (the ,(form-type (content-of printer-input)) (content-of (the tree/leaf document))))))))))
                                           (make-operation/text/replace-range printer-input it (replacement-of operation))))
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
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a text selection"
                       :operation (when tree-selection?
                                    (make-operation/replace-selection printer-input (if (opening-delimiter-of printer-input)
                                                                                        `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (opening-delimiter-of (the tree/leaf document))))
                                                                                        `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (content-of (the tree/leaf document))))))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a tree selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection printer-input '((the tree/leaf document)))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Tree" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection `((the tree/leaf document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection printer-input new-selection)))))
                    (make-command/nothing (gesture-of input)))))

(def reader tree/node->text/text (projection recursion input printer-iomap)
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text/reference? selection))
         (tree-selection? (tree/reference? selection)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a text position selection"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (,(if (opening-delimiter-of node) 'opening-delimiter-of 'content-of) (the ,(form-type node) document)))
                                                                                          ,@(rest selection)))))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a tree node selection"
                       :operation (when text-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (make-operation/replace-selection printer-input `((the ,(form-type node) document) ,@node-reference))))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Tree" :description "Expands or collapses the selected ndoe"
                       :operation (progn
                                    #+nil
                                    (awhen (find-tree-node-parent-reference (find-tree-node-reference selection))
                                      (make-operation/tree/toggle-expanded printer-input it))))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Tree" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection '((the tree/node document))))
                                    (unless (equal new-selection (selection-of printer-input))
                                      (make-operation/replace-selection printer-input new-selection))))
                      ((gesture/keyboard/key-press :sdl-key-up)
                       :domain "Tree" :description "Moves the selection to the parent node"
                       :operation (when tree-selection?
                                    #+nil
                                    (make-operation/replace-selection printer-input `((the tree/node document) ,@(find-tree-node-parent-reference (find-tree-node-reference selection))))))
                      ((gesture/keyboard/key-press :sdl-key-down)
                       :domain "Tree" :description "Moves the selection to the first child node"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (when (and (typep node 'tree/node)
                                                   (> (length (children-of node)) 0))
                                          (bind ((first-child (elt (children-of node) 0)))
                                            (make-operation/replace-selection printer-input `((the ,(form-type first-child) document)
                                                                                              (the ,(form-type first-child) (elt (the sequence document) 0))
                                                                                              (the sequence (children-of (the tree/node document)))
                                                                                              ,@(rest node-reference)))))))))
                      ((gesture/keyboard/key-press :sdl-key-home)
                       :domain "Tree" :description "Moves the selection to the first sibling node"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        (((the ?type (?if (subtypep ?type 'tree/base)) document)
                                          (the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
                                          (the sequence (children-of (the tree/node document)))
                                          . ?rest)
                                         (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?rest))))
                                                (type (type-of (elt (children-of parent-node) 0))))
                                           (make-operation/replace-selection printer-input `((the ,type document)
                                                                                             (the ,type (elt (the sequence document) 0))
                                                                                             (the sequence (children-of (the tree/node document)))
                                                                                             ,@?rest))))))))
                      ((gesture/keyboard/key-press :sdl-key-end)
                       :domain "Tree" :description "Moves the selection to the last sibling node"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        (((the ?type (?if (subtypep ?type 'tree/base)) document)
                                          (the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
                                          (the sequence (children-of (the tree/node document)))
                                          . ?rest)
                                         (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?rest)))))
                                           (bind ((index (1- (length (children-of parent-node))))
                                                  (type (type-of (elt (children-of parent-node) index))))
                                             (make-operation/replace-selection printer-input `((the ,type document)
                                                                                               (the ,type (elt (the sequence document) ,index))
                                                                                               (the sequence (children-of (the tree/node document)))
                                                                                               ,@?rest)))))))))
                      ((gesture/keyboard/key-press :sdl-key-left)
                       :domain "Tree" :description "Moves the selection to the preceding sibling node"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        (((the ?type (?if (subtypep ?type 'tree/base)) document)
                                          (the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
                                          (the sequence (children-of (the tree/node document)))
                                          . ?rest)
                                         (when (> ?b 0)
                                           (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?rest))))
                                                  (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                             (make-operation/replace-selection printer-input `((the ,type document)
                                                                                               (the ,type (elt (the sequence document) ,(1- ?b)))
                                                                                               (the sequence (children-of (the tree/node document)))
                                                                                               ,@?rest)))))))))
                      ((gesture/keyboard/key-press :sdl-key-right)
                       :domain "Tree" :description "Moves the selection to the following sibling node"
                       :operation (when tree-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        (((the ?type (?if (subtypep ?type 'tree/base)) document)
                                          (the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
                                          (the sequence (children-of (the tree/node document)))
                                          . ?rest)
                                         (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?rest)))))
                                           (when (< ?b (1- (length (children-of parent-node))))
                                             (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                               (make-operation/replace-selection printer-input `((the ,type document)
                                                                                                 (the ,type (elt (the sequence document) ,(1+ ?b)))
                                                                                                 (the sequence (children-of (the tree/node document)))
                                                                                                 ,@?rest))))))))))
                      ((gesture/keyboard/key-press :sdl-key-up :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the parent node"
                       :operation (when text-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                      (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                        (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                        ,@node-reference)))))
                      ((gesture/keyboard/key-press :sdl-key-down :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the first child node"
                       :operation (when text-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (when (and (typep node 'tree/node)
                                                   (> (length (children-of node)) 0))
                                          (bind ((first-child (elt (children-of node) 0)))
                                            (make-operation/replace-selection printer-input
                                                                              (etypecase first-child
                                                                                (tree/leaf `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                             (the text/text (content-of (the tree/leaf document)))
                                                                                             (the tree/leaf (elt (the sequence document) 0))
                                                                                             (the sequence (children-of (the tree/node document)))
                                                                                             ,@node-reference))
                                                                                (tree/node `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                             (the tree/node (elt (the sequence document) 0))
                                                                                             (the sequence (children-of (the tree/node document)))
                                                                                             ,@node-reference))))))))))
                      ((gesture/keyboard/key-press :sdl-key-left :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the preceding sibling node"
                       :operation (when text-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence (children-of (the tree/node ?a))) ?b))
                                         (when (> ?b 0)
                                           (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?a))))
                                                  (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                             (make-operation/replace-selection printer-input
                                                                               (ecase type
                                                                                 (tree/leaf `(the text/text (text/subseq (the text/text (content-of (the ,type (elt (the sequence (children-of (the tree/node ,?a))) ,(1- ?b))))) 0 0)))
                                                                                 (tree/node `(the text/text (text/subseq (the text/text (opening-delimiter-of (the ,type (elt (the sequence (children-of (the tree/node ,?a))) ,(1- ?b))))) 0 0))))))))))))
                      ((gesture/keyboard/key-press :sdl-key-right :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the following sibling node"
                       :operation (when text-selection?
                                    #+nil
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (pattern-case node-reference
                                        ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence (children-of (the tree/node ?a))) ?b))
                                         (bind ((parent-node (eval-reference printer-input (reference/flatten (reverse ?a)))))
                                           (when (< ?b (1- (length (children-of parent-node))))
                                             (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                               (make-operation/replace-selection printer-input
                                                                                 (ecase type
                                                                                   (tree/leaf `(the text/text (text/subseq (the text/text (content-of (the ,type (elt (the sequence (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0 0)))
                                                                                   (tree/node `(the text/text (text/subseq (the text/text (opening-delimiter-of (the ,type (elt (the sequence (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0 0)))))))))))))
                      #+nil
                      ((gesture/keyboard/key-press :sdl-key-delete)
                       :domain "Tree" :description "Deletes the tree node"
                       :operation (when tree-selection?
                                    ;; TODO:
                                    (make-operation/replace input nil (document/nothing)))))
                    (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?parent-character-index ?parent-character-index)) . ?rest)
                                                   (bind (((:values child-index child-character-index) (find-child-character-index printer-iomap ?parent-character-index)))
                                                     (if child-index
                                                         (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                                                (child (input-of child-iomap))
                                                                (input-child-operation (make-operation/replace-selection child `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index)))))
                                                                (input-child-command (make-command nil input-child-operation :domain (domain-of input) :description (description-of input)))
                                                                (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                                                (output-child-operation (operation-of output-child-command)))
                                                           (when (typep output-child-operation 'operation/replace-selection)
                                                             (append (selection-of output-child-operation)
                                                                     `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                       (the sequence (children-of (the tree/node document)))))))
                                                         (append (selection-of operation) `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/text/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-parent-character-index ?end-parent-character-index)) . ?rest)
                                                   (bind (((:values start-child-index start-child-character-index) (find-child-character-index printer-iomap ?start-parent-character-index))
                                                          ((:values end-child-index end-child-character-index) (find-child-character-index printer-iomap ?end-parent-character-index)))
                                                     (when (and start-child-index end-child-index (= start-child-index end-child-index))
                                                       (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of printer-iomap) start-child-index)))
                                                              (child (input-of child-iomap))
                                                              (input-child-operation (make-operation/text/replace-range child `((the text/text (text/subseq (the text/text document) ,start-child-character-index ,end-child-character-index))) (replacement-of operation)))
                                                              (input-child-command (make-command gesture input-child-operation :domain (domain-of input) :description (description-of input)))
                                                              (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                                              (output-child-operation (operation-of output-child-command)))
                                                         (when (typep output-child-operation 'operation/text/replace-range)
                                                           (append (selection-of output-child-operation)
                                                                   `((the ,(form-type child) (elt (the sequence document) ,start-child-index))
                                                                     (the sequence (children-of (the tree/node document)))))))))))
                                           (make-operation/text/replace-range printer-input it (replacement-of operation))))
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
