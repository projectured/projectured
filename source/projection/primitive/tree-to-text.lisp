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
   (line-indentation :type integer)
   (origin-character-index :type integer)
   (first-character-index :type integer)
   (last-character-index :type integer)))

;;;;;;
;;; Mapping

(def function find-child-character-index (iomap parent-character-index)
  (iter (for index :from 0)
        (for child-iomap :in-sequence (child-iomaps-of iomap))
        (for first-character-index = (first-character-index-of child-iomap))
        (for last-character-index = (last-character-index-of child-iomap))
        (when (<= first-character-index parent-character-index last-character-index)
          (bind ((line-indentation (line-indentation-of child-iomap))
                 (indented-child (indented-child-of child-iomap))
                 (indented-character-index (- parent-character-index (origin-character-index-of child-iomap)))
                 (indented-origin-position (text/origin-position indented-child))
                 (indented-character-position (text/relative-position indented-child indented-origin-position indented-character-index))
                 (indented-line-start-position (text/line-start-position indented-child indented-character-position))
                 (indented-line-character-index (text/length indented-child indented-line-start-position indented-character-position))
                 (output (output-of (content-iomap-of child-iomap))))
            (when (or (text/first-position? indented-child indented-line-start-position)
                      (<= line-indentation indented-line-character-index))
              (bind ((indented-line-count (if (< indented-character-index 0)
                                              (text/count indented-child #\NewLine indented-character-position indented-origin-position)
                                              (text/count indented-child #\NewLine indented-origin-position indented-character-position)))
                     (origin-relative-character-index (- indented-character-index (* indented-line-count line-indentation))))
                (when (text/relative-position output (text/origin-position output) origin-relative-character-index)
                  (return (values index origin-relative-character-index)))))))))

(def function find-parent-character-index (child-iomap origin-relative-character-index)
  (bind ((line-indentation (line-indentation-of child-iomap))
         (output (output-of (content-iomap-of child-iomap)))
         (origin-character-index (origin-character-index-of child-iomap))
         (origin-position (text/origin-position output))
         (character-position (text/relative-position output origin-position origin-relative-character-index))
         (line-count (if (< origin-relative-character-index 0)
                         (text/count output #\NewLine character-position origin-position)
                         (text/count output #\NewLine origin-position character-position)))
         (indented-character-index (+ origin-relative-character-index (* line-count line-indentation))))
    (+ origin-character-index indented-character-index)))

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
                                 (((the tree/leaf document))
                                  (bind ((start-index (aif (opening-delimiter-of input) (- (text/length it)) 0))
                                         (end-index (+ (text/length (output-of (va content-iomap))) (aif (closing-delimiter-of input) (text/length it) 0))))
                                    `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
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
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the tree/node document)))
                                                                ,@(typed-reference (form-type input) input-reference))))
                                        (recurse-printer recursion (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-origin-character-index child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child-input (input-of child-iomap))
                                                  (child-output (output-of child-iomap))
                                                  (child-length (as (text/length child-output)))
                                                  (separator-length (aif (separator-of input) (text/length it) 0))
                                                  (last-line-length (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                          (while current-element)
                                                                          (for text = (output-of (value-of current-element)))
                                                                          (for position = (text/find text (text/last-position text) (lambda (ch) (char= ch #\NewLine)) :direction :backward))
                                                                          (if position
                                                                              (return (- (text/length text) (text/length text position (text/last-position text))))
                                                                              (summing (text/length text) :into result))
                                                                          (summing separator-length :into result)
                                                                          (awhen (indentation-of (input-of (value-of current-element)))
                                                                            (return (+ result it)))
                                                                          (finally (return (+ result (aif (opening-delimiter-of input) (text/length it) 0))))))
                                                  (indentation (indentation-of child-input))
                                                  (line-indentation (or indentation last-line-length))
                                                  (indentation-length (as (bind ((child-line-count (text/count-lines child-output)))
                                                                            (+ (* line-indentation (1- child-line-count))
                                                                               (if indentation (1+ indentation) 0))))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of child-output))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element)))
                                                                                                                                      (if (and indentation
                                                                                                                                               (not (previous-element-of child-element-element)))
                                                                                                                                          (if (zerop indentation)
                                                                                                                                              (ll (list (text/newline) child-element) 1)
                                                                                                                                              (ll (list (text/newline) (text/string (make-string-of-spaces indentation)) child-element) 2))
                                                                                                                                          (if (and (not (zerop line-indentation))
                                                                                                                                                   (text/newline? child-element)
                                                                                                                                                   (previous-element-of child-element-element))
                                                                                                                                              (ll (list child-element (text/string (make-string-of-spaces line-indentation))))
                                                                                                                                              (ll (list child-element)))))))))))
                                                                          (indented-child-origin-preceding-length (as (text/length (va indented-child) (text/first-position (va indented-child)) (text/origin-position (va indented-child)))))
                                                                          (indented-child-origin-following-length (as (text/length (va indented-child) (text/origin-position (va indented-child)) (text/last-position (va indented-child))))))
                                                                     (make-iomap 'iomap/tree/node->text/text/child
                                                                                 :content-iomap child-iomap
                                                                                 :indented-child indented-child
                                                                                 :line-indentation line-indentation
                                                                                 :origin-character-index (as (or child-origin-character-index
                                                                                                                 (and child-first-character-index
                                                                                                                      (+ child-first-character-index (va indented-child-origin-preceding-length)))
                                                                                                                 (and child-last-character-index
                                                                                                                      (- child-last-character-index (va indented-child-origin-following-length)))))
                                                                                 :first-character-index (as (or child-first-character-index
                                                                                                                (and child-origin-character-index
                                                                                                                     (- child-origin-character-index (va indented-child-origin-preceding-length)))
                                                                                                                (and child-last-character-index
                                                                                                                     (- child-last-character-index (va child-length) (va indentation-length)))))
                                                                                 :last-character-index (as (or child-last-character-index
                                                                                                               (and child-origin-character-index
                                                                                                                    (+ child-origin-character-index (va indented-child-origin-following-length)))
                                                                                                               (and child-first-character-index
                                                                                                                    (+ child-first-character-index (va child-length) (va indentation-length))))))))
                                                               (as (or previous-element
                                                                       (awhen (previous-element-of element)
                                                                         (recurse it nil -self- nil nil (- (first-character-index-of (value-of -self-)) separator-length)))))
                                                               (as (or next-element
                                                                       (awhen (next-element-of element)
                                                                         (recurse it -self- nil nil (+ (last-character-index-of (value-of -self-)) separator-length) nil))))))))
                                  (awhen (va child-iomaps)
                                    (recurse it nil nil 0 nil nil)))))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p projection))
                                     (opening-delimiter (opening-delimiter-of input))
                                     (closing-delimiter (closing-delimiter-of input)))
                                (if (va node-child-iomaps)
                                    (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                    (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                    (append-ll (bind ((indented-children (map-ll (va node-child-iomaps)
                                                                                                 (lambda (node-child-iomap)
                                                                                                   ;; DEBUG: (format t "~%~S ~A ~A ~A" (text/as-string (indented-child-of node-child-iomap)) (first-character-index-of node-child-iomap) (origin-character-index-of node-child-iomap) (last-character-index-of node-child-iomap))
                                                                                                   (elements-of (indented-child-of node-child-iomap))))))
                                                                 (aif (separator-of input)
                                                                      (separate-elements-ll indented-children (ll (elements-of it)))
                                                                      indented-children)))
                                                    (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter)))
                                    (list (text/string ""))))))
         (output-selection (as (pattern-case (reverse (selection-of input))
                                 (((the tree/node document))
                                  (bind ((text (text/make-text output-elements))
                                         (origin-position (text/origin-position text))
                                         (start-index (- (text/length text (text/first-position text) origin-position)))
                                         (end-index (text/length text origin-position (text/last-position text))))
                                    `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
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
                                                                     ,(find-parent-character-index child-iomap ?end-character-index)))))))))))
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
    (merge-commands (pattern-case (reverse (selection-of printer-input))
                      (((the ?content-type (content-of (the tree/leaf document))) . ?rest)
                       (bind ((content-iomap (content-iomap-of printer-iomap))
                              (output-operation (operation-of (recurse-reader recursion (make-command/nothing gesture) content-iomap))))
                         (awhen (operation/extend printer-input `((the ,?content-type (content-of (the tree/leaf document)))) output-operation)
                           (make-command/clone input it)))))
                    (gesture-case gesture
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
                    (awhen (labels ((recurse (operation)
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
                                        (operation/describe
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                   (bind ((content-output (output-of content-iomap))
                                                          (content-output-origin-position (text/origin-position content-output))
                                                          (content-output-position (text/relative-position content-output content-output-origin-position ?character-index)))
                                                     (if content-output-position
                                                         (bind ((content-command (recurse-reader recursion input content-iomap))
                                                                (content-operation (operation-of content-command)))
                                                           (when (typep content-operation 'operation/describe)
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
                                           (make-operation/describe it)))
                                        (operation/show-context-sensitive-help
                                         (make-instance 'operation/show-context-sensitive-help
                                                        :commands (iter (for command :in (commands-of operation))
                                                                        (awhen (recurse (operation-of command))
                                                                          (collect (make-command/clone command it))))))
                                        (operation/compound
                                         (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null child-operations)
                                             (make-operation/compound child-operations)))))))
                             (recurse (operation-of input)))
                      (make-command/clone input it))
                    (make-command/nothing gesture))))

(def reader tree/node->text/text (projection recursion input printer-iomap)
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text/reference? selection))
         (tree-selection? (tree/reference? selection)))
    (merge-commands (pattern-case (reverse (selection-of printer-input))
                      (((the sequence (children-of (the tree/node document)))
                        (the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                       (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of printer-iomap) ?element-index)))
                              (output-operation (operation-of (recurse-reader recursion (make-command/nothing (gesture-of input)) child-iomap))))
                         (awhen (operation/extend printer-input `((the ,?element-type (elt (the sequence document) ,?element-index))
                                                                  (the sequence (children-of (the tree/node document))))
                                                  output-operation)
                           (make-command/clone input it)))))
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a text position selection"
                       :operation (when tree-selection?
                                    (make-operation/replace-selection printer-input (if (opening-delimiter-of printer-input)
                                                                                        `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (opening-delimiter-of (the tree/node document))))
                                                                                        `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (printer-output (the tree/node document) ,projection ,recursion)))))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a tree node selection"
                       :operation (when text-selection?
                                    (pattern-case selection
                                      (((the text/text (text/subseq (the text/text document) ?start-index ?end-index))
                                        (the text/text (printer-output (the tree/node document) ?projection ?recursion)))
                                       (make-operation/replace-selection printer-input '((the tree/node document)))))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Tree" :description "Expands or collapses the selected ndoe"
                       :operation (make-operation/tree/toggle-expanded printer-input '((the tree/node document))))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Tree" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection '((the tree/node document))))
                                    (unless (equal new-selection (selection-of printer-input))
                                      (make-operation/replace-selection printer-input new-selection))))
                      ((gesture/keyboard/key-press :sdl-key-up)
                       :domain "Tree" :description "Moves the selection to the parent node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the ?child-type document)
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (make-operation/replace-selection printer-input '((the tree/node document)))))))
                      ((gesture/keyboard/key-press :sdl-key-down)
                       :domain "Tree" :description "Moves the selection to the first child node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the tree/node document))
                                       (bind ((child-type (type-of (elt (children-of printer-input) 0))))
                                         (make-operation/replace-selection printer-input `((the ,child-type document)
                                                                                           (the ,child-type (elt (the sequence document) 0))
                                                                                           (the sequence (children-of (the tree/node document))))))))))
                      ((gesture/keyboard/key-press :sdl-key-home)
                       :domain "Tree" :description "Moves the selection to the first sibling node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the ?child-type document)
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (bind ((child-type (type-of (elt (children-of printer-input) 0))))
                                         (make-operation/replace-selection printer-input `((the ,child-type document)
                                                                                           (the ,child-type (elt (the sequence document) 0))
                                                                                           (the sequence (children-of (the tree/node document))))))))))
                      ((gesture/keyboard/key-press :sdl-key-end)
                       :domain "Tree" :description "Moves the selection to the last sibling node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the ?child-type document)
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (bind ((index (1- (length (children-of printer-input))))
                                              (child-type (type-of (elt (children-of printer-input) index))))
                                         (make-operation/replace-selection printer-input `((the ,child-type document)
                                                                                           (the ,child-type (elt (the sequence document) ,index))
                                                                                           (the sequence (children-of (the tree/node document))))))))))
                      ((gesture/keyboard/key-press :sdl-key-left)
                       :domain "Tree" :description "Moves the selection to the preceding sibling node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the ?child-type document)
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (when (> ?child-index 0)
                                         (bind ((child-type (type-of (elt (children-of printer-input) (1- ?child-index)))))
                                           (make-operation/replace-selection printer-input `((the ,child-type document)
                                                                                             (the ,child-type (elt (the sequence document) ,(1- ?child-index)))
                                                                                             (the sequence (children-of (the tree/node document)))))))))))
                      ((gesture/keyboard/key-press :sdl-key-right)
                       :domain "Tree" :description "Moves the selection to the following sibling node"
                       :operation (when tree-selection?
                                    (pattern-case selection
                                      (((the ?child-type document)
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (when (< ?child-index (1- (length (children-of printer-input))))
                                         (bind ((child-type (type-of (elt (children-of printer-input) (1+ ?child-index)))))
                                           (make-operation/replace-selection printer-input `((the ,child-type document)
                                                                                             (the ,child-type (elt (the sequence document) ,(1+ ?child-index)))
                                                                                             (the sequence (children-of (the tree/node document)))))))))))
                      ((gesture/keyboard/key-press :sdl-key-up :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the parent node"
                       :operation (when text-selection?
                                    (pattern-case selection
                                      (((the text/text (text/subseq (the text/text document) 0 0))
                                        (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                        (the tree/node (elt (the sequence document) ?child-index))
                                        (the sequence (children-of (the tree/node document))))
                                       (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                         (the text/text (printer-output (the tree/node document) ,projection ,recursion)))))
                                      (((the text/text (text/subseq (the text/text document) ?start-index ?end-index))
                                        (the text/text (printer-output (the tree/node document) ?projection ?recursion)))
                                       (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                         (the text/text (printer-output (the tree/node document) ,projection ,recursion))))))
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
                                                                                   (tree/node `(the text/text (text/subseq (the text/text (opening-delimiter-of (the ,type (elt (the sequence (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0 0))))))))))))))
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
                                        (operation/describe
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?parent-character-index ?parent-character-index)) . ?rest)
                                                   (bind (((:values child-index child-character-index) (find-child-character-index printer-iomap ?parent-character-index)))
                                                     (if child-index
                                                         (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                                                (child (input-of child-iomap))
                                                                (input-child-operation (make-operation/describe `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index)))))
                                                                (input-child-command (make-command nil input-child-operation :domain (domain-of input) :description (description-of input)))
                                                                (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                                                (output-child-operation (operation-of output-child-command)))
                                                           (when (typep output-child-operation 'operation/describe)
                                                             (append (selection-of output-child-operation)
                                                                     `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                       (the sequence (children-of (the tree/node document)))))))
                                                         (append (selection-of operation) `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))))))
                                           (make-operation/describe it)))
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
