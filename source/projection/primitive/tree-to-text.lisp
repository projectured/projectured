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
;;; Forward mapper

(def function forward-mapper/tree/leaf->text/text (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap))
         (content-iomap (content-iomap-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the tree/leaf document))
       (bind ((start-index (aif (opening-delimiter-of printer-input) (- (text/length it)) 0))
              (end-index (+ (text/length (output-of content-iomap)) (aif (closing-delimiter-of printer-input) (text/length it) 0))))
         `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
      (((the ?content-type (content-of (the tree/leaf document)))
        . ?rest)
       (values nil nil content-iomap))
      (((the text/text (opening-delimiter-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((content-output (output-of content-iomap))
              (opening-delimiter (opening-delimiter-of printer-input))
              (start-character-index (- (+ (text/length opening-delimiter
                                                        (text/relative-position opening-delimiter (text/origin-position opening-delimiter) ?start-character-index)
                                                        (text/last-position opening-delimiter))
                                           (text/length content-output (text/first-position content-output) (text/origin-position content-output)))))
              (end-character-index (- (+ (text/length opening-delimiter
                                                      (text/relative-position opening-delimiter (text/origin-position opening-delimiter) ?end-character-index)
                                                      (text/last-position opening-delimiter))
                                         (text/length content-output (text/first-position content-output) (text/origin-position content-output))))))
         `((the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index)))))
      (((the text/text (closing-delimiter-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((content-output (output-of content-iomap))
              (closing-delimiter (closing-delimiter-of printer-input))
              (start-character-index (+ (text/length closing-delimiter
                                                     (text/first-position closing-delimiter)
                                                     (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?start-character-index))
                                        (text/length content-output (text/origin-position content-output) (text/last-position content-output))))
              (end-character-index (+ (text/length closing-delimiter
                                                   (text/first-position closing-delimiter)
                                                   (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?end-character-index))
                                      (text/length content-output (text/origin-position content-output) (text/last-position content-output)))))
         `((the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index)))))
      (((the text/text (printer-output (the tree/leaf document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/tree/leaf->text/text (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap))
         (content-iomap (content-iomap-of printer-iomap)))
    (pattern-case reference
      (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((content-output (output-of content-iomap))
              (content-output-origin-position (text/origin-position content-output))
              (start-position (text/relative-position content-output content-output-origin-position ?start-character-index))
              (end-position (text/relative-position content-output content-output-origin-position ?end-character-index)))
         (if (and start-position end-position)
             (values `((the ,(form-type (content-of printer-input)) (content-of (the tree/leaf document))))
                     reference
                     content-iomap)
             (if (and (< ?start-character-index 0) (< ?end-character-index 0))
                 (bind ((offset (+ (text/length (opening-delimiter-of printer-input))
                                   (text/length content-output (text/first-position content-output) content-output-origin-position))))
                   `((the text/text (text/subseq (the text/text document) ,(+ offset ?start-character-index) ,(+ offset ?end-character-index)))
                     (the text/text (opening-delimiter-of (the tree/leaf document)))))
                 (bind ((offset (- (text/length content-output content-output-origin-position (text/last-position content-output)))))
                   `((the text/text (text/subseq (the text/text document) ,(+ offset ?start-character-index) ,(+ offset ?end-character-index)))
                     (the text/text (closing-delimiter-of (the tree/leaf document)))))))))
      (?
       (append reference `((the text/text (printer-output (the tree/leaf document) ,projection ,recursion))))))))

(def function backward-mapper/tree/node->text/text (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the text/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
       (bind (((:values child-start-index child-start-character-index) (find-child-character-index printer-iomap ?parent-start-character-index))
              ((:values child-end-index child-end-character-index) (find-child-character-index printer-iomap ?parent-end-character-index)))
         (if (and child-start-index child-end-index (= child-start-index child-end-index))
             (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of printer-iomap) child-start-index)))
                    (child (input-of child-iomap)))
               (values `((the ,(form-type child) (elt (the sequence document) ,child-start-index))
                         (the sequence (children-of (the tree/node document))))
                       `((the text/text (text/subseq (the text/text document) ,child-start-character-index ,child-end-character-index)))
                       child-iomap))
             (if (and (< ?parent-start-character-index 0) (< ?parent-end-character-index 0))
                 (bind ((text (output-of printer-iomap))
                        (offset (text/length text (text/first-position text) (text/origin-position text))))
                   `((the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index)))
                     (the text/text (opening-delimiter-of (the tree/node document)))))
                 (append reference `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))
                 #+nil
                 (bind ((text (output-of printer-iomap))
                        (offset (text/length text (text/origin-position text) (text/last-position text))))
                   `((the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index)))
                     (the text/text (closing-delimiter-of (the tree/node document)))))))))
      (?
       (append reference `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))))))

;;;;;;
;;; Printer

(def printer tree/leaf->text/text (projection recursion input input-reference)
  (bind ((content-reference `(((content-of (the ,(form-type input) document))) ,@(typed-reference (form-type input) input-reference)))
         (content-iomap (as (recurse-printer recursion (content-of input) content-reference)))
         (output-selection (as (print-selection (make-iomap 'iomap/tree/leaf->text/text
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :content-iomap content-iomap)
                                                (selection-of input)
                                                'forward-mapper/tree/leaf->text/text)))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p projection))
                                     (opening-delimiter (opening-delimiter-of input))
                                     (closing-delimiter (closing-delimiter-of input)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (elements-of (output-of (va content-iomap)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap 'iomap/tree/leaf->text/text
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :content-iomap content-iomap)))

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
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (if (va node-child-iomaps)
                                                    (append-ll (bind ((indented-children (map-ll (va node-child-iomaps)
                                                                                                 (lambda (node-child-iomap)
                                                                                                   ;; DEBUG: (format t "~%~S ~A ~A ~A" (text/as-string (indented-child-of node-child-iomap)) (first-character-index-of node-child-iomap) (origin-character-index-of node-child-iomap) (last-character-index-of node-child-iomap))
                                                                                                   (elements-of (indented-child-of node-child-iomap))))))
                                                                 (aif (separator-of input)
                                                                      (separate-elements-ll indented-children (ll (elements-of it)))
                                                                      indented-children)))
                                                    (list-ll (text/string "")))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output-selection (as (pattern-case (reverse (selection-of input))
                                 (((the tree/node document))
                                  (bind ((text (text/make-text output-elements))
                                         (origin-position (text/origin-position text))
                                         (start-index (- (text/length text (text/first-position text) origin-position)))
                                         (end-index (text/length text origin-position (text/last-position text))))
                                    `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
                                 (((the text/text (opening-delimiter-of (the tree/node document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((text (text/make-text output-elements))
                                         (character-index (+ (- (text/length text (text/first-position text) (text/origin-position text))) ?character-index)))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the text/text (closing-delimiter-of (the tree/node document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((text (text/make-text output-elements))
                                         (closing-delimiter (closing-delimiter-of input))
                                         (character-index (+ (text/length closing-delimiter
                                                                          (text/first-position closing-delimiter)
                                                                          (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?character-index))
                                                             (text/length text (text/origin-position text) (text/last-position text)))))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
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
                                 (((the text/text (printer-output (the tree/node document) ?projection ?recursion)) . ?rest)
                                  (when (and (eq projection ?projection) (eq recursion ?recursion))
                                    (reverse ?rest))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap 'iomap/tree/node->text/text
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :projection projection :recursion recursion
                :child-iomaps node-child-iomaps)))

;;;;;;
;;; Reader

(def function tree/make-text-reference (projection recursion printer-iomap &optional child-index)
  (bind ((node-iomap (if child-index
                         (content-iomap-of (elt (child-iomaps-of printer-iomap) child-index))
                         printer-iomap))
         (node-input (input-of node-iomap))
         (node-output (output-of node-iomap))
         (reference (if (opening-delimiter-of node-input)
                        `((the text/text (text/subseq (the text/text document) 0 0))
                          (the text/text (opening-delimiter-of (the ,(form-type node-input) document))))
                        (bind ((index (- (text/length node-output (text/first-position node-output) (text/origin-position node-output)))))
                          (typecase node-input
                            (tree/leaf `((the text/text (text/subseq (the text/text document) ,index ,index))
                                         (the text/text (content-of (the tree/leaf document)))))
                            (tree/node `((the text/text (text/subseq (the text/text document) ,index ,index))
                                         (the text/text (printer-output (the tree/node document) ,projection ,recursion)))))))))
    (if child-index
        (append reference
                `((the ,(form-type node-input) (elt (the sequence document) ,child-index))
                  (the sequence (children-of (the tree/node document)))))
        reference)))

(def reader tree/leaf->text/text (projection recursion input printer-iomap)
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap))
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
                       :domain "Tree" :description "Turns the selection into a text selection"
                       :operation (when tree-selection?
                                    (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turns the selection into a tree selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection printer-input '((the tree/leaf document)))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-home (list :control :alt))
                       :domain "Tree" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection `((the tree/leaf document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection printer-input new-selection)))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/tree/leaf->text/text nil)
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
                              (output-operation (operation-of (recurse-reader recursion (make-command/nothing gesture) child-iomap))))
                         (awhen (operation/extend printer-input `((the ,?element-type (elt (the sequence document) ,?element-index))
                                                                  (the sequence (children-of (the tree/node document))))
                                                  output-operation)
                           (make-command/clone input it)))))
                    (gesture-case gesture
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turns the selection into a text position selection"
                       :operation (when tree-selection?
                                    (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turns the selection into a tree node selection"
                       :operation (when text-selection?
                                    (pattern-case selection
                                      ((?or ((the text/text (text/subseq (the text/text document) ?start-index ?end-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion)))
                                            ((the text/text (text/subseq (the text/text document) ?start-index ?end-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))))
                                       (make-operation/replace-selection printer-input '((the tree/node document)))))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Tree" :description "Expands or collapses the selected ndoe"
                       :operation (make-operation/tree/toggle-expanded printer-input '((the tree/node document))))
                      ((gesture/keyboard/key-press :sdl-key-home (list :control :alt))
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
                                    (pattern-case (reverse selection)
                                      ((?or ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/leaf (elt (the sequence document) ?child-index))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest))
                                       (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap))))))
                      ((gesture/keyboard/key-press :sdl-key-down :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the first child node"
                       :operation (when text-selection?
                                    (pattern-case (reverse (selection-of printer-input))
                                      ((?or ((the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest)
                                            ((the text/text (closing-delimiter-of (the tree/node document)))
                                             . ?rest))
                                       (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap 0))))))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the first sibling node"
                       :operation (when text-selection?
                                    (pattern-case (reverse selection)
                                      ((?or ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap 0))))))
                      ((gesture/keyboard/key-press :sdl-key-end :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the last sibling node"
                       :operation (when text-selection?
                                    (pattern-case (reverse selection)
                                      ((?or ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap (1- (length (children-of printer-input)))))))))
                      ((gesture/keyboard/key-press :sdl-key-left :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the preceding sibling node"
                       :operation (when text-selection?
                                    (pattern-case (reverse selection)
                                      ((?or ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (when (> ?child-index 0)
                                         (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap (1- ?child-index))))))))
                      ((gesture/keyboard/key-press :sdl-key-right :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the following sibling node"
                       :operation (when text-selection?
                                    (pattern-case (reverse selection)
                                      ((?or ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the tree/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the tree/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the tree/node document)))
                                             (the tree/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (when (< ?child-index (1- (length (child-iomaps-of printer-iomap))))
                                         (make-operation/replace-selection printer-input (tree/make-text-reference projection recursion printer-iomap (1+ ?child-index)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/tree/node->text/text nil)
                    (make-command/nothing gesture))))
