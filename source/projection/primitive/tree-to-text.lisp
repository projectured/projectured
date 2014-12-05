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
  ((child-iomap :type iomap)
   (child-indentation :type integer)
   (child-first-character-index :type integer)
   (child-last-character-index :type integer)))

;;;;;;
;;; Forward mapper

(def function tree-parent-character-index (child-iomap child-character-index)
  (bind ((child-indentation (child-indentation-of child-iomap))
         (child-first-character-index (child-first-character-index-of child-iomap))
         (child-line-index (count #\NewLine (text/as-string (output-of (child-iomap-of child-iomap))) :end child-character-index)))
    (+ (* child-line-index child-indentation) child-first-character-index child-character-index)))

;;;;;;
;;; Backward mapper

(def function tree-child-character-index (iomap parent-character-index)
  (iter (for child-index :from 0)
        (for child-iomap :in-sequence (child-iomaps-of iomap))
        (for child-first-character-index = (child-first-character-index-of child-iomap))
        (for child-last-character-index = (child-last-character-index-of child-iomap))
        (for child-indentation = (child-indentation-of child-iomap))
        (when (<= child-first-character-index parent-character-index child-last-character-index)
          (bind ((output-string (text/as-string (output-of (child-iomap-of child-iomap))))
                 (child-line-index (funcall 'count #\NewLine output-string :end (- parent-character-index child-first-character-index))))
            (return (values child-index (- parent-character-index child-first-character-index (* child-line-index child-indentation))))))))

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
  (bind ((content-iomap (as (recurse-printer recursion (content-of input)
                                             `((content-of (the tree/leaf document))
                                               ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (bind ((content-output (output-of (va content-iomap))))
                                 (pattern-case (reverse (selection-of input))
                                   (((the tree/leaf document))
                                    (bind ((opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                           (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                      `((the text/text (text/subbox (the text/text document) 0 ,(+ (text/length content-output) opening-delimiter-length closing-delimiter-length))))))
                                   (((the ?content-type (content-of (the tree/leaf document)))
                                     . ?rest)
                                    (pattern-case (selection-of content-output)
                                      (((the text/text (text/subseq (the text/text document) ?index ?index)))
                                       (bind ((character-index (+ ?index
                                                                  (aif (opening-delimiter-of input)
                                                                       (text/length it)
                                                                       0))))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))))
                                   (?a
                                    (pattern-case (selection-of input)
                                      (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                        (the text/text (opening-delimiter-of (the tree/leaf document))))
                                       `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
                                      (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                        (the text/text (closing-delimiter-of (the tree/leaf document))))
                                       (bind ((content-length (text/length (output-of (va content-iomap))))
                                              (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                              (character-index (+ ?character-index opening-delimiter-length content-length)))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))))))))
         (output-elements (as (concatenate 'vector
                                           (awhen (and (output-delimiters-p projection)
                                                       (opening-delimiter-of input))
                                             (elements-of it))
                                           (elements-of (output-of (va content-iomap)))
                                           (awhen (and (output-delimiters-p projection)
                                                       (closing-delimiter-of input))
                                             (elements-of it)))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap 'iomap/tree/leaf->text/text :input input :output output :content-iomap content-iomap)))

(def printer tree/node->text/text (projection recursion input input-reference)
  (bind ((separator-length (awhen (separator-of input) (text/length it)))
         (opening-delimiter-length (awhen (opening-delimiter-of input) (text/length it)))
         (child-iomaps (as (map-ll* (ll (children-of input)) (lambda (child index)
                                                               (recurse-printer recursion child
                                                                                `((elt-ll (the sequence document) ,index)
                                                                                  (the sequence (children-of (the tree/node document)))
                                                                                  ,@(typed-reference (form-type input) input-reference)))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-first-character-index child-last-character-index)
                                           (make-computed-ll (as (bind ((child-iomap (value-of element))
                                                                        (child (input-of child-iomap))
                                                                        (child-length (text/length (output-of child-iomap)))
                                                                        (child-indentation (indentation-of child)))
                                                                   (make-iomap 'iomap/tree/node->text/text/child
                                                                               :child-iomap child-iomap
                                                                               :child-indentation (or child-indentation 0)
                                                                               :child-first-character-index (or child-first-character-index
                                                                                                                (- child-last-character-index child-length))
                                                                               :child-last-character-index (or child-last-character-index
                                                                                                               (+ child-first-character-index child-length)))))
                                                             (as (or previous-element
                                                                     (awhen (previous-element-of element)
                                                                       (recurse it nil -self-
                                                                                nil
                                                                                (- (or child-first-character-index
                                                                                       (- child-last-character-index
                                                                                          (text/length (output-of (value-of element)))))
                                                                                   (or separator-length 0)
                                                                                   (bind ((indentation (indentation-of (input-of (value-of element)))))
                                                                                     (if indentation
                                                                                         (1+ (* (1+ (text/count (output-of (value-of element)) #\NewLine)) indentation))
                                                                                         0)))))))
                                                             (as (or next-element
                                                                     (awhen (next-element-of element)
                                                                       (recurse it -self- nil
                                                                                (+ (or child-last-character-index
                                                                                       (+ child-first-character-index
                                                                                          (text/length (output-of (value-of element)))))
                                                                                   (or separator-length 0)
                                                                                   (bind ((indentation (indentation-of (input-of (value-of it)))))
                                                                                     (if indentation
                                                                                         (1+ (* (1+ (text/count (output-of (value-of element)) #\NewLine)) indentation))
                                                                                         0)))
                                                                                nil)))))))
                                  (recurse (va child-iomaps) nil nil (or opening-delimiter-length 0) nil))))
         (output-elements (as (append-ll (ll (append (awhen (opening-delimiter-of input)
                                                       (list (ll (elements-of it))))
                                                     (list (append-ll (bind ((indented-children (map-ll (va child-iomaps)
                                                                                                        (lambda (child-iomap)
                                                                                                          (bind ((child (input-of child-iomap))
                                                                                                                 (child-elements (elements-of (output-of child-iomap)))
                                                                                                                 (indentation-elements (awhen (indentation-of child)
                                                                                                                                         (append (list (text/newline))
                                                                                                                                                 (unless (zerop it)
                                                                                                                                                   (list (text/string (make-string-of-spaces it))))))))
                                                                                                            (if indentation-elements
                                                                                                                (append-ll (ll (list (ll indentation-elements)
                                                                                                                                     (append-ll (map-ll (ll child-elements)
                                                                                                                                                        (lambda (child-element)
                                                                                                                                                          (if (and (text/newline? child-element)
                                                                                                                                                                   (> (indentation-of child) 0))
                                                                                                                                                              (ll (list child-element (text/string (make-string-of-spaces (indentation-of child)))))
                                                                                                                                                              (ll (list child-element))))))) 1))
                                                                                                                (ll child-elements)))))))
                                                                        (aif (separator-of input)
                                                                             (separate-elements-ll indented-children (ll (elements-of it)))
                                                                             indented-children))))
                                                     (awhen (closing-delimiter-of input)
                                                       (list (ll (elements-of it)))))
                                             (if (opening-delimiter-of input) 1 0)))))
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
                                    (pattern-case (selection-of (output-of (child-iomap-of child-iomap)))
                                      (((the text/text (text/subseq (the text/text document) ?child-character-index ?child-character-index)))
                                       (bind ((character-index (tree-parent-character-index child-iomap ?child-character-index)))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                      (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                       `((the text/text (text/subbox (the text/text document)
                                                                     ,(tree-parent-character-index child-iomap ?start-character-index)
                                                                     ,(tree-parent-character-index child-iomap ?end-character-index))))))))
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

(def function find-tree-node-reference (reference)
  (pattern-case reference
    (((the sequence ((?or subseq text/subseq) (the (?or string text/text) document) ?c ?c))
      (the (?or string text/text) (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                       ((?or opening-delimiter-of closing-delimiter-of) (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                       (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                  (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                       (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)))
      . ?rest)
     ?rest)
    (((the sequence ((?or subseq text/subseq) (the (?or string text/text) document) ?c ?c))
      (the (?or string text/text) (content-of (the text/string document)))
      (the text/string (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                            ((?or opening-delimiter-of closing-delimiter-of) (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                            (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                       (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                            (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)))
      . ?rest)
     `((the ,?type ,?a) ,@?rest))
    (((the ?type (?if (subtypep ?type 'tree/base)) ?a)
      . ?rest)
     `((the ,?type ,?a) ,@?rest))))

(def function find-tree-node-parent-reference (reference)
  (pattern-case reference
    (((the ?type document)
      (the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
      (the sequence (children-of (the tree/node document)))
      . ?rest)
     ?rest)
    (((the ?type (?if (subtypep ?type 'tree/base)) (elt (the sequence document) ?b))
      (the sequence (children-of (the tree/node document)))
      . ?rest)
     ?rest)))

(def function tree/leaf->text/text/read-backward (printer-iomap command)
  (bind ((printer-input (input-of printer-iomap))
         (content-iomap (content-iomap-of printer-iomap))
         (gesture (gesture-of command)))
    (awhen (labels ((recurse (operation)
                      (typecase operation
                        (operation/quit operation)
                        (operation/functional operation)
                        (operation/replace-selection
                         (awhen (if (and (typep gesture 'gesture/mouse/button/click)
                                         (equal (modifiers-of gesture) '(:control)))
                                    '((the tree/leaf document))
                                    (pattern-case (selection-of operation)
                                      (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)) . ?rest)
                                       (bind ((content (output-of content-iomap))
                                              (content-length (text/length content))
                                              (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                              (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                         (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
                                                 (bind ((character-index (- ?character-index opening-delimiter-length)))
                                                   `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                                     (the text/text (content-of (the tree/leaf document))))))
                                                ((<= ?character-index opening-delimiter-length)
                                                 `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                                   (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                                 (bind ((character-index (- ?character-index opening-delimiter-length content-length)))
                                                   `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                                     (the text/text (closing-delimiter-of (the tree/leaf document)))))))))))
                           (make-operation/replace-selection printer-input it)))
                        (operation/sequence/replace-element-range
                         (awhen (pattern-case (target-of operation)
                                  (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)) . ?rest)
                                   (bind ((content (output-of content-iomap))
                                          (content-length (text/length content))
                                          (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                          (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                     (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
                                             (bind ((character-index (- ?character-index opening-delimiter-length)))
                                               `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                                 (the text/text (content-of (the tree/leaf document))))))
                                            ((<= ?character-index opening-delimiter-length)
                                             `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                               (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                            ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                             (bind ((character-index (- ?character-index opening-delimiter-length content-length)))
                                               `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                                 (the text/text (closing-delimiter-of (the tree/leaf document)))))))))
                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)) . ?rest)
                                   (bind ((content (output-of content-iomap))
                                          (content-length (text/length content))
                                          (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0)))
                                     (when (and (<= opening-delimiter-length ?start-character-index (+ opening-delimiter-length content-length))
                                                (<= opening-delimiter-length ?end-character-index (+ opening-delimiter-length content-length)))
                                       `((the text/text (text/subseq (the text/text document) ,(- ?start-character-index opening-delimiter-length) ,(- ?end-character-index opening-delimiter-length)))
                                         (the text/text (content-of (the tree/leaf document))))))))
                           (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                        (operation/describe
                         (make-instance 'operation/describe
                                        :target (pattern-case (target-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)) . ?rest)
                                                   (bind ((content (output-of content-iomap))
                                                          (content-length (text/length content))
                                                          (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                                          (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                                     (econd ((<= opening-delimiter-length ?start-character-index (+ opening-delimiter-length content-length))
                                                             `((the text/text (text/subseq (the text/text document) ,(- ?start-character-index opening-delimiter-length) ,(- ?end-character-index opening-delimiter-length)))
                                                               (the text/text (content-of (the tree/leaf document)))))
                                                            ((<= ?start-character-index opening-delimiter-length)
                                                             `((the text/text (text/elt (the text/text document) ,?start-character-index ,?end-character-index))
                                                               (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                            ((<= ?start-character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                                             `((the text/text (text/subseq (the text/text document) ,(- ?start-character-index opening-delimiter-length content-length) ,(- ?end-character-index opening-delimiter-length content-length)))
                                                               (the text/text (closing-delimiter-of (the tree/leaf document)))))))))))
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
             (recurse (operation-of command)))
      (make-command gesture it
                    :domain (domain-of command)
                    :description (description-of command)))))

(def reader tree/leaf->text/text (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (content-iomap (content-iomap-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text/reference? selection))
         (tree-selection? (tree/reference? selection))
         (content-command (recurse-reader recursion input content-iomap)))
    (merge-commands (tree/leaf->text/text/read-backward printer-iomap content-command)
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

(def function tree/node->text/text/read-backward (projection recursion command printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (gesture (gesture-of command)))
    (awhen (labels ((recurse (operation)
                      (typecase operation
                        (operation/quit operation)
                        (operation/functional operation)
                        (operation/replace-selection
                         (pattern-case (selection-of operation)
                           (((the text/text (text/subseq (the text/text document) ?parent-character-index ?parent-character-index)) . ?rest)
                            (bind (#+nil
                                   (total-length (text/length (output-of printer-iomap)))
                                   (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                   (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                              (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                (if child-index
                                    (bind ((child-iomap (child-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                           (child (input-of child-iomap))
                                           (input-child-operation (make-operation/replace-selection child `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index)))))
                                           (output-child-operation (operation-of (recurse-reader recursion (make-command nil input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                      (when output-child-operation
                                        (make-operation/replace-selection printer-input
                                                                          (append (selection-of output-child-operation)
                                                                                  `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                                    (the sequence (children-of (the tree/node document))))))))
                                    (make-operation/replace-selection printer-input
                                                                      (append (selection-of operation) `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))))
                                #+nil
                                (cond ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                       (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                         (if child-index
                                             (bind ((child-iomap (child-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                                    (child (input-of child-iomap))
                                                    (input-child-operation (make-operation/replace-selection child `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index)))))
                                                    (output-child-operation (operation-of (recurse-reader recursion (make-command nil input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                               (when output-child-operation
                                                 (make-operation/replace-selection printer-input
                                                                                   (append (selection-of output-child-operation)
                                                                                           `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                                             (the sequence (children-of (the tree/node document))))))))
                                             (make-operation/replace-selection printer-input
                                                                               (append (selection-of operation) `((the text/text (printer-output (the tree/node document) ,projection ,recursion))))))))
                                      ((<= 0 ?parent-character-index opening-delimiter-length)
                                       (make-operation/replace-selection printer-input (if (and (typep gesture 'gesture/mouse/button/click)
                                                                                                (equal (modifiers-of gesture) '(:control)))
                                                                                           '((the tree/node document))
                                                                                           `((the text/text (text/subseq (the text/text document) ,?parent-character-index ,?parent-character-index))
                                                                                             (the text/text (opening-delimiter-of (the tree/node document)))))))
                                      ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
                                       (make-operation/replace-selection printer-input
                                                                         (if (and (typep gesture 'gesture/mouse/button/click)
                                                                                  (equal (modifiers-of gesture) '(:control)))
                                                                             '((the tree/node document))
                                                                             (bind ((character-index (+ (- ?parent-character-index total-length) opening-delimiter-length)))
                                                                               `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                                                                 (the text/text (closing-delimiter-of (the tree/node document))))))))))))))
                        (operation/sequence/replace-element-range
                         (awhen (pattern-case (target-of operation)
                                  (((the text/text (text/subseq (the text/text document) ?parent-character-index ?parent-character-index)) . ?rest)
                                   (bind (#+nil(total-length (text/length (output-of printer-iomap)))
                                               (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                               (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                     (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                       (when child-index
                                         (bind ((child-iomap (child-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                                (child (input-of child-iomap))
                                                (input-child-operation (make-operation/sequence/replace-element-range child `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index))) (replacement-of operation)))
                                                (output-child-operation (operation-of (recurse-reader recursion (make-command gesture input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                           (when (typep output-child-operation 'operation/sequence/replace-element-range)
                                             (append (target-of output-child-operation)
                                                     `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                       (the sequence (children-of (the tree/node document)))))))))
                                     #+nil
                                     (econd ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                             (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                               (when child-index
                                                 (bind ((child-iomap (child-iomap-of (elt (child-iomaps-of printer-iomap) child-index)))
                                                        (child (input-of child-iomap))
                                                        (input-child-operation (make-operation/sequence/replace-element-range child `((the text/text (text/subseq (the text/text document) ,child-character-index ,child-character-index))) (replacement-of operation)))
                                                        (output-child-operation (operation-of (recurse-reader recursion (make-command gesture input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                                   (when (typep output-child-operation 'operation/sequence/replace-element-range)
                                                     (append (target-of output-child-operation)
                                                             `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                               (the sequence (children-of (the tree/node document))))))))))
                                            ((<= 0 ?parent-character-index opening-delimiter-length)
                                             `((the text/text (text/subseq (the text/text document) ,?parent-character-index ,?parent-character-index))
                                               (the text/text (opening-delimiter-of (the tree/node document)))))
                                            ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
                                             (bind ((character-index (+ (- ?parent-character-index total-length) opening-delimiter-length)))
                                               `((the text/text (text/subseq (the text/text document)  ,character-index ,character-index))
                                                 (the text/text (closing-delimiter-of (the tree/node document)))))))))
                                  (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)) . ?rest)
                                   (bind ((total-length (text/length (output-of printer-iomap)))
                                          (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                          (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                     (when (and (<= opening-delimiter-length ?start-index (- total-length closing-delimiter-length))
                                                (<= opening-delimiter-length ?end-index (- total-length closing-delimiter-length)))
                                       (bind (((:values start-child-index start-child-character-index) (tree-child-character-index printer-iomap ?start-index))
                                              ((:values end-child-index end-child-character-index) (tree-child-character-index printer-iomap ?end-index)))
                                         (when (and start-child-index end-child-index (= start-child-index end-child-index))
                                           (bind ((child-iomap (child-iomap-of (elt (child-iomaps-of printer-iomap) start-child-index)))
                                                  (child (input-of child-iomap))
                                                  (input-child-operation (make-operation/sequence/replace-element-range child `((the text/text (text/subseq (the text/text document) ,start-child-character-index ,end-child-character-index))) (replacement-of operation)))
                                                  (output-child-operation (operation-of (recurse-reader recursion (make-command gesture input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                             (when (typep output-child-operation 'operation/sequence/replace-element-range)
                                               (append (target-of output-child-operation)
                                                       `((the ,(form-type child) (elt (the sequence document) ,start-child-index))
                                                         (the sequence (children-of (the tree/node document)))))))))))))
                           (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                        (operation/describe
                         (pattern-case (target-of operation)
                           (((the tex/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
                            (bind ((total-length (text/length (output-of printer-iomap)))
                                   (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                   (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                              (make-instance 'operation/describe
                                             :target (econd ((<= opening-delimiter-length ?parent-start-character-index (- total-length closing-delimiter-length))
                                                             (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-start-character-index))
                                                                    (child-iomap (elt (child-iomaps-of printer-iomap) child-index))
                                                                    (child (input-of child-iomap))
                                                                    (input-child-operation (make-instance 'operation/describe :target `((the text/text (text/subseq (the text/text document) ,child-character-index ,(1+ child-character-index))))))
                                                                    (output-child-operation (operation-of (recurse-reader recursion (make-command gesture input-child-operation :domain (domain-of command) :description (description-of command)) child-iomap))))
                                                               (append (target-of output-child-operation)
                                                                       `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                         (the sequence (children-of (the tree/node document)))))))
                                                            ((<= 0 ?parent-start-character-index opening-delimiter-length)
                                                             `((the text/text (text/subseq (the text/text document) ,?parent-start-character-index ,(1+ ?parent-start-character-index)))
                                                               (the text/text (opening-delimiter-of (the tree/node document)))))
                                                            ((<= (- total-length closing-delimiter-length) ?parent-start-character-index total-length)
                                                             `((the text/text (text/subseq (the text/text document) ,(+ (- ?parent-start-character-index total-length) opening-delimiter-length) ,(+ (- ?parent-start-character-index total-length) opening-delimiter-length)))
                                                               (the text/text (closing-delimiter-of (the tree/node document)))))))))))
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
             (recurse (operation-of command)))
      (make-command (gesture-of command) it
                    :domain (domain-of command)
                    :description (description-of command)))))

(def reader tree/node->text/text (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text/reference? selection))
         (tree-selection? (tree/reference? selection)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a text position selection"
                       :operation (when tree-selection?
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                          (the text/text (,(if (opening-delimiter-of node) 'opening-delimiter-of 'content-of) (the ,(form-type node) document)))
                                                                                          ,@(rest selection)))))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Tree" :description "Turn the selection into a tree node selection"
                       :operation (when text-selection?
                                    (when-bind node-reference (find-tree-node-reference selection)
                                      (bind ((node (eval-reference printer-input (reference/flatten (reverse node-reference)))))
                                        (make-operation/replace-selection printer-input `((the ,(form-type node) document) ,@node-reference))))))
                      ((gesture/keyboard/key-press :sdl-key-d :control)
                       :domain "Tree" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p projection) (not (output-delimiters-p projection))))))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Tree" :description "Expands or collapses the selected ndoe"
                       :operation (awhen (find-tree-node-parent-reference (find-tree-node-reference selection))
                                    (make-operation/tree/toggle-expanded printer-input it)))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Tree" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection '((the tree/node document))))
                                    (unless (equal new-selection (selection-of printer-input))
                                      (make-operation/replace-selection printer-input new-selection))))
                      ((gesture/keyboard/key-press :sdl-key-up)
                       :domain "Tree" :description "Moves the selection to the parent node"
                       :operation (when tree-selection?
                                    (make-operation/replace-selection printer-input `((the tree/node document) ,@(find-tree-node-parent-reference (find-tree-node-reference selection))))))
                      ((gesture/keyboard/key-press :sdl-key-down)
                       :domain "Tree" :description "Moves the selection to the first child node"
                       :operation (when tree-selection?
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
                                    (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                      (make-operation/replace-selection printer-input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                                        (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                        ,@node-reference)))))
                      ((gesture/keyboard/key-press :sdl-key-down :alt)
                       :domain "Tree" :description "Moves the selection to the first character of the first child node"
                       :operation (when text-selection?
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
                    (tree/node->text/text/read-backward projection recursion input printer-iomap)
                    (make-command/nothing (gesture-of input)))))
