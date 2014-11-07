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
  ((child-iomaps :type sequence)
   (child-indentations :type sequence)
   (child-first-character-indices :type sequence)
   (child-last-character-indices :type sequence)))

;;;;;;
;;; Forward mapper

(def function tree-parent-character-index (iomap child-index child-character-index)
  (bind ((child-indentation (elt (child-indentations-of iomap) child-index))
         (child-first-character-index (elt (child-first-character-indices-of iomap) child-index))
         (child-line-index (count #\NewLine (text/as-string (output-of (elt (child-iomaps-of iomap) child-index))) :end child-character-index)))
    (+ (* child-line-index child-indentation) child-first-character-index child-character-index)))

;;;;;;
;;; Backward mapper

(def function tree-child-character-index (iomap parent-character-index)
  (iter (with count = (length (children-of (input-of iomap))))
        (for child-index :from 0 :below count)
        (for child-first-character-index = (elt (child-first-character-indices-of iomap) child-index))
        (for child-last-character-index = (elt (child-last-character-indices-of iomap) child-index))
        (for child-indentation = (elt (child-indentations-of iomap) child-index))
        (when (<= child-first-character-index parent-character-index child-last-character-index)
          (bind ((output-string (text/as-string (output-of iomap)))
                 (child-line-index (funcall 'count #\NewLine output-string :start child-first-character-index :end parent-character-index)))
            (return (values child-index (- parent-character-index child-first-character-index (* child-line-index child-indentation))))
            #+nil ;; TODO: KLUDGE: this was here for no apparent reason, or at least I don't remember
            (unless (print (find #\NewLine output-string :from-end #t :start (- parent-character-index child-indentation) :end parent-character-index))
              (return (values child-index (- parent-character-index child-first-character-index (* child-line-index child-indentation)))))))))

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
         (output (text/make-text (as (concatenate 'vector
                                                  (awhen (and (output-delimiters-p projection)
                                                              (opening-delimiter-of input))
                                                    (elements-of it))
                                                  (elements-of (output-of (va content-iomap)))
                                                  (awhen (and (output-delimiters-p projection)
                                                              (closing-delimiter-of input))
                                                    (elements-of it))))
                                 :selection output-selection)))
    (make-iomap 'iomap/tree/leaf->text/text :input input :output output :content-iomap content-iomap)))

#+nil ;; TODO: delete old version
(def printer tree/node->text/text (projection recursion input input-reference)
  (bind ((pack (as (bind ((child-iomaps nil)
                          (child-indentations nil)
                          (child-first-character-indices nil)
                          (child-last-character-indices nil)
                          (output (text/text ())))
                     (labels ((push-string (string)
                                (bind ((last-element (unless (zerop (length (elements-of output))) (last-elt (elements-of output)))))
                                  (text/push output (text/text ()
                                                      (if (typep last-element 'text/string)
                                                          (text/string string :font (font-of last-element) :font-color (font-color-of last-element) :fill-color (fill-color-of last-element) :line-color (line-color-of last-element))
                                                          (text/string string :font *font/default* :font-color *color/default*))))))
                              (next-line (indentation)
                                (unless (zerop indentation)
                                  (push-string (make-string-of-spaces indentation)))))
                       (next-line 0)
                       (when input
                         (awhen (and (output-delimiters-p projection)
                                     (opening-delimiter-of input))
                           (text/push output it))
                         (iter (with children = (children-of input))
                               (for index :from 0)
                               (for child :in-sequence children)
                               (for indentation = (indentation-of child))
                               (unless (or (expanded-p input)
                                           (first-iteration-p))
                                 (finish))
                               (unless (first-iteration-p)
                                 (awhen (separator-of input)
                                   (text/push output it)))
                               (when indentation
                                 (push-string (string #\NewLine))
                                 (next-line indentation))
                               (bind ((child-iomap (recurse-printer recursion child
                                                                    `((elt (the sequence document) ,index)
                                                                      (the sequence (children-of (the tree/node document)))
                                                                      ,@(typed-reference (form-type input) input-reference))))
                                      ;; KLUDGE: replace with text API to avoid this slowness
                                      (child-indentation (or indentation (position #\NewLine (text/as-string output) :from-end #t) (text/length output)))
                                      (content (output-of child-iomap)))
                                 (push child-iomap child-iomaps)
                                 (push child-indentation child-indentations)
                                 (push (text/length output) child-first-character-indices)
                                 (bind ((index 0))
                                   (text/map-split content #\NewLine
                                                   (lambda (start-element-index start-character-index end-element-index end-character-index)
                                                     (when (> (incf index) 1)
                                                       (push-string (string #\NewLine))
                                                       (push-string (make-string-of-spaces child-indentation)))
                                                     (text/push output (text/substring content start-element-index start-character-index end-element-index end-character-index)))))
                                 (push (text/length output) child-last-character-indices)))
                         (unless (expanded-p input)
                           (push-string " ..."))
                         (awhen (and (output-delimiters-p projection)
                                     (closing-delimiter-of input))
                           (text/push output it)))
                       (next-line 0))
                     (list (text/consolidate output) (nreverse child-iomaps) (nreverse child-indentations) (nreverse child-first-character-indices) (nreverse child-last-character-indices)))))
         (output-selection (as (pattern-case (reverse (selection-of input))
                                 (((the tree/node document))
                                  `((the text/text (text/subbox (the text/text document) 0 ,(text/length (first (va pack)))))))
                                 (((the text/text (printer-output (the tree/node document) ?projection ?recursion)) . ?rest)
                                  (when (and (eq projection ?projection) (eq recursion ?recursion))
                                    (reverse ?rest)))
                                 (((the sequence (children-of (the tree/node document)))
                                   (the ?child-type (elt (the sequence document) ?child-index))
                                   . ?rest)
                                  (bind ((iomap (make-iomap 'iomap/tree/node->text/text
                                                            :child-iomaps (elt (va pack) 1)
                                                            :child-indentations (elt (va pack) 2)
                                                            :child-first-character-indices (elt (va pack) 3)
                                                            :child-last-character-indices (elt (va pack) 4)))
                                         (child-iomap (elt (child-iomaps-of iomap) ?child-index)))
                                    (pattern-case (selection-of (output-of child-iomap))
                                      (((the text/text (text/subseq (the text/text document) ?child-character-index ?child-character-index)))
                                       (bind ((character-index (tree-parent-character-index iomap ?child-index ?child-character-index)))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                      (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                       `((the text/text (text/subbox (the text/text document)
                                                                     ,(tree-parent-character-index iomap ?child-index ?start-character-index)
                                                                     ,(tree-parent-character-index iomap ?child-index ?end-character-index))))))))
                                 (?a
                                  (pattern-case (selection-of input)
                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                      (the text/text (opening-delimiter-of (the tree/node document))))
                                     `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                      (the text/text (closing-delimiter-of (the tree/node document))))
                                     (bind ((total-length (text/length (first (va pack))))
                                            (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0))
                                            (character-index (+ (- total-length closing-delimiter-length) ?character-index)))
                                       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))))
         (output (as (text/make-text (as (elements-of (first (va pack)))) :selection output-selection))))
    (make-iomap 'iomap/tree/node->text/text
                :input input :output output
                :child-iomaps (as (elt (va pack) 1))
                :child-indentations (as (elt (va pack) 2))
                :child-first-character-indices (as (elt (va pack) 3))
                :child-last-character-indices (as (elt (va pack) 4)))))

(def printer tree/node->text/text (projection recursion input input-reference)
  (bind ((output (text/make-text (append-ll (ll (append (awhen (opening-delimiter-of input)
                                                          (list (ll (elements-of it))))
                                                        (list (append-ll (bind ((indented-children (map-ll (ll (children-of input))
                                                                                                           (lambda (child)
                                                                                                             (bind ((child-iomap (recurse-printer recursion child nil))
                                                                                                                    (child-content (output-of child-iomap))
                                                                                                                    (child-elements (etypecase child-content
                                                                                                                                      (text/text (elements-of child-content))))
                                                                                                                    (indentation-elements (awhen (indentation-of child)
                                                                                                                                            (append (list (text/newline))
                                                                                                                                                    (unless (zerop it)
                                                                                                                                                      (list (text/string (make-string-of-spaces it))))))))
                                                                                                               (if indentation-elements
                                                                                                                   (append-ll (ll (list (ll indentation-elements)
                                                                                                                                        (append-ll (map-ll (ll child-elements)
                                                                                                                                                           (lambda (child-element)
                                                                                                                                                             (if (text/newline? child-element)
                                                                                                                                                                 (ll (list child-element (text/string (make-string-of-spaces (indentation-of child)))))
                                                                                                                                                                 (ll (list child-element))))))) 1))
                                                                                                                   (ll child-elements)))))))
                                                                           (aif (separator-of input)
                                                                                (separate-elements-ll indented-children (ll (elements-of it)))
                                                                                indented-children))))
                                                        (awhen (closing-delimiter-of input)
                                                          (list (ll (elements-of it)))))
                                                (if (opening-delimiter-of input) 1 0))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def function tree-reference? (reference)
  (pattern-case reference
    (((the ?type (?if (subtypep ?type 'tree/base)) ?a) . ?rest)
     #t)))

(def function text-reference? (reference)
  (pattern-case reference
    (((the text/text (text/subseq (the text/text document) ?b ?a)) . ?rest)
     #t)))

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
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection))
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
                            (bind ((total-length (text/length (output-of printer-iomap)))
                                   (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                   (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                              (cond ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                     (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                       (if child-index
                                           (bind ((child-iomap (elt (child-iomaps-of printer-iomap) child-index))
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
                                                                               (the text/text (closing-delimiter-of (the tree/node document)))))))))))))
                        (operation/sequence/replace-element-range
                         (awhen (pattern-case (target-of operation)
                                  (((the text/text (text/subseq (the text/text document) ?parent-character-index ?parent-character-index)) . ?rest)
                                   (bind ((total-length (text/length (output-of printer-iomap)))
                                          (opening-delimiter-length (aif (opening-delimiter-of printer-input) (text/length it) 0))
                                          (closing-delimiter-length (aif (closing-delimiter-of printer-input) (text/length it) 0)))
                                     (econd ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                             (bind (((:values child-index child-character-index) (tree-child-character-index printer-iomap ?parent-character-index)))
                                               (when child-index
                                                 (bind ((child-iomap (elt (child-iomaps-of printer-iomap) child-index))
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
                                           (bind ((child-iomap (elt (child-iomaps-of printer-iomap) start-child-index))
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
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection)))
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
