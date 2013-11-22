;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection tree/leaf->text ()
  ())

(def projection tree/node->text ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/tree/leaf->text ()
  (make-projection 'tree/leaf->text))

(def (function e) make-projection/tree/node->text ()
  (make-projection 'tree/node->text))

;;;;;;
;;; Construction

(def (macro e) tree/leaf->text ()
  `(make-projection/tree/leaf->text))

(def (macro e) tree/node->text ()
  `(make-projection/tree/node->text))

;;;;;;
;;; Printer

(def printer tree/leaf->text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (delimiter-iomaps nil)
         (content-iomaps nil)
         (output-content (bind ((string-position 0)
                                (element-index 0)
                                (elements nil))
                           (labels ((write-element (element)
                                      (push element elements)
                                      (incf element-index))
                                    (write-text/string (element)
                                      (write-element element)
                                      (incf string-position (length (content-of element))))
                                    (write-text/text (text)
                                      (iter (for element :in-sequence (elements-of text))
                                            (etypecase element
                                              (text/string
                                               (write-text/string element))
                                              (image/image
                                               (write-element element))))))
                             (if input
                                 (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                                   (awhen (opening-delimiter-of input)
                                     (push (make-iomap/text projection recursion
                                                            it `(opening-delimiter-of ,typed-input-reference) 0
                                                            (text/text ()) output-reference string-position
                                                            (text/length it))
                                           delimiter-iomaps)
                                     (write-text/text it))
                                   (bind ((content (content-of input)))
                                     (etypecase content
                                       (text/string
                                        (push (make-iomap/string (content-of content) `(content-of (the text/string (content-of ,typed-input-reference))) 0
                                                                 (content-of content) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                 (length (content-of content)))
                                              content-iomaps)
                                        (write-text/string content))
                                       (text/text
                                           (push (make-iomap/text projection recursion
                                                                  content `(content-of ,typed-input-reference) 0
                                                                  (text/text ()) output-reference string-position
                                                                  (text/length content))
                                                 content-iomaps)
                                         (write-text/text content))
                                       (image/image
                                        (write-element content))))
                                   (awhen (closing-delimiter-of input)
                                     (push (make-iomap/text projection recursion
                                                            it `(closing-delimiter-of ,typed-input-reference) 0
                                                            (text/text ()) output-reference string-position
                                                            (text/length it))
                                           delimiter-iomaps)
                                     (write-text/text it)))
                                 (push (make-iomap/object* projection recursion
                                                           input `(the null ,input-reference)
                                                           nil `(the sequence-position (text/pos (the text/text ,output-reference) 0)))
                                       content-iomaps))
                             (nreverse elements))))
         (output (make-text/text output-content)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         ;; KLUDGE: use a separate iomap kind for box
                         (append (list (make-iomap/object* projection recursion input typed-input-reference output `(the sequence-box (text/subbox (the text/text ,output-reference) 0 ,(text/length output)))))
                                 (nreverse delimiter-iomaps)
                                 (nreverse content-iomaps)))))

(def printer tree/node->text (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (delimiter-iomaps nil)
         (separator-iomaps nil)
         (indentation-iomaps nil)
         (child-iomaps nil)
         (output-content (bind ((line-index nil)
                                (string-position 0)
                                (element-index 0)
                                (line-position 0)
                                (elements nil))
                           (labels ((write-element (element)
                                      (push element elements)
                                      (incf element-index))
                                    (write-text/string (element)
                                      (write-element element)
                                      (incf string-position (length (content-of element))))
                                    (write-text/text (text)
                                      (iter (for element :in-sequence (elements-of text))
                                            (etypecase element
                                              (text/string
                                               (write-text/string element))
                                              (image/image
                                               (write-element element)))))
                                    (next-line (indentation typed-input-reference)
                                      (when line-index
                                        (bind ((content (text/text () (text/string (string #\NewLine) :font *font/default* :font-color *color/default*))))
                                          (push (make-iomap/text projection recursion
                                                                 content `(new-line ,typed-input-reference) 0
                                                                 (text/text ()) output-reference string-position
                                                                 1)
                                                indentation-iomaps)
                                          (write-text/text content))
                                        (setf line-position string-position))
                                      (bind ((indentation-string (make-string-of-spaces indentation))
                                             (indentation-text (text/text () (make-text/string indentation-string :font *font/default* :font-color *color/default*))))
                                        (unless (string= indentation-string "")
                                          (push (make-iomap/text projection recursion
                                                                 indentation-text `(indentation ,typed-input-reference ,indentation-string) 0
                                                                 (text/text ()) output-reference string-position
                                                                 indentation)
                                                indentation-iomaps)
                                          (write-text/text indentation-text)))
                                      (if line-index
                                          (incf line-index)
                                          (setf line-index 0))))
                             (next-line 0 typed-input-reference)
                             (if input
                                 (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                                   (awhen (opening-delimiter-of input)
                                     (push (make-iomap/text projection recursion
                                                            it `(opening-delimiter-of ,typed-input-reference) 0
                                                            (text/text ()) output-reference string-position
                                                            (text/length it))
                                           delimiter-iomaps)
                                     (write-text/text it))
                                   (if (expanded-p input)
                                       (iter (with children = (children-of input))
                                             (for index :from 0)
                                             (for child :in-sequence children)
                                             (for child-path = `(elt (the ,(form-type children) (children-of ,typed-input-reference)) ,index))
                                             (for child-reference = `(the ,(form-type child) ,child-path))
                                             (for previous-child-reference :previous child-reference)
                                             (for indentation = (indentation-of child))
                                             (unless (first-iteration-p)
                                               (awhen (separator-of input)
                                                 ;; KLUDGE: unfortunatly iomap/object of later stages can't substitute both arguments of separator, so backward/forward mapping won't work this way
                                                 (push (make-iomap/text projection recursion
                                                                        it `(separator nil #+nil ,previous-child-reference ,child-reference) 0
                                                                        (text/text ()) output-reference string-position
                                                                        (text/length it))
                                                       separator-iomaps)
                                                 (write-text/text it)))
                                             (when indentation
                                               (next-line indentation child-reference))
                                             (bind ((child-iomap (recurse-printer recursion iomap child child-path output-reference))
                                                    (content (output-of child-iomap))
                                                    (string-content (text/as-string (output-of child-iomap)))
                                                    (child-indentation (- string-position line-position)))
                                               (labels ((recurse (iomap)
                                                          ;; KLUDGE: shifting
                                                          (labels ((incifinci (index)
                                                                     (bind ((newline-count (funcall 'count #\NewLine string-content :end index)))
                                                                       (+ index string-position (* newline-count child-indentation)))))
                                                            (etypecase iomap
                                                              (iomap/object
                                                               ;; KLUDGE: make it a separate iomap kind
                                                               (setf (elt (elt (output-reference-of iomap) 2) 2) (incifinci (elt (elt (output-reference-of iomap) 2) 2)))
                                                               (setf (elt (elt (output-reference-of iomap) 2) 3) (incifinci (elt (elt (output-reference-of iomap) 2) 3))))
                                                              (iomap/text
                                                               (setf (output-offset-of iomap) (incifinci (output-offset-of iomap))))
                                                              (iomap/compound
                                                               (iter (for child-iomap :in-sequence (child-iomaps-of iomap))
                                                                     (recurse child-iomap)))))))
                                                 (recurse child-iomap))
                                               (etypecase content
                                                 (text/text
                                                     (bind ((index 0))
                                                       (text/map-split content #\NewLine
                                                                       (lambda (start-element-index start-character-index end-element-index end-character-index)
                                                                         (when (> (incf index) 1)
                                                                           (write-element (text/newline))
                                                                           (write-text/string (make-text/string (make-string-of-spaces (or (indentation-of child) child-indentation)) :font *font/default* :font-color *color/default*)))
                                                                         (write-text/text (text/substring content start-element-index start-character-index end-element-index end-character-index)))))
                                                   ;;(write-text/text content)
                                                   (push child-iomap child-iomaps))
                                                 (image/image
                                                  (write-element content))))
                                             (finally
                                              ;; TODO: this caused indentation problems in JSON, XML ... delete?
                                              #+nil
                                              (when-bind indentation (indentation-of input)
                                                (next-line indentation previous-child-reference))))
                                       (write-text/string (make-text/string "..." :font *font/default* :font-color *color/default*)))
                                   (awhen (closing-delimiter-of input)
                                     (push (make-iomap/text projection recursion
                                                            it `(closing-delimiter-of ,typed-input-reference) 0
                                                            (text/text ()) output-reference string-position
                                                            (text/length it))
                                           delimiter-iomaps)
                                     (write-text/text it)))
                                 (push (make-iomap/object* projection recursion
                                                           input `(the null ,input-reference)
                                                           nil `(the sequence-position (text/pos (the text/text ,output-reference) 0)))
                                       child-iomaps))
                             (setf line-index nil)
                             (next-line 0 typed-input-reference)
                             (nreverse elements))))
         (output (make-text/text output-content)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         ;; KLUDGE: use a separate iomap kind for box
                         (append (list (make-iomap/object* projection recursion input typed-input-reference output `(the sequence-box (text/subbox (the text/text ,output-reference) 0 ,(text/length output)))))
                                 (nreverse indentation-iomaps)
                                 (nreverse separator-iomaps)
                                 (nreverse delimiter-iomaps)
                                 (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def function tree-reference? (reference)
  (pattern-case reference
    ((the ?type (?if (subtypep ?type 'tree/base)) ?a)
     #t)))

(def function text-reference? (reference)
  (pattern-case reference
    ((the sequence-position (text/pos (the text/text ?a) ?b))
     #t)
    ((the character (elt (the string ?a) ?b))
     #t)
    ((the sequence-position (text/pos (the text/text ?a) ?b))
     #t)
    ((the character (text/elt (the text/text ?a) ?b))
     #t)))

(def function find-tree-node-reference (reference)
  (pattern-case reference
    ((the sequence-position ((?or pos text/pos) (the (?or string text/text) (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                                                                 ((?or opening-delimiter-of closing-delimiter-of) (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                                                                 (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                                                            (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                                                                 (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b))) ?c))
     `(the ,?type ,?a))
    ((the sequence-position ((?or pos text/pos) (the (?or string text/text) (content-of (the text/string (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                                                                                              ((?or opening-delimiter-of closing-delimiter-of) (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                                                                                              (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                                                                                         (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                                                                                              (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b))))) ?c))
     `(the ,?type ,?a))
    ((the ?type (?if (subtypep ?type 'tree/base)) ?a)
     `(the ,?type ,?a))))

(def function find-tree-node-parent-reference (reference)
  (pattern-case reference
    ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
     `(the tree/node ,?a))))

(def reader tree/leaf->text (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (document (input-of document-iomap))
         (selection (selection-of document))
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection))
         (parent-operation (operation/read-backward operation projection-iomap document-iomap)))
    (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a text selection"
                         :operation (when tree-selection?
                                      (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of ,selection)) 0)))))
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a tree selection"
                         :operation (when text-selection?
                                      (make-operation/replace-selection document (find-tree-node-reference selection))))
                        ((gesture/keyboard/key-press :sdl-key-home :alt)
                         :domain "Tree" :help "Moves the selection to the root node"
                         :operation (bind ((new-selection `(the ,(form-type (content-of document)) (content-of (the document document)))))
                                      (unless (equal new-selection (selection-of document))
                                        (make-operation/replace-selection document new-selection)))))
                      parent-operation)))

(def reader tree/node->text (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (document (input-of document-iomap))
         (selection (selection-of document))
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection))
         (parent-operation (operation/read-backward operation projection-iomap document-iomap)))
    (or (merge-operations (gesture-case latest-gesture
                            ((gesture/keyboard/key-press :sdl-key-space :control)
                             :domain "Tree" :help "Turn the selection into a text selection"
                             :operation (when tree-selection?
                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of ,selection)) 0)))))
                            ((gesture/keyboard/key-press :sdl-key-space :control)
                             :domain "Tree" :help "Turn the selection into a tree selection"
                             :operation (when text-selection?
                                          (make-operation/replace-selection document (find-tree-node-reference selection))))
                            ((gesture/keyboard/key-press :sdl-key-home :alt)
                             :domain "Tree" :help "Moves the selection to the root node"
                             :operation (bind ((new-selection `(the ,(form-type (content-of document)) (content-of (the document document)))))
                                          (unless (equal new-selection (selection-of document))
                                            (make-operation/replace-selection document new-selection))))
                            ((gesture/keyboard/key-press :sdl-key-up)
                             :domain "Tree" :help "Moves the selection to the parent node"
                             :operation (when tree-selection?
                                          (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                            (make-operation/replace-selection document node-reference))))
                            ((gesture/keyboard/key-press :sdl-key-down)
                             :domain "Tree" :help "Moves the selection to the first child node"
                             :operation (when tree-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (bind ((node (eval-reference document node-reference)))
                                              (when (and (typep node 'tree/node)
                                                         (> (length (children-of node)) 0))
                                                (bind ((first-child (elt (children-of node) 0)))
                                                  (make-operation/replace-selection document `(the ,(form-type first-child) (elt (the list (children-of ,node-reference)) 0)))))))))
                            ((gesture/keyboard/key-press :sdl-key-left)
                             :domain "Tree" :help "Moves the selection to the preceding sibling node"
                             :operation (when tree-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (pattern-case node-reference
                                              ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                               (when (> ?b 0)
                                                 (bind ((parent-node (eval-reference document ?a))
                                                        (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                                   (make-operation/replace-selection document `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b)))))))))))
                            ((gesture/keyboard/key-press :sdl-key-right)
                             :domain "Tree" :help "Moves the selection to the following sibling node"
                             :operation (when tree-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (pattern-case node-reference
                                              ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                               (bind ((parent-node (eval-reference document ?a)))
                                                 (when (< ?b (1- (length (children-of parent-node))))
                                                   (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                     (make-operation/replace-selection document `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))))))))))
                            ((gesture/keyboard/key-press :sdl-key-up :alt)
                             :domain "Tree" :help "Moves the selection to the first character of the parent node"
                             :operation (when text-selection?
                                          (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                            (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (opening-delimiter-of ,node-reference)) 0))))))
                            ((gesture/keyboard/key-press :sdl-key-down :alt)
                             :domain "Tree" :help "Moves the selection to the first character of the first child node"
                             :operation (when text-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (bind ((node (eval-reference document node-reference)))
                                              (when (and (typep node 'tree/node)
                                                         (> (length (children-of node)) 0))
                                                (bind ((first-child (elt (children-of node) 0)))
                                                  (make-operation/replace-selection document
                                                                                    (etypecase first-child
                                                                                      (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of ,node-reference)) 0)))) 0)))
                                                                                      (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the tree/node (elt (the list (children-of ,node-reference)) 0)))) 0)))))))))))
                            ((gesture/keyboard/key-press :sdl-key-left :alt)
                             :domain "Tree" :help "Moves the selection to the first character of the preceding sibling node"
                             :operation (when text-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (pattern-case node-reference
                                              ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                               (when (> ?b 0)
                                                 (bind ((parent-node (eval-reference document ?a))
                                                        (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                                   (make-operation/replace-selection document
                                                                                     (ecase type
                                                                                       (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0)))
                                                                                       (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0))))))))))))
                            ((gesture/keyboard/key-press :sdl-key-right :alt)
                             :domain "Tree" :help "Moves the selection to the first character of the following sibling node"
                             :operation (when text-selection?
                                          (when-bind node-reference (find-tree-node-reference selection)
                                            (pattern-case node-reference
                                              ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                               (bind ((parent-node (eval-reference document ?a)))
                                                 (when (< ?b (1- (length (children-of parent-node))))
                                                   (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                     (make-operation/replace-selection document
                                                                                       (ecase type
                                                                                         (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0)))
                                                                                         (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0))))))))))))))
                          parent-operation)
        ;; TODO: make gesture-case
        (cond ((text-reference? selection)
               (cond ((typep operation 'operation/sequence/replace-element-range)
                      (bind ((tree-reference (map-backward/clever (target-of operation) document-iomap)))
                        (pattern-case tree-reference
                          ((the sequence (subseq (the string (content-of (the text/string (opening-delimiter-of (the tree/node (elt ?a ?b)))))) 0 1))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)) nil)
                                                          (make-operation/replace-selection document nil))))
                          (?a
                           (make-operation/sequence/replace-element-range document (tree-replace tree-reference `(the document ,(input-reference-of document-iomap)) '(the document document)) nil)))))
                     ((key-press? latest-gesture :key :sdl-key-delete :modifier :control)
                      (when-bind node-reference (find-tree-node-reference selection)
                        (pattern-case node-reference
                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt ?a ?b))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                         nil)
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ,?a ,?b)))) 0))))))
                          ((the ?type (?if (subtypep ?type 'tree/base)) (content-of (the document ?a)))
                           (make-operation/compound (list (make-operation/replace-content document nil)
                                                          (make-operation/replace-selection document `(the null (content-of (the document ,?a))))))))))
                     ((key-press? latest-gesture :character #\( :modifiers '(:shift :control))
                      (pattern-case selection
                        ;; content of a leaf being a child of a node
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ?a ?b)))) ?c))
                         (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                       (list (make-tree/node nil)))
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the tree/node (elt ,?a ,?b)))) 1))))))
                        ;; empty content of a document
                        ((the null (content-of (the document ?a)))
                         (make-operation/compound (list (make-operation/replace-content document (make-tree/node nil))
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the tree/node (content-of (the document document))))) 1))))))))
                     ((key-press? latest-gesture :character #\" :modifiers '(:shift :control))
                      (pattern-case selection
                        ;; content of a leaf being a child of a node
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ?a ?b)))) ?c))
                         (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                       (list (make-tree/leaf "")))
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ,?a ,?b)))) 0))))))
                        ;; delimiter of a node
                        ((the sequence-position (text/pos (the text/text (opening-delimiter-of (the tree/node ?a))) ?b))
                         (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq (the list (children-of (the tree/node ,?a))) 0 0)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                       (list (make-tree/leaf "")))
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,?a))) 0)))) 0))))))
                        ;; empty content of a document
                        ((the null (content-of (the document ?a)))
                         (make-operation/compound (list (make-operation/replace-content document (make-tree/leaf ""))
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (content-of (the document document))))) 0))))))))
                     ;; move leaf/node up
                     ((key-press? latest-gesture :key :sdl-key-u :modifier :control)
                      (pattern-case selection
                        ;; content of a leaf being a child of a node that is a child of another node
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)))) ?d))
                         (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                         (list value))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ,?a ,?b)))) ,?d)))))))
                        ;; delimiter of a leaf/node being a child of a node that is a child of another node
                        ((the sequence-position (text/pos (the text/text (?delimiter (?if (member ?delimiter '(opening-delimiter-of closing-delimiter-of))) (the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)))) ?e))
                         (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                         (list value))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (,?delimiter (the ,?type (elt ,?a ,?b)))) ,?e)))))))
                        ;; TODO: indentation of a leaf/node being a child of a node that is a child of another node
                        ;; TODO: content of a leaf being a child of a node that is the content of a document
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node (content-of (the document ?a))))) ?b)))) ?c))
                         (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (content-of (the document ,?a))))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/replace-content document value)
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (content-of (the document ,?a))))) ,?c)))))))
                        ;; TODO: delimiter of a leaf/node being a child of a node that is the content of a document
                        ((the sequence-position (text/pos (the text/text (?delimiter (?if (member ?delimiter '(opening-delimiter-of closing-delimiter-of))) (the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node (content-of (the document ?a))))) ?b)))) ?d))
                         (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (content-of (the document ,?a))))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/replace-content document value)
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (,?delimiter (the ,?type (content-of (the document ,?a))))) ,?d)))))))
                        ;; TODO: indentation of a leaf/node being a child of a node that is the content of a document
                        ))
                     ;; wrap leaf/node
                     ((key-press? latest-gesture :key :sdl-key-w :modifier :control)
                      (pattern-case selection
                        ;; content of a leaf being a child of a node
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ?a ?b)))) ?c))
                         (bind ((value (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                         (list (make-tree/node (list value))))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt ,?a ,?b)))) 0)))) ,?c)))))))
                        ;; delimiter of a leaf/node being a child of a node
                        ((the sequence-position (text/pos (the text/text (?delimiter (?if (member ?delimiter '(opening-delimiter-of closing-delimiter-of))) (the ?type (?if (subtypep ?type 'tree/base)) (elt ?a ?b)))) ?d))
                         (bind ((value (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                         (list (make-tree/node (list value))))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (,?delimiter (the ,?type (elt (the list (children-of (the tree/node (elt ,?a ,?b)))) 0)))) ,?d)))))))
                        ;; TODO: indentation of a leaf/node being a child of a node
                        ;; content of a leaf being the content of a document
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (content-of (the document ?a))))) ?b))
                         (bind ((value (eval-reference document (tree-replace `(content-of (the document ,?a)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/replace-content document (make-tree/node (list value)))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node (content-of (the document ,?a))))) 0)))) ,?b)))))))
                        ;; delimiter of a leaf/node being the content of a document
                        ((the sequence-position (text/pos (the text/text (?delimiter (?if (member ?delimiter '(opening-delimiter-of closing-delimiter-of))) (the ?type (?if (subtypep ?type 'tree/base)) (content-of (the document ?a))))) ?c))
                         (bind ((value (eval-reference document (tree-replace `(content-of (the document ,?a)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                           (make-operation/compound (list (make-operation/replace-content document (make-tree/node (list value)))
                                                          (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (,?delimiter (the ,?type (elt (the list (children-of (the tree/node (content-of (the document ,?a))))) 0)))) ,?c)))))))
                        ;; TODO: indentation of a leaf/node being the content of a document
                        ))
                     ;; transpose
                     ((key-press? latest-gesture :key :sdl-key-t :modifier :control)
                      (pattern-case selection
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node ?a))) ?b)))) ?c))
                         (when (< ?b (1- (length (children-of (eval-reference document ?a)))))
                           (bind ((value-1 (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node ,?a))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))))
                                  (value-2 (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                             (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq (the list (children-of (the tree/node ,?a))) ,?b ,(+ ?b 2))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                           (list value-2 value-1))
                                                            (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) ,?c))))))))))
                     ;; reverse transpose
                     ((key-press? latest-gesture :key :sdl-key-t :modifiers '(:control :shift))
                      (pattern-case selection
                        ((the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ?a ?b)))) ?c))
                         (when (> ?b 0)
                           (bind ((value-1 (eval-reference document (tree-replace `(elt ,?a ,(1- ?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))))
                                  (value-2 (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                             (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,(- ?b 1) ,(+ ?b 1))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                           (list value-2 value-1))
                                                            (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text (content-of (the tree/leaf (elt ,?a ,(1- ?b))))) ,?c))))))))))
                     ;; collapse/expand
                     ((key-press? latest-gesture :key :sdl-key-tab :modifier :control)
                      (awhen (find-tree-node-parent-reference (find-tree-node-reference selection))
                        (make-operation/tree/toggle-node document it)))
                     (t
                      parent-operation)))
              (t
               parent-operation)))))
