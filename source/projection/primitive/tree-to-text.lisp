;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/tree/leaf->text/text (iomap)
  ((content-iomap :type iomap)))

(def iomap iomap/tree/node->text/text (iomap)
  ((child-iomaps :type sequence)
   (child-indentations :type sequence)
   (child-first-character-indices :type sequence)
   (child-last-character-indices :type sequence)))

;;;;;;
;;; Reference applier

(def reference-applier iomap/tree/leaf->text/text (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

(def reference-applier iomap/tree/node->text/text (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

;;;;;;
;;; Forward mapper

(def function tree-parent-character-index (iomap child-index child-character-index)
  (bind ((child-indentation (elt (child-indentations-of iomap) child-index))
         (child-first-character-index (elt (child-first-character-indices-of iomap) child-index))
         (child-line-index (count #\NewLine (text/as-string (output-of (elt (child-iomaps-of iomap) child-index))) :end child-character-index)))
    (+ (* child-line-index child-indentation) child-first-character-index child-character-index)))

(def forward-mapper iomap/tree/leaf->text/text (iomap input-reference function)
  (pattern-case (reverse input-reference)
    (((the ?content-type (content-of (the tree/leaf document)))
      . ?rest)
     (map-forward (content-iomap-of iomap)
                  (reverse ?rest)
                  (lambda (content-iomap output-reference)
                    (pattern-case output-reference
                      (((the sequence-position (text/pos (the text/text document) ?content-character-index)) . ?rest)
                       (bind ((opening-delimiter-length (aif (opening-delimiter-of (input-of iomap)) (text/length it) 0)))
                         (funcall function content-iomap `((the sequence-position (text/pos (the text/text document) ,(+ opening-delimiter-length ?content-character-index)))))))))))
    (?a
     (pattern-case input-reference
       (((the sequence-position (text/pos (the text/text document) ?character-index))
         (the text/text (opening-delimiter-of (the tree/leaf document))))
        (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,?character-index)))))
       (((the sequence-position (text/pos (the text/text document) ?character-index))
         (the text/text (closing-delimiter-of (the tree/leaf document))))
        (bind ((input (input-of iomap))
               (content-length (text/length (output-of (content-iomap-of iomap))))
               (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0)))
          (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,(+ ?character-index opening-delimiter-length content-length)))))))))))

(def forward-mapper iomap/tree/node->text/text (iomap input-reference function)
  (error "Obsolete")
  #+nil
  (pattern-case (reverse input-reference)
    (((the list (children-of (the tree/node document)))
      (the ?child-type (elt (the list document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of iomap) ?child-index)))
       (if ?rest
           (map-forward child-iomap
                        (reverse ?rest)
                        (lambda (child-iomap output-reference)
                          (declare (ignore child-iomap))
                          (pattern-case output-reference
                            (((the sequence-position (text/pos (the text/text document) ?child-character-index)))
                             (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,(tree-parent-character-index iomap ?child-index ?child-character-index))))))
                            (((the sequence-box (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                             (funcall function iomap `((the sequence-box (text/subbox (the text/text document)
                                                                                      ,(tree-parent-character-index iomap ?child-index ?start-character-index)
                                                                                      ,(tree-parent-character-index iomap ?child-index ?end-character-index)))))))))
           (funcall function iomap `((the sequence-box (text/subbox (the text/text document)
                                                                    ,(tree-parent-character-index iomap ?child-index 0)
                                                                    ,(tree-parent-character-index iomap ?child-index (text/length (output-of child-iomap))))))))))
    (?a
     (pattern-case input-reference
       (((the sequence-position (text/pos (the text/text document) ?character-index))
         (the text/text (opening-delimiter-of (the tree/node document))))
        (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,?character-index)))))
       (((the sequence-position (text/pos (the text/text document) ?character-index))
         (the text/text (closing-delimiter-of (the tree/node document))))
        (bind ((input (input-of iomap))
               (total-length (text/length (output-of iomap)))
               (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
          (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,(+ (- total-length closing-delimiter-length) ?character-index)))))))))))

;;;;;;
;;; Backward mapper

(def function tree-child-character-index (iomap parent-character-index)
  (iter (with count = (length (children-of (input-of iomap))))
        (for child-index :from 0 :below count)
        (for child-first-character-index = (elt (child-first-character-indices-of iomap) child-index))
        (for child-last-character-index = (elt (child-last-character-indices-of iomap) child-index))
        (for child-indentation = (elt (child-indentations-of iomap) child-index))
        (when (<= child-first-character-index parent-character-index child-last-character-index)
          (bind ((child-line-index (funcall 'count #\NewLine (text/as-string (output-of iomap)) :start child-first-character-index :end parent-character-index)))
            (return (values child-index (- parent-character-index child-first-character-index (* child-line-index child-indentation))))))))

(def backward-mapper iomap/tree/leaf->text/text (iomap output-reference function)
  (pattern-case output-reference
    (((the sequence-position (text/pos (the text/text document) ?character-index)) . ?rest)
     (bind ((input (input-of iomap))
            (content-iomap (content-iomap-of iomap))
            (content (output-of content-iomap))
            (content-length (text/length content))
            (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
            (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
       (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
               (map-backward content-iomap
                             `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length))))
                             (lambda (content-iomap input-reference)
                               (funcall function iomap `(,@input-reference
                                                         (the ,(form-type content) (content-of (the tree/leaf document))))))))
              ((<= ?character-index opening-delimiter-length)
               (funcall function content-iomap `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                                 (the text/text (opening-delimiter-of (the tree/leaf document))))))
              ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
               (funcall function content-iomap `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length content-length)))
                                                 (the text/text (closing-delimiter-of (the tree/leaf document)))))))))))

(def backward-mapper iomap/tree/node->text/text (iomap output-reference function)
  (pattern-case output-reference
    (((the sequence-position (text/pos (the text/text document) ?parent-character-index)) . ?rest)
     (bind ((input (input-of iomap))
            (total-length (text/length (output-of iomap)))
            (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
            (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
       (econd ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
               (bind (((:values child-index child-character-index) (tree-child-character-index iomap ?parent-character-index))
                      (child-iomap (elt (child-iomaps-of iomap) child-index))
                      (child (input-of child-iomap)))
                 (map-backward child-iomap
                               `((the sequence-position (text/pos (the text/text document) ,child-character-index)))
                               (lambda (child-iomap input-reference)
                                 (funcall function iomap `(,@input-reference
                                                           (the ,(form-type child) (elt (the list document) ,child-index))
                                                           (the list (children-of (the tree/node document)))))))))
              ((<= 0 ?parent-character-index opening-delimiter-length)
               (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,?parent-character-index))
                                         (the text/text (opening-delimiter-of (the tree/node document))))))
              ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
               (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,(+ (- ?parent-character-index total-length) opening-delimiter-length)))
                                         (the text/text (closing-delimiter-of (the tree/node document)))))))))))

;;;;;;
;;; Projection

(def projection tree/leaf->text/text ()
  ())

(def projection tree/node->text/text ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/tree/leaf->text/text ()
  (make-projection 'tree/leaf->text/text))

(def (function e) make-projection/tree/node->text/text ()
  (make-projection 'tree/node->text/text))

;;;;;;
;;; Construction

(def (macro e) tree/leaf->text/text ()
  `(make-projection/tree/leaf->text/text))

(def (macro e) tree/node->text/text ()
  `(make-projection/tree/node->text/text))

;;;;;;
;;; Printer

(def printer tree/leaf->text/text (projection recursion input input-reference)
  (declare (ignore projection))
  (bind ((content-iomap (recurse-printer recursion (content-of input)
                                         `((content-of (the tree/leaf document))
                                           ,@(typed-reference (form-type input) input-reference))))
         (content-output (output-of content-iomap))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the ?content-type (content-of (the tree/leaf document)))
                               . ?rest)
                              (pattern-case (selection-of content-output)
                                (((the sequence-position (text/pos (the text/text document) ?index)))
                                 `((the sequence-position (text/pos (the text/text document) ,(+ ?index
                                                                                                 (aif (opening-delimiter-of input)
                                                                                                      (text/length it)
                                                                                                      0))))))))
                             (?a
                              (pattern-case (selection-of input)
                                (((the sequence-position (text/pos (the text/text document) ?character-index))
                                  (the text/text (opening-delimiter-of (the tree/leaf document))))
                                 `((the sequence-position (text/pos (the text/text document) ,?character-index))))
                                (((the sequence-position (text/pos (the text/text document) ?character-index))
                                  (the text/text (closing-delimiter-of (the tree/leaf document))))
                                 (bind ((content-length (text/length (output-of content-iomap)))
                                        (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0)))
                                   `((the sequence-position (text/pos (the text/text document) ,(+ ?character-index opening-delimiter-length content-length))))))))))
         (output (make-text/text (concatenate 'vector
                                              (awhen (opening-delimiter-of input)
                                                (elements-of it))
                                              (elements-of content-output)
                                              (awhen (closing-delimiter-of input)
                                                (elements-of it)))
                                 :selection output-selection)))
    (make-iomap 'iomap/tree/leaf->text/text :input input :output output :content-iomap content-iomap)))

(def printer tree/node->text/text (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (child-indentations nil)
         (child-first-character-indices nil)
         (child-last-character-indices nil)
         (output (text/text ()))
         (line-index nil)
         (string-position 0)
         (line-position 0))
    (labels ((next-line (indentation)
               (when line-index
                 (text/push output (text/text () (text/newline)))
                 (setf line-position string-position))
               (bind ((indentation-string (make-string-of-spaces indentation))
                      (indentation-text (text/text () (text/string indentation-string :font *font/default* :font-color *color/default*))))
                 (unless (string= indentation-string "")
                   (text/push output indentation-text)))
               (if line-index
                   (incf line-index)
                   (setf line-index 0))))
      (next-line 0)
      (when input
        (awhen (opening-delimiter-of input)
          (text/push output it))
        (if (expanded-p input)
            (iter (with children = (children-of input))
                  (for index :from 0)
                  (for child :in-sequence children)
                  (for indentation = (indentation-of child))
                  (unless (or indentation (first-iteration-p))
                    (awhen (separator-of input)
                      (text/push output it)))
                  (when indentation
                    (next-line indentation))
                  (bind ((child-iomap (recurse-printer recursion child
                                                       `((elt (the list document) ,index)
                                                         (the list (children-of (the tree/node document)))
                                                         ,@(typed-reference (form-type input) input-reference))))
                         (child-indentation (or (indentation-of child) 0))
                         (content (output-of child-iomap)))
                    (push child-iomap child-iomaps)
                    (push child-indentation child-indentations)
                    (push (text/length output) child-first-character-indices)
                    (bind ((index 0))
                      (text/map-split content #\NewLine
                                      (lambda (start-element-index start-character-index end-element-index end-character-index)
                                        (when (> (incf index) 1)
                                          (text/push output (text/text () (text/newline)))
                                          (text/push output (text/text () (text/string (make-string-of-spaces child-indentation) :font *font/default* :font-color *color/default*))))
                                        (text/push output (text/substring content start-element-index start-character-index end-element-index end-character-index)))))
                    (push (text/length output) child-last-character-indices)))
            (text/push (text/text () (text/string "..." :font *font/default* :font-color *color/default*))))
        (awhen (closing-delimiter-of input)
          (text/push output it)))
      (setf line-index nil)
      (next-line 0))
    ;; TODO: move iomap to return value, make more functional style
    (bind ((iomap (make-iomap 'iomap/tree/node->text/text
                              :input input :output output
                              :child-iomaps (nreverse child-iomaps)
                              :child-indentations (nreverse child-indentations)
                              :child-first-character-indices (nreverse child-first-character-indices)
                              :child-last-character-indices (nreverse child-last-character-indices)))
           (output-selection (pattern-case (reverse (selection-of input))
                               (((the text/text (printer-output (the tree/node document) ?projection ?recursion)) . ?rest)
                                (when (and (eq projection ?projection) (eq recursion ?recursion))
                                  (reverse ?rest)))
                               (((the list (children-of (the tree/node document)))
                                 (the ?child-type (elt (the list document) ?child-index))
                                 . ?rest)
                                (bind ((child-iomap (elt (child-iomaps-of iomap) ?child-index)))
                                  (if ?rest
                                      (pattern-case (selection-of (output-of child-iomap))
                                        (((the sequence-position (text/pos (the text/text document) ?child-character-index)))
                                         `((the sequence-position (text/pos (the text/text document) ,(tree-parent-character-index iomap ?child-index ?child-character-index))))))
                                      `((the sequence-box (text/subbox (the text/text document)
                                                                       ,(tree-parent-character-index iomap ?child-index 0)
                                                                       ,(tree-parent-character-index iomap ?child-index (text/length (output-of child-iomap)))))))))
                               (?a
                                (pattern-case (selection-of input)
                                  (((the sequence-position (text/pos (the text/text document) ?character-index))
                                    (the text/text (opening-delimiter-of (the tree/node document))))
                                   `((the sequence-position (text/pos (the text/text document) ,?character-index))))
                                  (((the sequence-position (text/pos (the text/text document) ?character-index))
                                    (the text/text (closing-delimiter-of (the tree/node document))))
                                   (bind ((total-length (text/length output))
                                          (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                     `((the sequence-position (text/pos (the text/text document) ,(+ (- total-length closing-delimiter-length) ?character-index)))))))))))
      (setf (selection-of output) output-selection)
      iomap)))

;;;;;;
;;; Reader

(def function tree-reference? (reference)
  (pattern-case reference
    (((the ?type (?if (subtypep ?type 'tree/base)) ?a) . ?rest)
     #t)))

(def function text-reference? (reference)
  (pattern-case reference
    (((the sequence-position (text/pos (the text/text document) ?b)) . ?rest)
     #t)
    (((the character (elt (the string document) ?b)) . ?rest)
     #t)
    (((the sequence-position (text/pos (the text/text document) ?b)) . ?rest)
     #t)
    (((the character (text/elt (the text/text document) ?b)) . ?rest)
     #t)))

(def function find-tree-node-reference (reference)
  (pattern-case reference
    (((the sequence-position ((?or pos text/pos) (the (?or string text/text) document) ?c))
      (the (?or string text/text) (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                       ((?or opening-delimiter-of closing-delimiter-of) (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                       (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                  (the ?type (?if (subtypep ?type 'tree/base)) ?a))
                                       (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)))
      . ?rest)
     ?rest)
    (((the sequence-position ((?or pos text/pos) (the (?or string text/text) document) ?c))
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
    (((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list document) ?b))
      (the list (children-of (the tree/node document)))
      . ?rest)
     ?rest)))

(def reader tree/leaf->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (input (input-of projection-iomap))
         (content-iomap (content-iomap-of projection-iomap))
         (selection (selection-of input))
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection))
         (content-operation (recurse-reader recursion content-iomap gesture-queue operation))
         (document-operation (document/read-operation input latest-gesture)))
    (merge-operations (labels ((recurse (operation)
                                 (typecase operation
                                   (operation/replace-selection
                                    (make-operation/replace-selection input (pattern-case (selection-of operation)
                                                                              (((the sequence-position (text/pos (the text/text document) ?character-index)) . ?rest)
                                                                               (bind ((content (output-of content-iomap))
                                                                                      (content-length (text/length content))
                                                                                      (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                                                                      (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                                                                 (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
                                                                                         `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length)))
                                                                                           (the text/text (content-of (the tree/leaf document)))))
                                                                                        ((<= ?character-index opening-delimiter-length)
                                                                                         `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                                                                           (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                                                        ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                                                                         `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length content-length)))
                                                                                           (the text/text (closing-delimiter-of (the tree/leaf document)))))))))))
                                   (operation/sequence/replace-element-range
                                    (make-operation/sequence/replace-element-range input (pattern-case (target-of operation)
                                                                                           (((the sequence-position (text/pos (the text/text document) ?character-index)) . ?rest)
                                                                                            (bind ((content (output-of content-iomap))
                                                                                                   (content-length (text/length content))
                                                                                                   (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                                                                                   (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                                                                              (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
                                                                                                      `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length)))
                                                                                                        (the text/text (content-of (the tree/leaf document)))))
                                                                                                     ((<= ?character-index opening-delimiter-length)
                                                                                                      `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                                                                                        (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                                                                     ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                                                                                      `((the sequence-position (text/pos (the text/text document) ,(- ?character-index opening-delimiter-length content-length)))
                                                                                                        (the text/text (closing-delimiter-of (the tree/leaf document)))))))))
                                                                                   (replacement-of operation)))
                                   (operation/describe
                                    (make-instance 'operation/describe
                                                   :target (pattern-case (target-of operation)
                                                             (((the character (text/elt (the text/text document) ?character-index)) . ?rest)
                                                              (bind ((content (output-of content-iomap))
                                                                     (content-length (text/length content))
                                                                     (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                                                     (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                                                (econd ((<= opening-delimiter-length ?character-index (+ opening-delimiter-length content-length))
                                                                        `((the character (text/elt (the text/text document) ,(- ?character-index opening-delimiter-length)))
                                                                          (the text/text (content-of (the tree/leaf document)))))
                                                                       ((<= ?character-index opening-delimiter-length)
                                                                        `((the character (text/elt (the text/text document) ,?character-index))
                                                                          (the text/text (opening-delimiter-of (the tree/leaf document)))))
                                                                       ((<= ?character-index (+ opening-delimiter-length content-length closing-delimiter-length))
                                                                        `((the character (text/elt (the text/text document) ,(- ?character-index opening-delimiter-length content-length)))
                                                                          (the text/text (closing-delimiter-of (the tree/leaf document)))))))))))
                                   (operation/compound
                                    (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                      (unless (some 'null child-operations)
                                        (make-operation/compound child-operations)))))))
                        (recurse content-operation))
                      document-operation)
    #+nil
    (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a text selection"
                         :operation (when tree-selection?
                                      (make-operation/replace-selection input `(the sequence-position (text/pos (the text/text (content-of ,selection)) 0)))))
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a tree selection"
                         :operation (when text-selection?
                                      (make-operation/replace-selection input (find-tree-node-reference selection))))
                        ((gesture/keyboard/key-press :sdl-key-home :alt)
                         :domain "Tree" :help "Moves the selection to the root node"
                         :operation (bind ((new-selection `((the ,(form-type input) (content-of (the document document))))))
                                      (unless (equal new-selection selection)
                                        (make-operation/replace-selection input new-selection)))))
                      parent-operation)))

(def reader tree/node->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (input (input-of projection-iomap))
         (selection (selection-of input))
         (text-selection? (text-reference? selection))
         (tree-selection? (tree-reference? selection)))
    (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a text position selection"
                         :operation (when tree-selection?
                                      (make-operation/replace-selection input `(the sequence-position (text/pos (the text/text (content-of ,selection)) 0)))))
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a tree node selection"
                         :operation (when text-selection?
                                      (make-operation/replace-selection input (find-tree-node-reference selection))))
                        ((gesture/keyboard/key-press :sdl-key-home :alt)
                         :domain "Tree" :help "Moves the selection to the root node"
                         :operation (bind ((new-selection `(the ,(form-type input) (content-of (the document document)))))
                                      (unless (equal new-selection (selection-of input))
                                        (make-operation/replace-selection input new-selection))))
                        ((gesture/keyboard/key-press :sdl-key-up)
                         :domain "Tree" :help "Moves the selection to the parent node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                        (make-operation/replace-selection input node-reference))))
                        ((gesture/keyboard/key-press :sdl-key-down)
                         :domain "Tree" :help "Moves the selection to the first child node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        #+nil
                                        (bind ((node (eval-reference input (reference/flatten node-reference))))
                                          (when (and (typep node 'tree/node)
                                                     (> (length (children-of node)) 0))
                                            (bind ((first-child (elt (children-of node) 0)))
                                              (make-operation/replace-selection input `(the ,(form-type first-child) (elt (the list (children-of ,node-reference)) 0)))))))))
                        ((gesture/keyboard/key-press :sdl-key-left)
                         :domain "Tree" :help "Moves the selection to the preceding sibling node"
                         :operation (when tree-selection?
                                      #+nil
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (when (> ?b 0)
                                             (bind ((parent-node (eval-reference input (reference/flatten ?a)))
                                                    (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                               (make-operation/replace-selection input `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b)))))))))))
                        ((gesture/keyboard/key-press :sdl-key-right)
                         :domain "Tree" :help "Moves the selection to the following sibling node"
                         :operation (when tree-selection?
                                      #+nil
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (bind ((parent-node (eval-reference input (reference/flatten ?a))))
                                             (when (< ?b (1- (length (children-of parent-node))))
                                               (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                 (make-operation/replace-selection input `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))))))))))
                        ((gesture/keyboard/key-press :sdl-key-up :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the parent node"
                         :operation (when text-selection?
                                      (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                        (make-operation/replace-selection input `((the sequence-position (text/pos (the text/text document) 0))
                                                                                  (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                  ,@node-reference)))))
                        ((gesture/keyboard/key-press :sdl-key-down :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the first child node"
                         :operation (when text-selection?
                                      #+nil
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (bind ((node (eval-reference input (reference/flatten node-reference))))
                                          (when (and (typep node 'tree/node)
                                                     (> (length (children-of node)) 0))
                                            (bind ((first-child (elt (children-of node) 0)))
                                              (make-operation/replace-selection input
                                                                                (etypecase first-child
                                                                                  (tree/leaf `((the sequence-position (text/pos (the text/text document) 0))
                                                                                               (the text/text (content-of (the tree/leaf document)))
                                                                                               (the tree/leaf (elt (the list document) 0))
                                                                                               (the list (children-of (the tree/node document)))
                                                                                               ,@node-reference))
                                                                                  (tree/node `((the sequence-position (text/pos (the text/text document) 0))
                                                                                               (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                               (the tree/node (elt (the list document) 0))
                                                                                               (the list (children-of (the tree/node document)))
                                                                                               ,@node-reference))))))))))
                        ((gesture/keyboard/key-press :sdl-key-left :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the preceding sibling node"
                         :operation (when text-selection?
                                      #+nil
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (when (> ?b 0)
                                             (bind ((parent-node (eval-reference input (reference/flatten ?a)))
                                                    (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                               (make-operation/replace-selection input
                                                                                 (ecase type
                                                                                   (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0)))
                                                                                   (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0))))))))))))
                        ((gesture/keyboard/key-press :sdl-key-right :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the following sibling node"
                         :operation (when text-selection?
                                      #+nil
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (bind ((parent-node (eval-reference input (reference/flatten ?a))))
                                             (when (< ?b (1- (length (children-of parent-node))))
                                               (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                 (make-operation/replace-selection input
                                                                                   (ecase type
                                                                                     (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0)))
                                                                                     (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0))))))))))))))
                      (labels ((recurse (operation)
                                 (typecase operation
                                   (operation/quit
                                    operation)
                                   (operation/replace-selection
                                    (pattern-case (selection-of operation)
                                      (((the sequence-position (text/pos (the text/text document) ?parent-character-index)) . ?rest)
                                       (bind ((total-length (text/length (output-of projection-iomap)))
                                              (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                              (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                         (cond ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                                (bind (((:values child-index child-character-index) (tree-child-character-index projection-iomap ?parent-character-index)))
                                                  (if child-index
                                                      (bind ((child-iomap (elt (child-iomaps-of projection-iomap) child-index))
                                                             (child (input-of child-iomap))
                                                             (input-child-operation (make-operation/replace-selection child `((the sequence-position (text/pos (the text/text document) ,child-character-index)))))
                                                             (output-child-operation (recurse-reader recursion child-iomap gesture-queue input-child-operation)))
                                                        (when output-child-operation
                                                          (make-operation/replace-selection input
                                                                                            (append (selection-of output-child-operation)
                                                                                                    `((the ,(form-type child) (elt (the list document) ,child-index))
                                                                                                      (the list (children-of (the tree/node document))))))))
                                                      (make-operation/replace-selection input
                                                                                        (append (selection-of operation) `((the text/text (printer-output (the  document) ,projection ,recursion))))))))
                                               ((<= 0 ?parent-character-index opening-delimiter-length)
                                                (make-operation/replace-selection input `((the sequence-position (text/pos (the text/text document) ,?parent-character-index))
                                                                                          (the text/text (opening-delimiter-of (the tree/node document))))))
                                               ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
                                                (make-operation/replace-selection input `((the sequence-position (text/pos (the text/text document) ,(+ (- ?parent-character-index total-length) opening-delimiter-length)))
                                                                                          (the text/text (closing-delimiter-of (the tree/node document)))))))))))
                                   (operation/sequence/replace-element-range
                                    (pattern-case (target-of operation)
                                      (((the sequence-position (text/pos (the text/text document) ?parent-character-index)) . ?rest)
                                       (bind ((total-length (text/length (output-of projection-iomap)))
                                              (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                              (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                         (make-operation/sequence/replace-element-range input
                                                                                        (econd ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                                                                                (bind (((:values child-index child-character-index) (tree-child-character-index projection-iomap ?parent-character-index))
                                                                                                       (child-iomap (elt (child-iomaps-of projection-iomap) child-index))
                                                                                                       (child (input-of child-iomap))
                                                                                                       (input-child-operation (make-operation/sequence/replace-element-range child `((the sequence-position (text/pos (the text/text document) ,child-character-index))) (replacement-of operation)))
                                                                                                       (output-child-operation (recurse-reader recursion child-iomap gesture-queue input-child-operation)))
                                                                                                  (append (target-of output-child-operation)
                                                                                                          `((the ,(form-type child) (elt (the list document) ,child-index))
                                                                                                            (the list (children-of (the tree/node document)))))))
                                                                                               ((<= 0 ?parent-character-index opening-delimiter-length)
                                                                                                `((the sequence-position (text/pos (the text/text document) ,?parent-character-index))
                                                                                                  (the text/text (opening-delimiter-of (the tree/node document)))))
                                                                                               ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
                                                                                                `((the sequence-position (text/pos (the text/text document) ,(+ (- ?parent-character-index total-length) opening-delimiter-length)))
                                                                                                  (the text/text (closing-delimiter-of (the tree/node document))))))
                                                                                        (replacement-of operation))))))
                                   (operation/describe
                                    (pattern-case (target-of operation)
                                      (((the character (text/elt (the text/text document) ?parent-character-index)) . ?rest)
                                       (bind ((total-length (text/length (output-of projection-iomap)))
                                              (opening-delimiter-length (aif (opening-delimiter-of input) (text/length it) 0))
                                              (closing-delimiter-length (aif (closing-delimiter-of input) (text/length it) 0)))
                                         (make-instance 'operation/describe
                                                        :target (econd ((<= opening-delimiter-length ?parent-character-index (- total-length closing-delimiter-length))
                                                                        (bind (((:values child-index child-character-index) (tree-child-character-index projection-iomap ?parent-character-index))
                                                                               (child-iomap (elt (child-iomaps-of projection-iomap) child-index))
                                                                               (child (input-of child-iomap))
                                                                               (input-child-operation (make-instance 'operation/describe :target `((the character (text/elt (the text/text document) ,child-character-index)))))
                                                                               (output-child-operation (recurse-reader recursion child-iomap gesture-queue input-child-operation)))
                                                                          (append (target-of output-child-operation)
                                                                                  `((the ,(form-type child) (elt (the list document) ,child-index))
                                                                                    (the list (children-of (the tree/node document)))))))
                                                                       ((<= 0 ?parent-character-index opening-delimiter-length)
                                                                        `((the character (text/elt (the text/text document) ,?parent-character-index))
                                                                          (the text/text (opening-delimiter-of (the tree/node document)))))
                                                                       ((<= (- total-length closing-delimiter-length) ?parent-character-index total-length)
                                                                        `((the character (text/elt (the text/text document) ,(+ (- ?parent-character-index total-length) opening-delimiter-length)))
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
                        (recurse operation)))))

#+nil
(or (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a text position selection"
                         :operation (when tree-selection?
                                      (make-operation/replace-selection input `(the sequence-position (text/pos (the text/text (content-of ,selection)) 0)))))
                        ((gesture/keyboard/key-press :sdl-key-space :control)
                         :domain "Tree" :help "Turn the selection into a tree node selection"
                         :operation (when text-selection?
                                      (make-operation/replace-selection input (find-tree-node-reference selection))))
                        ((gesture/keyboard/key-press :sdl-key-home :alt)
                         :domain "Tree" :help "Moves the selection to the root node"
                         :operation (bind ((new-selection `(the ,(form-type input) (content-of (the document document)))))
                                      (unless (equal new-selection (selection-of input))
                                        (make-operation/replace-selection input new-selection))))
                        ((gesture/keyboard/key-press :sdl-key-up)
                         :domain "Tree" :help "Moves the selection to the parent node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                        (make-operation/replace-selection input node-reference))))
                        ((gesture/keyboard/key-press :sdl-key-down)
                         :domain "Tree" :help "Moves the selection to the first child node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (bind ((node (eval-reference input node-reference)))
                                          (when (and (typep node 'tree/node)
                                                     (> (length (children-of node)) 0))
                                            (bind ((first-child (elt (children-of node) 0)))
                                              (make-operation/replace-selection input `(the ,(form-type first-child) (elt (the list (children-of ,node-reference)) 0)))))))))
                        ((gesture/keyboard/key-press :sdl-key-left)
                         :domain "Tree" :help "Moves the selection to the preceding sibling node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (when (> ?b 0)
                                             (bind ((parent-node (eval-reference input ?a))
                                                    (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                               (make-operation/replace-selection input `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b)))))))))))
                        ((gesture/keyboard/key-press :sdl-key-right)
                         :domain "Tree" :help "Moves the selection to the following sibling node"
                         :operation (when tree-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (bind ((parent-node (eval-reference input ?a)))
                                             (when (< ?b (1- (length (children-of parent-node))))
                                               (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                 (make-operation/replace-selection input `(the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))))))))))
                        ((gesture/keyboard/key-press :sdl-key-up :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the parent node"
                         :operation (when text-selection?
                                      (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference selection))
                                        (make-operation/replace-selection input `((the sequence-position (text/pos (the text/text document) 0))
                                                                                  (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                  ,@node-reference)))))
                        ((gesture/keyboard/key-press :sdl-key-down :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the first child node"
                         :operation (when text-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (bind ((node (eval-reference input node-reference)))
                                          (when (and (typep node 'tree/node)
                                                     (> (length (children-of node)) 0))
                                            (bind ((first-child (elt (children-of node) 0)))
                                              (make-operation/replace-selection input
                                                                                (etypecase first-child
                                                                                  (tree/leaf `((the sequence-position (text/pos (the text/text document) 0))
                                                                                               (the text/text (content-of (the tree/leaf document)))
                                                                                               (the tree/leaf (elt (the list document) 0))
                                                                                               (the list (children-of (the tree/node document)))
                                                                                               ,@node-reference))
                                                                                  (tree/node `((the sequence-position (text/pos (the text/text document) 0))
                                                                                               (the text/text (opening-delimiter-of (the tree/node document)))
                                                                                               (the tree/node (elt (the list document) 0))
                                                                                               (the list (children-of (the tree/node document)))
                                                                                               ,@node-reference))))))))))
                        ((gesture/keyboard/key-press :sdl-key-left :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the preceding sibling node"
                         :operation (when text-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (when (> ?b 0)
                                             (bind ((parent-node (eval-reference input ?a))
                                                    (type (type-of (elt (children-of parent-node) (1- ?b)))))
                                               (make-operation/replace-selection input
                                                                                 (ecase type
                                                                                   (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0)))
                                                                                   (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0))))))))))))
                        ((gesture/keyboard/key-press :sdl-key-right :alt)
                         :domain "Tree" :help "Moves the selection to the first character of the following sibling node"
                         :operation (when text-selection?
                                      (when-bind node-reference (find-tree-node-reference selection)
                                        (pattern-case node-reference
                                          ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                                           (bind ((parent-node (eval-reference input ?a)))
                                             (when (< ?b (1- (length (children-of parent-node))))
                                               (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                                                 (make-operation/replace-selection input
                                                                                   (ecase type
                                                                                     (tree/leaf `(the sequence-position (text/pos (the text/text (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0)))
                                                                                     (tree/node `(the sequence-position (text/pos (the text/text (opening-delimiter-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0))))))))))))))
                      parent-operation)
    ;; TODO: make gesture-case
    #+nil
    (cond ((text-reference? selection)
           (cond ((typep operation 'operation/sequence/replace-element-range)
                  (bind ((tree-reference (project-backward (content-iomap-of document-iomap) (target-of operation))))
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
                    (make-operation/tree/toggle-expanded document it)))
                 (t
                  parent-operation)))
          (t
           parent-operation)))
