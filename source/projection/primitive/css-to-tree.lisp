;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection css/attribute->tree/leaf ()
  ())

(def projection css/rule->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/css/attribute->tree/leaf ()
  (make-projection 'css/attribute->tree/leaf))

(def function make-projection/css/rule->tree/node ()
  (make-projection 'css/rule->tree/node))

;;;;;;
;;; Printer

(def printer css/attribute->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (reverse (selection-of input))
                             (((the tree/node (printer-output (the css/attribute document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))
                             (((the string (name-of (the css/attribute document)))
                               (the string (subseq (the string document) ?character-index ?character-index)))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the sequence document) 0))
                                (the sequence (children-of (the tree/node document)))))
                             (((the string (value-of (the css/attribute document)))
                               (the string (subseq (the string document) ?character-index ?character-index)))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the sequence document) 1))
                                (the sequence (children-of (the tree/node document)))))))
         (input-name (name-of input))
         (input-value (value-of input))
         (attribute-name (if (zerop (length input-name))
                             "enter css attribute name"
                             input-name))
         (attribute-value (if (zerop (length input-value))
                             "enter css attribute value"
                             input-value))
         (attribute-name-color (if (zerop (length input-name))
                                   (color/lighten *color/solarized/red* 0.75)
                                   *color/solarized/red*))
         (attribute-value-color (if (zerop (length input-value))
                                    (color/lighten *color/solarized/green* 0.75)
                                    *color/solarized/green*))
         (output (tree/node (:selection output-selection
                             :separator (text/text () (text/string ": " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                             :closing-delimiter (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                   (tree/leaf (:selection (butlast output-selection 2))
                     (text/text (:selection (butlast output-selection 3))
                       (text/string attribute-name :font *font/ubuntu/monospace/regular/18* :font-color attribute-name-color)))
                   (tree/leaf (:selection (butlast output-selection 2))
                     (text/text (:selection (butlast output-selection 3))
                       (text/string attribute-value :font *font/ubuntu/monospace/regular/18* :font-color attribute-value-color))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer css/rule->tree/node (projection recursion input input-reference)
  (bind ((attribute-iomaps (iter (for attribute :in-sequence (attributes-of input))
                                 (for attribute-index :from 0)
                                 (collect (recurse-printer recursion attribute
                                                           `((elt (the sequence document) ,attribute-index)
                                                             (the sequence (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (selector (selector-of input))
         (selector-value (if (zerop (length selector))
                             "enter css selector"
                             selector))
         (selector-color (if (zerop (length selector))
                             (color/lighten *color/solarized/blue* 0.75)
                             *color/solarized/blue*))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the tree/node (printer-output (the css/rule document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))
                             (((the string (selector-of (the css/rule document)))
                               (the string (subseq (the string document) ?start-index ?end-index)))
                              `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the sequence document) 0))
                                (the sequence (children-of (the tree/node document)))))
                             (((the sequence (attributes-of (the css/rule document)))
                               (the ?child-type (elt (the sequence document) ?child-index))
                               . ?rest)
                              (bind ((attribute-iomap (elt attribute-iomaps ?child-index))
                                     (attribute-output (output-of attribute-iomap)))
                                (append (selection-of attribute-output)
                                        `((the ,(form-type attribute-output) (elt (the sequence document) ,?child-index))
                                          (the sequence (children-of (the tree/node document)))
                                          (the tree/node (elt (the sequence document) 1))
                                          (the sequence (children-of (the tree/node document)))))))))
         (output (make-tree/node (list (tree/leaf (:selection (butlast output-selection 2))
                                         (text/text (:selection (butlast output-selection 3))
                                           (text/string selector-value :font *font/ubuntu/monospace/regular/18* :font-color selector-color)))
                                       (make-tree/node (iter (for attribute-iomap :in attribute-iomaps)
                                                             (setf (indentation-of (output-of attribute-iomap)) 2)
                                                             (collect (output-of attribute-iomap)))
                                                       :indentation 0
                                                       :selection (butlast output-selection 2)
                                                       :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :closing-delimiter (text/text () (text/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
                                 :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output attribute-iomaps)))

;;;;;;
;;; Reader

(def reader css/attribute->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-p :control)
                       :domain "CSS" :description "Switches to generic tree notation"
                       :operation (make-operation/functional (lambda () (setf (projection-of printer-input) (recursive (make-projection/t->tree))))))
                      ((gesture/keyboard/key-press :sdl-key-tab)
                       :domain "CSS" :description "Moves the selection to the value"
                       :operation (make-operation/replace-selection printer-input
                                                                    '((the string (subseq (the string document) 0 0))
                                                                      (the string (value-of (the css/attribute document)))))))
                    (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (make-operation/replace-selection printer-input
                                                                           (pattern-case (selection-of operation)
                                                                             (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                                               (the text/text (content-of (the tree/leaf document)))
                                                                               (the tree/leaf (elt (the sequence document) 0))
                                                                               (the sequence (children-of (the tree/node document))))
                                                                              `((the string (subseq (the string document) ,?start-character-index ,?end-character-index))
                                                                                (the string (name-of (the css/attribute document)))))
                                                                             (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                                               (the text/text (content-of (the tree/leaf document)))
                                                                               (the tree/leaf (elt (the sequence document) 1))
                                                                               (the sequence (children-of (the tree/node document))))
                                                                              `((the string (subseq (the string document) ,?start-character-index ,?end-character-index))
                                                                                (the string (value-of (the css/attribute document)))))
                                                                             (?a
                                                                              (append (selection-of operation) `((the tree/node (printer-output (the css/attribute document) ,projection ,recursion))))))))
                                        (operation/sequence/replace-element-range
                                         (awhen (pattern-case (target-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                    (the text/text (content-of (the tree/leaf document)))
                                                    (the tree/leaf (elt (the sequence document) 0))
                                                    (the sequence (children-of (the tree/node document))))
                                                   `((the string (subseq (the string document) ,?start-character-index ,?end-character-index))
                                                     (the string (name-of (the css/attribute document)))))
                                                  (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                    (the text/text (content-of (the tree/leaf document)))
                                                    (the tree/leaf (elt (the sequence document) 1))
                                                    (the sequence (children-of (the tree/node document))))
                                                   `((the string (subseq (the string document) ,?start-character-index ,?end-character-index))
                                                     (the string (value-of (the css/attribute document))))))
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

(def reader css/rule->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-insert)
                       :domain "CSS" :description "Starts an object insertion into the attributes of the CSS rule"
                       :operation (make-operation/compound (bind ((attributes-length (length (attributes-of printer-input))))
                                                             (list (make-operation/sequence/replace-element-range printer-input `((the sequence (subseq (the sequence document) ,attributes-length ,attributes-length))
                                                                                                                                  (the sequence (attributes-of (the css/rule document)))) (list (document/insertion :font *font/liberation/serif/regular/18*)))
                                                                   (make-operation/replace-selection printer-input `((the string (subseq (the string document) 0 0))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,attributes-length))
                                                                                                                     (the sequence (attributes-of (the css/rule document)))))))))
                      ((gesture/keyboard/key-press #\" :shift)
                       :domain "CSS" :description "Inserts a new CSS attribute into the attributes of the CSS rule"
                       :operation (bind ((index (length (attributes-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-element-range printer-input `((the sequence (subseq (the sequence document) ,index ,index))
                                                                                                                                  (the sequence (attributes-of (the css/rule document))))
                                                                                                                  (list (css/attribute (:selection '((the string (subseq (the string document) 0 0))
                                                                                                                                                     (the string (name-of (the css/attribute document))))) "" "")))
                                                                   (make-operation/replace-selection printer-input `((the string (subseq (the string document) 0 0))
                                                                                                                     (the string (name-of (the css/attribute document)))
                                                                                                                     (the css/attribute (elt (the sequence document) ,index))
                                                                                                                     (the sequence (attributes-of (the css/rule document)))))))))
                      ((gesture/keyboard/key-press :sdl-key-p :control)
                       :domain "CSS" :description "Switches to generic tree notation"
                       :operation (make-operation/functional (lambda () (setf (projection-of printer-input) (recursive (make-projection/t->tree))))))
                      ((gesture/keyboard/key-press :sdl-key-tab)
                       :domain "CSS" :description "Moves the selection to the value"
                       :operation (make-operation/replace-selection printer-input
                                                                    '((the string (subseq (the string document) 0 0))
                                                                      (the string (value-of (the css/attribute document)))))))
                    (make-command/nothing (gesture-of input)))))
