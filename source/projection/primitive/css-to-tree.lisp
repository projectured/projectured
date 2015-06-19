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
;;; Forward mapper

(def function forward-mapper/css/attribute->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (name-of (the css/attribute document)))
        (the string (subseq (the string document) ?character-index ?character-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
      (((the string (value-of (the css/attribute document)))
        (the string (subseq (the string document) ?character-index ?character-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 1))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
      (((the tree/leaf (printer-output (the css/attribute document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/css/rule->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (selector-of (the css/rule document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the sequence (attributes-of (the css/rule document)))
        (the ?child-type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((attribute-iomap (elt (child-iomaps-of printer-iomap) ?child-index))
              (attribute-output (output-of attribute-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the tree/node (elt (the sequence document) 1))
                   (the sequence (children-of (the tree/node document)))
                   (the ,(form-type attribute-output) (elt (the sequence document) ,?child-index)))
                 ?rest
                 attribute-iomap)))
      (((the tree/node (printer-output (the css/rule document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/css/attribute->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       `((the string (name-of (the css/attribute document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index))))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 1))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       `((the string (value-of (the css/attribute document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index))))
      (?a
       (append `((the tree/leaf (printer-output (the css/attribute document) ,projection ,recursion))) reference)))))

(def function backward-mapper/css/rule->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= (selector-of printer-input) "")
           (append `((the tree/node (printer-output (the css/rule document) ,projection ,recursion))) reference)
           `((the string (selector-of (the css/rule document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) 1))
        (the sequence (children-of (the tree/node document)))
        (the ?child-type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((attribute-iomap (elt (child-iomaps-of printer-iomap) ?child-index))
              (attribute (elt (attributes-of printer-input) ?child-index)))
         (values `((the sequence (attributes-of (the css/rule document)))
                   (the ,(form-type attribute) (elt (the sequence document) ,?child-index)))
                 ?rest
                 attribute-iomap)))
      (?a
       (append `((the tree/node (printer-output (the css/rule document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer css/attribute->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/css/attribute->tree/leaf)))
         (output (as (bind ((input-name (name-of input))
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
                                                       *color/solarized/green*)))
                       (tree/node (:selection output-selection
                                   :separator (text/text () (text/string ": " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :closing-delimiter (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                         (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                           (text/text (:selection (as (nthcdr 3 (va output-selection))))
                             (text/string attribute-name :font *font/ubuntu/monospace/regular/24* :font-color attribute-name-color)))
                         (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                           (text/text (:selection (as (nthcdr 3 (va output-selection))))
                             (text/string attribute-value :font *font/ubuntu/monospace/regular/24* :font-color attribute-value-color))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer css/rule->tree/node (projection recursion input input-reference)
  (bind ((attribute-iomaps (as (iter (for attribute :in-sequence (attributes-of input))
                                     (for attribute-index :from 0)
                                     (collect (recurse-printer recursion attribute
                                                               `((elt (the sequence document) ,attribute-index)
                                                                 (the sequence (attributes-of document))
                                                                 ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil attribute-iomaps)
                                                (selection-of input)
                                                'forward-mapper/css/rule->tree/node)))
         (output (as (bind ((selector (selector-of input))
                            (selector-value (if (zerop (length selector))
                                                "enter css selector"
                                                selector))
                            (selector-color (if (zerop (length selector))
                                                (color/lighten *color/solarized/blue* 0.75)
                                                *color/solarized/blue*)))
                       (make-tree/node (list (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                               (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                 (text/string selector-value :font *font/ubuntu/monospace/regular/24* :font-color selector-color)))
                                             (make-tree/node (iter (for attribute-iomap :in (va attribute-iomaps))
                                                                   (for attribute-output = (output-of attribute-iomap))
                                                                   (collect (typecase attribute-output
                                                                              (tree/leaf (tree/clone-leaf attribute-output :indentation 2))
                                                                              (tree/node (tree/clone-node attribute-output :indentation 2)))))
                                                             :indentation 0
                                                             :selection (as (nthcdr 2 (va output-selection)))
                                                             :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                             :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                             :closing-delimiter (text/text () (text/newline) (text/string "}" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))))
                                       :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output attribute-iomaps)))

;;;;;;
;;; Reader

(def reader css/attribute->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (name-of (the css/attribute document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the string (value-of (the css/attribute document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))))))))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-tab)
                       :domain "CSS" :description "Moves the selection to the value"
                       :operation (make-operation/replace-selection printer-input
                                                                    '((the string (value-of (the css/attribute document)))
                                                                      (the string (subseq (the string document) 0 0))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/css/attribute->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader css/rule->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (selector-of (the css/rule document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/node (printer-output (the css/rule document) ?projection ?recursion)) . ?rest)
                                   (make-operation/sequence/replace-range printer-input
                                                                          '((the string (selector-of (the css/rule document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation)))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/css/rule->tree/node 'backward-mapper/css/rule->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-insert)
                       :domain "CSS" :description "Starts an insertion into the attributes of the CSS rule"
                       :operation (make-operation/compound (bind ((attributes-length (length (attributes-of printer-input))))
                                                             (list (make-operation/sequence/replace-range printer-input `((the sequence (attributes-of (the css/rule document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,attributes-length ,attributes-length)))
                                                                                                          (list (document/insertion :font *font/liberation/serif/regular/24*)))
                                                                   (make-operation/replace-selection printer-input `((the sequence (attributes-of (the css/rule document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,attributes-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((gesture/keyboard/key-press #\" :shift)
                       :domain "CSS" :description "Inserts a new CSS attribute into the attributes of the CSS rule"
                       :operation (bind ((index (length (attributes-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (attributes-of (the css/rule document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                          (list (css/attribute (:selection '((the string (name-of (the css/attribute document)))
                                                                                                                                             (the string (subseq (the string document) 0 0)))) "" "")))
                                                                   (make-operation/replace-selection printer-input `((the sequence (attributes-of (the css/rule document)))
                                                                                                                     (the css/attribute (elt (the sequence document) ,index))
                                                                                                                     (the string (name-of (the css/attribute document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((gesture/keyboard/key-press :sdl-key-tab)
                       :domain "CSS" :description "Moves the selection to the value"
                       :operation (make-operation/replace-selection printer-input
                                                                    '((the string (value-of (the css/attribute document)))
                                                                      (the string (subseq (the string document) 0 0))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/css/rule->tree/node operation-mapper)
                    (make-command/nothing (gesture-of input)))))
