;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection xml/text->tree/leaf ()
  ())

(def projection xml/attribute->tree/node ()
  ())

(def projection xml/element->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/xml/text->tree/leaf ()
  (make-projection 'xml/text->tree/leaf))

(def (function e) make-projection/xml/attribute->tree/node ()
  (make-projection 'xml/attribute->tree/node))

(def (function e) make-projection/xml/element->tree/node ()
  (make-projection 'xml/element->tree/node))

;;;;;;
;;; Construction

(def (macro e) xml/text->tree/leaf ()
  '(make-projection/xml/text->tree/leaf))

(def (macro e) xml/attribute->tree/node ()
  '(make-projection/xml/attribute->tree/node))

(def (macro e) xml/element->tree/node ()
  '(make-projection/xml/element->tree/node))

;;;;;;
;;; IO map

(def iomap iomap/xml/element->tree/node (iomap)
  ((attribute-iomaps :type sequence)
   (child-iomaps :type sequence)))

;;;;;;
;;; Printer

(def printer xml/text->tree/leaf (projection recursion input input-reference)
  (bind ((output-reference (pattern-case (reverse (selection-of input))
                             (((the string (text-of (the xml/text document)))
                               (the sequence-position (pos (the string document) ?index)))
                              `((the sequence-position (text/pos (the text/text document) ,?index))
                                (the text/text (content-of (the tree/leaf document)))))))
         (output (tree/leaf (:selection output-reference)
                   (text/text (:selection (butlast output-reference))
                     (text/string (text-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer xml/attribute->tree/node (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (selection-of input)
                             (((the sequence-position (pos (the string document) ?character-index))
                               (the string (name-of (the xml/attribute document))))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the list document) 0))
                                (the list (children-of (the tree/node document)))))
                             (((the sequence-position (pos (the string document) ?character-index))
                               (the string (value-of (the xml/attribute document))))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the list document) 1))
                                (the list (children-of (the tree/node document)))))))
         (output (tree/node (:opening-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                             :closing-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                             :separator (text/text () (text/string "=" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                             :selection output-selection)
                   (tree/leaf (:selection (butlast output-selection 2))
                     (text/text (:selection (butlast output-selection 3))
                       (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))
                   (tree/leaf (:opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                               :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                               :selection (butlast output-selection 2))
                     (text/text (:selection (butlast output-selection 3))
                       (text/string (value-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*))))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer xml/element->tree/node (projection recursion input input-reference)
  (bind ((deep-element (find-if (of-type 'xml/element) (children-of input)))
         (attribute-iomaps (iter (for attribute :in (attributes-of input))
                                 (for attribute-index :from 0)
                                 (collect (recurse-printer recursion attribute
                                                           `((elt (the list document) ,attribute-index)
                                                             (the list (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (child-iomaps (iter (for child :in (children-of input))
                             (for child-index :from 0)
                             (for child-iomap = (recurse-printer recursion child
                                                                 `((elt (the list document) ,child-index)
                                                                   (the list (children-of document))
                                                                   ,@(typed-reference (form-type input) input-reference))))
                             ;; KLUDGE:
                             (when deep-element
                               (setf (indentation-of (output-of child-iomap)) 2))
                             (collect child-iomap)))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the string (name-of (the xml/element document)))
                               (the sequence-position (pos (the string document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the list document) 0))
                                (the list (children-of (the tree/node document)))))
                             (((the list (children-of (the xml/element document)))
                               (the ?child-type (elt (the list document) ?child-index))
                               . ?rest)
                              (bind ((child-iomap (elt child-iomaps ?child-index))
                                     (child-output (output-of child-iomap)))
                                (append (selection-of child-output)
                                        `((the ,(form-type child-output) (elt (the list document) ,(+ ?child-index (if attribute-iomaps 2 1))))
                                          (the list (children-of (the tree/node document)))))))))
         (output (make-tree/node (append (list (tree/leaf (:opening-delimiter (text/text () (text/string "<" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                           :closing-delimiter (unless attribute-iomaps
                                                                                (text/text () (text/string (if child-iomaps ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                           :selection (butlast output-selection 2))
                                                 (text/text (:selection (butlast output-selection 3))
                                                   (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                         (when attribute-iomaps
                                           (list (make-tree/node (mapcar 'output-of attribute-iomaps)
                                                                 :opening-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                                                 :closing-delimiter (text/text () (text/string (if child-iomaps ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
                                         (when child-iomaps
                                           (append (mapcar 'output-of child-iomaps)
                                                   (list (tree/leaf (:indentation (if deep-element 0 nil)
                                                                     :opening-delimiter (text/text () (text/string "</" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                     :closing-delimiter (text/text () (text/string ">" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                           (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))))))
                                 :indentation 4
                                 :opening-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                 :closing-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :selection output-selection)))
    (make-iomap 'iomap/xml/element->tree/node
                :projection projection :recursion recursion
                :input input :output output
                :attribute-iomaps attribute-iomaps
                :child-iomaps child-iomaps)))

;;;;;;
;;; Reader

(def reader xml/text->tree/leaf (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (selection-of operation)
                                                      (((the sequence-position (text/pos (the text/text document) ?index))
                                                        (the text/text (content-of (the tree/leaf document))))
                                                       `((the sequence-position (pos (the string document) ,?index))
                                                         (the string (text-of (the xml/text document))))))))
                 (operation/sequence/replace-element-range
                  (make-operation/sequence/replace-element-range input
                                                                 (pattern-case (target-of operation)
                                                                   (((the sequence-position (text/pos (the text/text document) ?index))
                                                                     (the text/text (content-of (the tree/leaf document))))
                                                                    `((the sequence-position (pos (the string document) ,?index))
                                                                      (the string (text-of (the xml/text document))))))
                                                                 (replacement-of operation)))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))

(def reader xml/attribute->tree/node (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (selection-of operation)
                                                      (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                        (the text/text (content-of (the tree/leaf document)))
                                                        (the tree/leaf (elt (the list document) 0))
                                                        (the list (children-of (the tree/node document))))
                                                       `((the sequence-position (pos (the string document) ,?character-index))
                                                         (the string (name-of (the xml/attribute document)))))
                                                      (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                        (the text/text (content-of (the tree/leaf document)))
                                                        (the tree/leaf (elt (the list document) 1))
                                                        (the list (children-of (the tree/node document))))
                                                       `((the sequence-position (pos (the string document) ,?character-index))
                                                         (the string (value-of (the xml/attribute document))))))))
                 (operation/sequence/replace-element-range
                  (make-operation/sequence/replace-element-range input
                                                                 (pattern-case (target-of operation)
                                                                   (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                                     (the text/text (content-of (the tree/leaf document)))
                                                                     (the tree/leaf (elt (the list document) 0))
                                                                     (the list (children-of (the tree/node document))))
                                                                    `((the sequence-position (pos (the string document) ,?character-index))
                                                                      (the string (name-of (the xml/attribute document)))))
                                                                   (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                                     (the text/text (content-of (the tree/leaf document)))
                                                                     (the tree/leaf (elt (the list document) 1))
                                                                     (the list (children-of (the tree/node document))))
                                                                    `((the sequence-position (pos (the string document) ,?character-index))
                                                                      (the string (value-of (the xml/attribute document))))))
                                                                 (replacement-of operation)))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))

(def reader xml/element->tree/node (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (reverse (selection-of operation))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the tree/leaf (elt (the list document) 0))
                                                        (the text/text (content-of (the tree/leaf document)))
                                                        (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                       `((the sequence-position (pos (the string document) ,?character-index))
                                                         (the string (name-of (the xml/element document)))))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the ?child-type (elt (the list document) ?child-index))
                                                        . ?rest)
                                                       (bind ((child-index (- ?child-index (if (attribute-iomaps-of projection-iomap) 2 1)))
                                                              (child (elt (children-of input) child-index))
                                                              (input-operation (make-operation/replace-selection child (reverse ?rest)))
                                                              (output-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) child-index) gesture-queue input-operation)))
                                                         (append (selection-of output-operation)
                                                                 `((the ,(form-type child) (elt (the list document) ,child-index))
                                                                   (the list (children-of (the xml/element document))))))))))
                 (operation/sequence/replace-element-range
                  (make-operation/sequence/replace-element-range input
                                                                 (pattern-case (reverse (target-of operation))
                                                                   (((the list (children-of (the tree/node document)))
                                                                     (the tree/leaf (elt (the list document) 0))
                                                                     (the text/text (content-of (the tree/leaf document)))
                                                                     (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                                    `((the sequence-position (pos (the string document) ,?character-index))
                                                                      (the string (name-of (the xml/element document)))))
                                                                   (((the list (children-of (the tree/node document)))
                                                                     (the ?child-type (elt (the list document) ?child-index))
                                                                     . ?rest)
                                                                    (bind ((child-index (- ?child-index (if (attribute-iomaps-of projection-iomap) 2 1)))
                                                                           (child (elt (children-of input) child-index))
                                                                           (input-operation (make-operation/sequence/replace-element-range child (reverse ?rest) (replacement-of operation)))
                                                                           (output-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) child-index) gesture-queue input-operation)))
                                                                      (append (target-of output-operation)
                                                                              `((the ,(form-type child) (elt (the list document) ,child-index))
                                                                                (the list (children-of (the xml/element document))))))))
                                                                 (replacement-of operation)))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))
