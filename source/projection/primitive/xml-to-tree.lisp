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
  (bind ((output-selection (pattern-case (reverse (selection-of input))
                             (((the tree/node (printer-output (the xml/attribute document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))
                             (((the string (name-of (the xml/attribute document)))
                               (the sequence-position (pos (the string document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the sequence document) 0))
                                (the sequence (children-of (the tree/node document)))))
                             (((the string (value-of (the xml/attribute document)))
                               (the sequence-position (pos (the string document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the sequence document) 1))
                                (the sequence (children-of (the tree/node document)))))))
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
                                                           `((elt (the sequence document) ,attribute-index)
                                                             (the sequence (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (child-iomaps (iter (for child :in (children-of input))
                             (for child-index :from 0)
                             (for child-iomap = (recurse-printer recursion child
                                                                 `((elt (the sequence document) ,child-index)
                                                                   (the sequence (children-of document))
                                                                   ,@(typed-reference (form-type input) input-reference))))
                             ;; KLUDGE:
                             (when deep-element
                               (setf (indentation-of (output-of child-iomap)) 2))
                             (collect child-iomap)))
         (tag-selection (pattern-case (reverse (selection-of input))
                          (((the tree/node (printer-output (the xml/element document) ?projection ?recursion)) . ?rest)
                           (when (and (eq projection ?projection) (eq recursion ?recursion))
                             (reverse ?rest)))
                          (((the string (xml/start-tag (the xml/element document)))
                            (the sequence-position (pos (the string document) ?character-index)))
                           `((the sequence-position (text/pos (the text/text document) ,?character-index))
                             (the text/text (content-of (the tree/leaf document)))
                             (the tree/leaf (elt (the sequence document) 0))
                             (the sequence (children-of (the tree/node document)))))
                          (((the string (xml/end-tag (the xml/element document)))
                            (the sequence-position (pos (the string document) ?character-index)))
                           `((the sequence-position (text/pos (the text/text document) ,?character-index))
                             (the text/text (content-of (the tree/leaf document)))
                             (the tree/leaf (elt (the sequence document) ,(+ (length child-iomaps) (if attribute-iomaps 2 1))))
                             (the sequence (children-of (the tree/node document)))))))
         (attribute-selection (pattern-case (reverse (selection-of input))
                                (((the tree/node (printer-output (the xml/element document) ?projection ?recursion)) . ?rest)
                                 (when (and (eq projection ?projection) (eq recursion ?recursion))
                                   (reverse ?rest)))
                                (((the sequence (attributes-of (the xml/element document)))
                                  (the ?attribute-type (elt (the sequence document) ?attribute-index))
                                  . ?rest)
                                 (bind ((attribute-iomap (elt attribute-iomaps ?attribute-index))
                                        (attribute-output (output-of attribute-iomap)))
                                   (append (selection-of attribute-output)
                                           `((the ,(form-type attribute-output) (elt (the sequence document) ,?attribute-index))
                                             (the sequence (children-of (the tree/node document)))
                                             (the tree/node (elt (the sequence document) 1))
                                             (the sequence (children-of (the tree/node document)))))))))
         (children-selection (pattern-case (reverse (selection-of input))
                               (((the tree/node (printer-output (the xml/element document) ?projection ?recursion)) . ?rest)
                                (when (and (eq projection ?projection) (eq recursion ?recursion))
                                  (reverse ?rest)))
                               (((the sequence (children-of (the xml/element document)))
                                 (the ?child-type (elt (the sequence document) ?child-index))
                                 . ?rest)
                                (bind ((child-iomap (elt child-iomaps ?child-index))
                                       (child-output (output-of child-iomap)))
                                  (append (selection-of child-output)
                                          `((the ,(form-type child-output) (elt (the sequence document) ,(+ ?child-index (if attribute-iomaps 2 1))))
                                            (the sequence (children-of (the tree/node document)))))))))
         (output (make-tree/node (append (list (tree/leaf (:opening-delimiter (text/text () (text/string "<" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                              :closing-delimiter (unless attribute-iomaps
                                                                                                   (text/text () (text/string (if child-iomaps ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                                              :selection (butlast tag-selection 2))
                                                 (text/text (:selection (butlast tag-selection 3))
                                                   (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                         (when attribute-iomaps
                                           (list (make-tree/node (mapcar 'output-of attribute-iomaps)
                                                                 :opening-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                                                 :closing-delimiter (text/text () (text/string (if child-iomaps ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                 :selection (butlast attribute-selection 2))))
                                         (when child-iomaps
                                           (append (mapcar 'output-of child-iomaps)
                                                   (list (tree/leaf (:indentation (if deep-element 0 nil)
                                                                                  :opening-delimiter (text/text () (text/string "</" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                                  :closing-delimiter (text/text () (text/string ">" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                                  :selection (butlast tag-selection 2))
                                                           (text/text (:selection (butlast tag-selection 3))
                                                             (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))))))
                                 :opening-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                 :closing-delimiter (text/text () (text/string "" :font *font/default* :font-color *color/default*))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :selection (or tag-selection attribute-selection children-selection))))
    (make-iomap 'iomap/xml/element->tree/node
                :projection projection :recursion recursion
                :input input :output output
                :attribute-iomaps attribute-iomaps
                :child-iomaps child-iomaps)))

;;;;;;
;;; Reader

(def reader xml/text->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/replace-selection
                                (awhen (pattern-case (selection-of operation)
                                         (((the sequence-position (text/pos (the text/text document) ?index))
                                           (the text/text (content-of (the tree/leaf document))))
                                          `((the sequence-position (pos (the string document) ,?index))
                                            (the string (text-of (the xml/text document))))))
                                  (make-operation/replace-selection printer-input it)))
                               (operation/sequence/replace-element-range
                                (awhen (pattern-case (target-of operation)
                                         (((the sequence-position (text/pos (the text/text document) ?index))
                                           (the text/text (content-of (the tree/leaf document))))
                                          `((the sequence-position (pos (the string document) ,?index))
                                            (the string (text-of (the xml/text document)))))
                                         (((the sequence (text/subseq (the text/text document) ?start-index ?end-index))
                                           (the text/text (content-of (the tree/leaf document))))
                                          `((the sequence (subseq (the string document) ,?start-index ,?end-index))
                                            (the string (text-of (the xml/text document))))))
                                  (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (recurse (operation-of input))))))

(def reader xml/attribute->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/replace-selection
                                (make-operation/replace-selection printer-input
                                                                  (pattern-case (selection-of operation)
                                                                    (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                                      (the text/text (content-of (the tree/leaf document)))
                                                                      (the tree/leaf (elt (the sequence document) 0))
                                                                      (the sequence (children-of (the tree/node document))))
                                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                                       (the string (name-of (the xml/attribute document)))))
                                                                    (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                                      (the text/text (content-of (the tree/leaf document)))
                                                                      (the tree/leaf (elt (the sequence document) 1))
                                                                      (the sequence (children-of (the tree/node document))))
                                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                                       (the string (value-of (the xml/attribute document)))))
                                                                    (?a
                                                                     (append (selection-of operation) `((the tree/node (printer-output (the xml/attribute document) ,projection ,recursion))))))))
                               (operation/sequence/replace-element-range
                                (awhen (pattern-case (target-of operation)
                                         (((the sequence-position (text/pos (the text/text document) ?character-index))
                                           (the text/text (content-of (the tree/leaf document)))
                                           (the tree/leaf (elt (the sequence document) 0))
                                           (the sequence (children-of (the tree/node document))))
                                          `((the sequence-position (pos (the string document) ,?character-index))
                                            (the string (name-of (the xml/attribute document)))))
                                         (((the sequence-position (text/pos (the text/text document) ?character-index))
                                           (the text/text (content-of (the tree/leaf document)))
                                           (the tree/leaf (elt (the sequence document) 1))
                                           (the sequence (children-of (the tree/node document))))
                                          `((the sequence-position (pos (the string document) ,?character-index))
                                            (the string (value-of (the xml/attribute document))))))
                                  (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (recurse (operation-of input))))))

(def reader xml/element->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (first-child-index (if (attribute-iomaps-of printer-iomap) 2 1))
         (last-child-index (+ first-child-index (1- (length (children-of printer-input))))))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/replace-selection
                                (awhen (pattern-case (reverse (selection-of operation))
                                         (((the sequence (children-of (the tree/node document)))
                                           (the ?child-type (elt (the sequence document) ?child-index))
                                           . ?rest)
                                          (econd ((= 0 ?child-index)
                                                  (pattern-case ?rest
                                                    (((the text/text (content-of (the tree/leaf document)))
                                                      (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                       (the string (xml/start-tag (the xml/element document)))))
                                                    (?a
                                                     (append (selection-of operation) `((the tree/node (printer-output (the xml/element document) ,projection ,recursion)))))))
                                                 ((= (1+ last-child-index) ?child-index)
                                                  (pattern-case ?rest
                                                    (((the text/text (content-of (the tree/leaf document)))
                                                      (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                       (the string (xml/end-tag (the xml/element document)))))
                                                    (?a
                                                     (append (selection-of operation) `((the tree/node (printer-output (the xml/element document) ,projection ,recursion)))))))
                                                 ((< ?child-index first-child-index)
                                                  (pattern-case ?rest
                                                    (((the sequence (children-of (the tree/node document)))
                                                      (the ?attribute-type (elt (the sequence document) ?attribute-index))
                                                      . ?rest)
                                                     (bind ((attribute (elt (attributes-of printer-input) ?attribute-index))
                                                            (input-operation (make-operation/replace-selection attribute (reverse ?rest)))
                                                            (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation) (elt (attribute-iomaps-of printer-iomap) ?attribute-index)))))
                                                       (append (selection-of output-operation)
                                                               `((the ,(form-type attribute) (elt (the sequence document) ,?attribute-index))
                                                                 (the sequence (attributes-of (the xml/element document)))))))
                                                    (?a
                                                     (append (selection-of operation) `((the tree/node (printer-output (the xml/element document) ,projection ,recursion)))))))
                                                 ((<= first-child-index ?child-index last-child-index)
                                                  (bind ((child-index (- ?child-index first-child-index))
                                                         (child (elt (children-of printer-input) child-index))
                                                         (input-operation (make-operation/replace-selection child (reverse ?rest)))
                                                         (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation) (elt (child-iomaps-of printer-iomap) child-index)))))
                                                    (when (typep output-operation 'operation/replace-selection)
                                                      (append (selection-of output-operation)
                                                              `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                (the sequence (children-of (the xml/element document))))))))))
                                         (?a
                                          (append (selection-of operation) `((the tree/node (printer-output (the xml/element document) ,projection ,recursion))))))
                                  (make-operation/replace-selection printer-input it)))
                               (operation/sequence/replace-element-range
                                (awhen (pattern-case (reverse (target-of operation))
                                         (((the sequence (children-of (the tree/node document)))
                                           (the ?child-type (elt (the sequence document) ?child-index))
                                           . ?rest)
                                          (econd ((= 0 ?child-index)
                                                  (pattern-case ?rest
                                                    (((the text/text (content-of (the tree/leaf document)))
                                                      (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                       (the string (name-of (the xml/element document)))))))
                                                 ((= (1+ last-child-index) ?child-index)
                                                  (pattern-case ?rest
                                                    (((the text/text (content-of (the tree/leaf document)))
                                                      (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                     `((the sequence-position (pos (the string document) ,?character-index))
                                                       (the string (name-of (the xml/element document)))))))
                                                 ((< ?child-index first-child-index)
                                                  (pattern-case ?rest
                                                    (((the sequence (children-of (the tree/node document)))
                                                      (the ?attribute-type (elt (the sequence document) ?attribute-index))
                                                      . ?rest)
                                                     (bind ((attribute (elt (attributes-of printer-input) ?attribute-index))
                                                            (input-operation (make-operation/sequence/replace-element-range attribute (reverse ?rest) (replacement-of operation)))
                                                            (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation) (elt (attribute-iomaps-of printer-iomap) ?attribute-index)))))
                                                       (when (typep output-operation 'operation/sequence/replace-element-range)
                                                         (append (target-of output-operation)
                                                                 `((the ,(form-type attribute) (elt (the sequence document) ,?attribute-index))
                                                                   (the sequence (attributes-of (the xml/element document))))))))))
                                                 ((<= first-child-index ?child-index last-child-index)
                                                  (bind ((child-index (- ?child-index first-child-index))
                                                         (child (elt (children-of printer-input) child-index))
                                                         (input-operation (make-operation/sequence/replace-element-range child (reverse ?rest) (replacement-of operation)))
                                                         (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation) (elt (child-iomaps-of printer-iomap) child-index)))))
                                                    (when (typep output-operation 'operation/sequence/replace-element-range)
                                                      (append (target-of output-operation)
                                                              `((the ,(form-type child) (elt (the sequence document) ,child-index))
                                                                (the sequence (children-of (the xml/element document)))))))))))
                                  (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (recurse (operation-of input))))))
