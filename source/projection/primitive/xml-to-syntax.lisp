;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection xml/insertion->syntax/leaf ()
  ())

(def projection xml/text->syntax/leaf ()
  ())

(def projection xml/attribute->syntax/node ()
  ())

(def projection xml/element->syntax/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/xml/insertion->syntax/leaf ()
  (make-projection 'xml/insertion->syntax/leaf))

(def function make-projection/xml/text->syntax/leaf ()
  (make-projection 'xml/text->syntax/leaf))

(def function make-projection/xml/attribute->syntax/node ()
  (make-projection 'xml/attribute->syntax/node))

(def function make-projection/xml/element->syntax/node ()
  (make-projection 'xml/element->syntax/node))

;;;;;;
;;; Construction

(def macro xml/insertion->syntax/leaf ()
  '(make-projection/xml/insertion->syntax/leaf))

(def macro xml/text->syntax/leaf ()
  '(make-projection/xml/text->syntax/leaf))

(def macro xml/attribute->syntax/node ()
  '(make-projection/xml/attribute->syntax/node))

(def macro xml/element->syntax/node ()
  '(make-projection/xml/element->syntax/node))

;;;;;;
;;; IO map

(def iomap iomap/xml/element->syntax/node ()
  ((attribute-iomaps :type sequence)
   (child-iomaps :type sequence)))

;;;;;;
;;; Forward mapper

(def forward-mapper xml/insertion->syntax/leaf ()
  (pattern-case -reference-
    (((the string (value-of (the xml/insertion document)))
      (the string (subseq (the string document) 0 0)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) 0 0))))))

(def forward-mapper xml/text->syntax/leaf ()
  (pattern-case -reference-
    (((the xml/text document))
     '((the syntax/leaf document)))
    (((the string (value-of (the xml/text document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper xml/attribute->syntax/node ()
  (pattern-case -reference-
    (((the xml/attribute document))
     '((the syntax/node document)))
    (((the string (name-of (the xml/attribute document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
    (((the string (value-of (the xml/attribute document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 1))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))))

(def forward-mapper xml/element->syntax/node ()
  (pattern-case -reference-
    (((the xml/element document))
     '((the syntax/node document)))
    (((the string (xml/start-tag (the xml/element document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
    (((the string (xml/end-tag (the xml/element document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) ,(+ (length (children-of -printer-input-)) (if (emptyp (attributes-of -printer-input-)) 1 2))))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
    (((the sequence (attributes-of (the xml/element document)))
      (the ?attribute-type (elt (the sequence document) ?attribute-index))
      . ?rest)
     (bind ((attribute-iomap (elt (attribute-iomaps-of -printer-iomap-) ?attribute-index))
            (attribute-output (output-of attribute-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the syntax/node (elt (the sequence document) 1))
                 (the sequence (children-of (the syntax/node document)))
                 (the ,(document-type attribute-output) (elt (the sequence document) ,?attribute-index)))
               ?rest
               attribute-iomap)))
    (((the sequence (children-of (the xml/element document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index))
            (child-output (output-of child-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type child-output) (elt (the sequence document) ,(+ ?child-index (if (emptyp (attributes-of -printer-input-)) 1 2)))))
               ?rest
               child-iomap)))))

;;;;;;
;;; Backward mapper

(def backward-mapper xml/insertion->syntax/leaf ()
  (pattern-case -reference-
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) 0 0)))
     `((the string (value-of (the xml/insertion document)))
       (the string (subseq (the string document) 0 0))))))

(def backward-mapper xml/text->syntax/leaf ()
  (pattern-case -reference-
    (((the syntax/leaf document))
     '((the xml/text document)))
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (unless (string= (value-of -printer-input-) "")
       `((the string (value-of (the xml/text document)))
         (the string (subseq (the string document) ,?start-index ,?end-index)))))))

(def backward-mapper xml/attribute->syntax/node ()
  (pattern-case -reference-
    (((the syntax/node document))
     '((the xml/attribute document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (unless (string= (name-of -printer-input-) "")
       `((the string (name-of (the xml/attribute document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 1))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (unless (string= (value-of -printer-input-) "")
       `((the string (value-of (the xml/attribute document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))))

(def backward-mapper xml/element->syntax/node ()
  (bind ((first-child-index (if (attribute-iomaps-of -printer-iomap-) 2 1))
         (last-child-index (+ first-child-index (1- (length (children-of -printer-input-))))))
    (pattern-case -reference-
      (((the syntax/node document))
       '((the xml/element document)))
      (((the sequence (children-of (the syntax/node document)))
        (the sequence (subseq (the sequence document) ?start-index ?end-index)))
       ;; TODO: 1?
       `((the sequence (children-of (the xml/element document)))
         (the sequence (subseq (the sequence document) ,(- ?start-index 1) ,(- ?end-index 1)))))
      (((the sequence (children-of (the syntax/node document)))
        (the ?child-type (elt (the sequence document) ?child-index))
        . ?rest)
       (econd ((= 0 ?child-index)
               (pattern-case ?rest
                 (((the text/text (content-of (the syntax/leaf document)))
                   (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                  (unless (string= (name-of -printer-input-) "")
                    `((the string (xml/start-tag (the xml/element document)))
                      (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))))
              ((= (1+ last-child-index) ?child-index)
               (pattern-case ?rest
                 (((the text/text (content-of (the syntax/leaf document)))
                   (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                  (unless (string= (name-of -printer-input-) "")
                    `((the string (xml/end-tag (the xml/element document)))
                      (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))))
              ((< ?child-index first-child-index)
               (pattern-case ?rest
                 (((the sequence (children-of (the syntax/node document)))
                   (the ?attribute-type (elt (the sequence document) ?attribute-index))
                   . ?rest)
                  (bind ((attribute (elt (attributes-of -printer-input-) ?attribute-index))
                         (attribute-iomap (elt (attribute-iomaps-of -printer-iomap-) ?attribute-index)))
                    (values `((the sequence (attributes-of (the xml/element document)))
                              (the ,(document-type attribute) (elt (the sequence document) ,?attribute-index)))
                            ?rest
                            attribute-iomap)))))
              ((<= first-child-index ?child-index last-child-index)
               (bind ((child-index (- ?child-index first-child-index))
                      (child (elt (children-of -printer-input-) child-index))
                      (child-iomap (elt (child-iomaps-of -printer-iomap-) child-index)))
                 (values `((the sequence (children-of (the xml/element document)))
                           (the ,(document-type child) (elt (the sequence document) ,child-index)))
                         ?rest
                         child-iomap))))))))

;;;;;;
;;; Printer

(def printer xml/insertion->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap- (get-selection -input-))))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (value-of -input-) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer xml/text->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap- (get-selection -input-))))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/make-default-text (value-of -input-) "enter xml text" :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer xml/attribute->syntax/node ()
  (bind ((output-selection (as (print-selection -printer-iomap- (get-selection -input-))))
         (output (as (syntax/node (:separator (text/text () (text/string "=" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                  :selection output-selection)
                       (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                         (text/make-default-text (name-of -input-) "enter xml attribute name" :selection (as (nthcdr 3 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))
                       (syntax/leaf (:opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :selection (as (nthcdr 2 (va output-selection))))
                         (text/make-default-text (value-of -input-) "enter xml attribute value" :selection (as (nthcdr 3 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer xml/element->syntax/node ()
  (bind ((deep-element (not (emptyp (children-of -input-))))
         (attribute-iomaps (as (iter (for attribute :in-sequence (attributes-of -input-))
                                     (for attribute-index :from 0)
                                     (collect (recurse-printer -recursion- attribute
                                                               `((elt (the sequence document) ,attribute-index)
                                                                 (the sequence (attributes-of document))
                                                                 ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (child-iomaps (as (map-ll* (children-of -input-) (lambda (child index)
                                                          (recurse-printer -recursion- (value-of child)
                                                                           `((elt (the sequence document) ,index)
                                                                             (the sequence (children-of (the xml/element document)))
                                                                             ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap- (get-selection -input-))))
         (output (as (bind ((children (children-of -input-))
                            (element-name (text/make-default-text (name-of -input-) "enter xml element name" :selection (as (nthcdr 3 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*)))
                       (make-syntax/node (append-ll (ll (append (list (ll (list (syntax/leaf (:opening-delimiter (text/text () (text/string "<" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                          :closing-delimiter (unless (va attribute-iomaps)
                                                                                                               (text/text () (text/string (if (emptyp children) "/>" ">") :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                                                                          :selection (as (nthcdr 2 (va output-selection))))
                                                                                element-name))))
                                                              (when (va attribute-iomaps)
                                                                (list (ll (list (make-syntax/node (mapcar 'output-of (va attribute-iomaps))
                                                                                                :closing-delimiter (text/text () (text/string (if (emptyp children) "/>" ">") :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                                :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                                :selection (as (nthcdr 2 (va output-selection))))))))
                                                              (unless (emptyp children)
                                                                (list (append-ll (ll (list (map-ll (va child-iomaps) (lambda (element)
                                                                                                                       (bind ((child-output (output-of element)))
                                                                                                                         (if deep-element
                                                                                                                             (etypecase child-output
                                                                                                                               (syntax/leaf (syntax/clone-leaf child-output :indentation 2))
                                                                                                                               (syntax/node (syntax/clone-node child-output :indentation 2))
                                                                                                                               (t child-output))
                                                                                                                             child-output))))
                                                                                           (ll (list (syntax/leaf (:indentation (if deep-element 0 nil)
                                                                                                                 :opening-delimiter (text/text () (text/string "</" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                                                 :closing-delimiter (text/text () (text/string ">" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                                                 :selection (as (nthcdr 2 (va output-selection))))
                                                                                                       element-name)))))))))))
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :collapsed (as (collapsed-p -input-))
                                       :selection output-selection)))))
    (make-instance 'iomap/xml/element->syntax/node
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :attribute-iomaps attribute-iomaps :child-iomaps child-iomaps)))

;;;;;;
;;; Reader

(def reader xml/insertion->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore selection child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                ;; TODO: do nothing
                                (make-operation/functional (lambda ())))))))
    (merge-commands (gesture-case -gesture-
                      ((make-type-in-gesture #\")
                       :domain "XML" :description "Inserts a new XML text into the children of the XML element"
                       :operation (make-operation/replace-target -printer-input- nil (xml/text (:selection '((the string (value-of (the xml/text document)))
                                                                                                             (the string (subseq (the string document) 0 0))))
                                                                                       "")))
                      ((make-type-in-gesture #\<)
                       :domain "XML" :description "Inserts a new XML element into the children of the XML element"
                       :operation (make-operation/replace-target -printer-input- nil (xml/element ("" nil :selection `((the string (xml/start-tag (the xml/element document)))
                                                                                                                       (the string (subseq (the string document) 0 0))))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader xml/text->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (value-of (the xml/text document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/leaf (printer-output (the xml/text document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (value-of (the xml/text document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader xml/attribute->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (name-of (the xml/attribute document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the string (value-of (the xml/attribute document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/node (printer-output (the xml/attribute document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 0))
                                    . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (name-of (the xml/attribute document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))
                                  (((the syntax/node (printer-output (the xml/attribute document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 1))
                                    . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (value-of (the xml/attribute document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case -gesture-
                      ((make-type-in-gesture #\=)
                       :domain "XML" :description "Moves the selection to the value"
                       :operation (pattern-case (selection-of -printer-input-)
                                    (((the string (name-of (the xml/attribute document)))
                                      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                     (make-operation/replace-selection -printer-input-
                                                                       '((the string (value-of (the xml/attribute document)))
                                                                         (the string (subseq (the string document) 0 0))))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader xml/element->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (xml/start-tag (the xml/element document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the string (xml/end-tag (the xml/element document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/node (printer-output (the xml/element document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 0))
                                    . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (xml/start-tag (the xml/element document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))
                                  (((the syntax/node (printer-output (the xml/element document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) ?child-index))
                                    . ?rest)
                                   (when (= ?child-index (1- (length (children-of (output-of -printer-iomap-)))))
                                     (make-operation/string/replace-range -printer-input-
                                                                          '((the string (xml/end-tag (the xml/element document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation))))))))))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-insert)
                       :domain "XML" :description "Starts a generic insertion into the children of the XML element"
                       :operation (make-operation/compound (bind ((children-length (length (children-of -printer-input-))))
                                                             (list (make-operation/sequence/replace-range -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                            (the sequence (subseq (the sequence document) ,children-length ,children-length)))
                                                                                                          (list (document/insertion (:font *font/liberation/serif/regular/24*))))
                                                                   (make-operation/replace-selection -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                       (the document/insertion (elt (the sequence document) ,children-length))
                                                                                                                       (the string (value-of (the document/insertion document)))
                                                                                                                       (the string (subseq (the string document) 0 0))))))))
                      ((make-key-press-gesture :scancode-space)
                       :domain "XML" :description "Inserts a new XML attribute into the attributes of the XML element"
                       :operation (pattern-case (selection-of -printer-input-)
                                    ((?or ((the string (xml/start-tag (the xml/element document)))
                                           (the string (subseq (the string document) ?start-index ?end-index)))
                                          ((the sequence (attributes-of (the xml/element document)))
                                           (the xml/attribute (elt (the sequence document) ?attribute-index))
                                           (the syntax/node (printer-output (the xml/attribute document) ?projection ?recursion)). ?rest)
                                          ((the syntax/node (printer-output (the xml/element document) ?projection ?recursion)) . ?rest))
                                     (bind ((index (length (attributes-of -printer-input-))))
                                       (make-operation/compound (list (make-operation/sequence/replace-range -printer-input- `((the sequence (attributes-of (the xml/element document)))
                                                                                                                               (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                             (list (xml/attribute () "" "")))
                                                                      (make-operation/replace-selection -printer-input- `((the sequence (attributes-of (the xml/element document)))
                                                                                                                          (the xml/attribute (elt (the sequence document) ,index))
                                                                                                                          (the string (name-of (the xml/attribute document)))
                                                                                                                          (the string (subseq (the string document) 0 0))))))))))
                      ((make-type-in-gesture #\")
                       :domain "XML" :description "Inserts a new XML text into the children of the XML element"
                       :operation (bind ((index (length (children-of -printer-input-))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                            (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                          (list (xml/text () "")))
                                                                   (make-operation/replace-selection -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                       (the xml/text (elt (the sequence document) ,index))
                                                                                                                       (the string (value-of (the xml/text document)))
                                                                                                                       (the string (subseq (the string document) 0 0))))))))
                      ((make-type-in-gesture #\<)
                       :domain "XML" :description "Inserts a new XML element into the children of the XML element"
                       :operation (bind ((index (length (children-of -printer-input-))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                            (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                          (list (xml/element ("" nil))))
                                                                   (make-operation/replace-selection -printer-input- `((the sequence (children-of (the xml/element document)))
                                                                                                                       (the xml/element (elt (the sequence document) ,index))
                                                                                                                       (the string (xml/start-tag (the xml/element document)))
                                                                                                                       (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))
