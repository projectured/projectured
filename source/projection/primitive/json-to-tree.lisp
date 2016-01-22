;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection json/insertion->tree/leaf ()
  ())

(def projection json/null->tree/leaf ()
  ())

(def projection json/boolean->tree/leaf ()
  ())

(def projection json/number->tree/leaf ()
  ())

(def projection json/string->tree/leaf ()
  ())

(def projection json/array->tree/node ()
  ())

(def projection json/object-entry->tree/node ()
  ())

(def projection json/object->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/json/insertion->tree/leaf ()
  (make-projection 'json/insertion->tree/leaf))

(def function make-projection/json/null->tree/leaf ()
  (make-projection 'json/null->tree/leaf))

(def function make-projection/json/boolean->tree/leaf ()
  (make-projection 'json/boolean->tree/leaf))

(def function make-projection/json/number->tree/leaf ()
  (make-projection 'json/number->tree/leaf))

(def function make-projection/json/string->tree/leaf ()
  (make-projection 'json/string->tree/leaf))

(def function make-projection/json/array->tree/node ()
  (make-projection 'json/array->tree/node))

(def function make-projection/json/object-entry->tree/node ()
  (make-projection 'json/object-entry->tree/node))

(def function make-projection/json/object->tree/node ()
  (make-projection 'json/object->tree/node))

;;;;;;
;;; Construction

(def macro json/insertion->tree/leaf ()
  '(make-projection/json/insertion->tree/leaf))

(def macro json/null->tree/leaf ()
  '(make-projection/json/null->tree/leaf))

(def macro json/boolean->tree/leaf ()
  '(make-projection/json/boolean->tree/leaf))

(def macro json/number->tree/leaf ()
  '(make-projection/json/number->tree/leaf))

(def macro json/string->tree/leaf ()
  `(make-projection/json/string->tree/leaf))

(def macro json/array->tree/node ()
  `(make-projection/json/array->tree/node))

(def macro json/object-entry->tree/node ()
  '(make-projection/json/object-entry->tree/node))

(def macro json/object->tree/node ()
  '(make-projection/json/object->tree/node))

;;;;;;
;;; Forward mapper

(def function forward-mapper/json/insertion->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (value-of (the json/insertion document)))
        (the string (subseq (the string document) 0 0)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) 0 0))))
      (((the tree/leaf (printer-output (the json/insertion document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/null->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/null document))
       '((the tree/leaf document)))
      (((the string (value-of (the json/null document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the tree/leaf (printer-output (the json/null document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/boolean->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/boolean document))
       '((the tree/leaf document)))
      (((the string ((?or false-value-of true-value-of) (the json/boolean document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the tree/leaf (printer-output (the json/boolean document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/number->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/number document))
       '((the tree/leaf document)))
      (((the document/number (value-of (the json/number document)))
        (the string (write-to-string (the document/number document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the tree/leaf (printer-output (the json/number document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/string->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/string document))
       '((the tree/leaf document)))
      (((the string (value-of (the json/string document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the tree/leaf (printer-output (the json/string document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/array->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/array document))
       '((the tree/node document)))
      (((the sequence (elements-of (the json/array document)))
        . ?rest)
       (values `((the sequence (children-of (the tree/node document))))
               ?rest
               (content-iomap-of printer-iomap)))
      (((the tree/node (printer-output (the json/array document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/object-entry->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/object-entry document))
       '((the tree/node document)))
      (((the string (key-of (the json/object-entry document)))
        (the string document))
       '((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the tree/leaf document)))
      (((the string (key-of (the json/object-entry document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the ?value-type (value-of (the json/object-entry document)))
        . ?rest)
       (values `((the sequence (children-of (the tree/node document)))
                 (the ,(document-type (output-of (content-iomap-of printer-iomap))) (elt (the sequence document) 1)))
               ?rest
               (content-iomap-of printer-iomap)))
      (((the tree/node (printer-output (the json/object-entry document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/json/object->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the json/object document))
       '((the tree/node document)))
      (((the sequence (entries-of (the json/object document)))
        . ?rest)
       (values `((the sequence (children-of (the tree/node document))))
               ?rest
               (content-iomap-of printer-iomap)))
      (((the tree/node (printer-output (the json/object document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/json/insertion->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) 0 0)))
       `((the string (value-of (the json/insertion document)))
         (the string (subseq (the string document) 0 0))))
      (?a
       (append `((the tree/leaf (printer-output (the json/insertion document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/null->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the json/null document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (value-of (the json/null document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the tree/leaf (printer-output (the json/null document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/boolean->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the json/boolean document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (,(if (value-p printer-input) 'true-value-of 'false-value-of) (the json/boolean document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the tree/leaf (printer-output (the json/boolean document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/number->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the json/number document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= "" (write-to-string (value-of printer-input)))
           (append `((the tree/leaf (printer-output (the json/number document) ,projection ,recursion))) reference)
           `((the document/number (value-of (the json/number document)))
             (the string (write-to-string (the document/number document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (?a
       (append `((the tree/leaf (printer-output (the json/number document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/string->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the json/string document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= "" (write-to-string (value-of printer-input) :escape #f))
           (append `((the tree/leaf (printer-output (the json/string document) ,projection ,recursion))) reference)
           `((the string (value-of (the json/string document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (?a
       (append `((the tree/leaf (printer-output (the json/string document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/array->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the json/array document)))
      (((the sequence (children-of (the tree/node document)))
        . ?rest)
       (values `((the sequence (elements-of (the json/array document))))
               ?rest
               (content-iomap-of printer-iomap)))
      (?a
       (append `((the tree/node (printer-output (the json/array document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/object-entry->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the json/object-entry document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the tree/leaf document))
       '((the string (key-of (the json/object-entry document)))
         (the string document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= "" (write-to-string (key-of printer-input) :escape #f))
           (append `((the tree/node (printer-output (the json/object-entry document) ,projection ,recursion))) reference)
           `((the string (key-of (the json/object-entry document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (((the sequence (children-of (the tree/node document)))
        (the ?child-type (elt (the sequence document) 1))
        . ?rest)
       (values `((the ,(document-type (value-of printer-input)) (value-of (the json/object-entry document))))
               ?rest
               (content-iomap-of printer-iomap)))
      (?a
       (append `((the tree/node (printer-output (the json/object-entry document) ,projection ,recursion))) reference)))))

(def function backward-mapper/json/object->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the json/object document)))
      (((the sequence (children-of (the tree/node document)))
        . ?rest)
       (values `((the sequence (entries-of (the json/object document))))
               ?rest
               (content-iomap-of printer-iomap)))
      (((the sequence (children-of (the tree/node document)))
        (the sequence (subseq (the sequence document) ?start-index ?end-index))
        . ?rest)
       (if (= ?start-index (1- ?end-index))
           (bind ((entry-iomap (elt (child-iomaps-of printer-iomap) ?start-index)))
             (values `((the sequence (entries-of (the json/object document)))
                       (the sequence (subseq (the sequence document) ,?start-index ,?end-index)))
                     ?rest
                     entry-iomap))
           (append `((the tree/node (printer-output (the json/object document) ,projection ,recursion))) reference)))
      (?a
       (append `((the tree/node (printer-output (the json/object document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer json/insertion->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil) (selection-of input) 'forward-mapper/json/insertion->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (value-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))))
    (make-iomap projection recursion input input-reference output)))

(def printer json/null->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil) (selection-of input) 'forward-mapper/json/null->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (value-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))))
    (make-iomap projection recursion input input-reference output)))

(def printer json/boolean->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil) (selection-of input) 'forward-mapper/json/boolean->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (if (value-p input) (true-value-of input) (false-value-of input)) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))))
    (make-iomap projection recursion input input-reference output)))

(def printer json/number->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil) (selection-of input) 'forward-mapper/json/number->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/make-default-text (write-number (value-of input) nil) "enter json number" :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*)))))
    (make-iomap projection recursion input input-reference output)))

(def printer json/string->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil) (selection-of input) 'forward-mapper/json/string->tree/leaf)))
         (output (as (tree/leaf (:opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                 :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                 :selection output-selection)
                       (text/make-default-text (value-of input) "enter json string" :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)))))
    (make-iomap projection recursion input input-reference output)))

(def printer json/array->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (recurse-printer recursion (elements-of input) `((elements-of (the json/array document))
                                                                              ,@(typed-reference (document-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/content projection recursion input input-reference nil element-iomaps) (selection-of input) 'forward-mapper/json/array->tree/node)))
         (output (as (bind ((deep-array (find-if-not (of-type '(or json/insertion json/null json/boolean json/number json/string)) (elements-of input))))
                       (make-tree/node (as (map-ll* (ll (output-of (va element-iomaps)))
                                                    (lambda (element index)
                                                      (bind ((element-output (value-of element)))
                                                        (if (and deep-array (previous-element-of element))
                                                            (typecase element-output
                                                              (tree/leaf (tree/clone-leaf element-output :indentation 1 :selection (as (nthcdr 2 (va output-selection)))))
                                                              (tree/node (tree/clone-node element-output :indentation 1 :selection (as (nthcdr 2 (va output-selection))))))
                                                            element-output)))))
                                       :opening-delimiter (text/text () (text/string "[" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :closing-delimiter (text/text () (text/string "]" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :separator (text/text () (text/string (if deep-array "," ", ") :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :collapsed (as (collapsed-p input))
                                       :selection output-selection)))))
    (make-iomap/content projection recursion input input-reference output element-iomaps)))

(def printer json/object-entry->tree/node (projection recursion input input-reference)
  (bind ((value-iomap (as (recurse-printer recursion (value-of input)
                                           `((value-of (the json/object-entry document))
                                             ,@(typed-reference (document-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/content projection recursion input input-reference nil value-iomap) (selection-of input) 'forward-mapper/json/object-entry->tree/node)))
         (output (as (tree/node (:separator (text/text () (text/string " : " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                 :collapsed (as (collapsed-p input))
                                 :selection output-selection)
                       (tree/leaf (:opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :selection (as (nthcdr 2 (va output-selection))))
                         (text/make-default-text (key-of input) "enter key" :selection (as (nthcdr 3 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))
                       (output-of (va value-iomap))))))
    (make-iomap/content projection recursion input input-reference output value-iomap)))

(def printer json/object->tree/node (projection recursion input input-reference)
  (bind ((entry-iomaps (as (recurse-printer recursion (entries-of input) `((entries-of (the json/object document))
                                                                           ,@(typed-reference (document-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/content projection recursion input input-reference nil entry-iomaps) (selection-of input) 'forward-mapper/json/object->tree/node)))
         (output (as (make-tree/node (as (map-ll* (ll (output-of (va entry-iomaps)))
                                                  (lambda (element index)
                                                    (bind ((entry-output (value-of element)))
                                                      (if (previous-element-of element)
                                                          (typecase entry-output
                                                            (tree/leaf (tree/clone-leaf entry-output :indentation 1 :selection (as (nthcdr 2 (va output-selection)))))
                                                            (tree/node (tree/clone-node entry-output :indentation 1 :selection (as (nthcdr 2 (va output-selection))))))
                                                          entry-output)))))
                                     :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :closing-delimiter (text/text () (text/string "}" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :separator (text/text () (text/string "," :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :collapsed (as (collapsed-p input))
                                     :selection output-selection))))
    (make-iomap/content projection recursion input input-reference output entry-iomaps)))

;;;;;;
;;; Reader

(def function json/read-command (printer-iomap gesture)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case gesture
                      ((make-type-in-gesture #\n)
                       :domain "JSON" :description "Replaces the selected element with null"
                       :operation (make-operation/replace-target printer-input nil (json/null (:selection '((the string (value-of (the json/null document)))
                                                                                                            (the string (subseq (the string document) 0 0)))))))
                      ((make-type-in-gesture #\f)
                       :domain "JSON" :description "Replaces the selected element with false"
                       :operation (make-operation/replace-target printer-input nil (json/boolean (:selection '((the string (false-value-of (the json/boolean document)))
                                                                                                               (the string (subseq (the string document) 0 0))))
                                                                                     #f)))
                      ((make-type-in-gesture #\t)
                       :domain "JSON" :description "Replaces the selected element with true"
                       :operation (make-operation/replace-target printer-input nil (json/boolean (:selection '((the string (true-value-of (the json/boolean document)))
                                                                                                               (the string (subseq (the string document) 0 0))))
                                                                                     #t)))
                      ((make-type-in-gesture #\")
                       :domain "JSON" :description "Replaces the selected element with an empty string"
                       :operation (make-operation/replace-target printer-input nil (json/string (:selection '((the string (value-of (the json/string document)))
                                                                                                              (the string (subseq (the string document) 0 0))))
                                                                                     "")))
                      ((make-type-in-gesture #\[)
                       :domain "JSON" :description "Replaces the selected element with an empty array"
                       :operation (make-operation/replace-target printer-input nil (json/array (:selection '((the sequence (elements-of (the json/array document)))
                                                                                                             (the json/insertion (elt (the sequence document) 0))
                                                                                                             (the string (value-of (the json/insertion document)))
                                                                                                             (the string (subseq (the string document) 0 0))))
                                                                                     (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                                                                                   (the string (subseq (the string document) 0 0))))))))
                      ((make-type-in-gesture #\:)
                       :domain "JSON" :description "Replaces the selected element with an object entry"
                       :operation (make-operation/replace-target printer-input nil (json/object-entry (:selection '((the string (key-of (the json/object-entry document)))
                                                                                                                    (the string (subseq (the string document) 0 0))))
                                                                                                      ""
                                                                                                      (json/insertion ()))))
                      ((make-type-in-gesture #\{)
                       :domain "JSON" :description "Replaces the selected element with an empty object"
                       :operation (make-operation/replace-target printer-input nil (make-json/object (make-document/sequence (list-ll (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                                                                                                                                    (the string (subseq (the string document) 0 0))))))
                                                                                                                             :selection '((the json/insertion (elt (the sequence document) 0))
                                                                                                                                          (the string (value-of (the json/insertion document)))
                                                                                                                                          (the string (subseq (the string document) 0 0))))
                                                                                                     :selection '((the sequence (entries-of (the json/object document)))
                                                                                                                  (the json/insertion (elt (the sequence document) 0))
                                                                                                                  (the string (value-of (the json/insertion document)))
                                                                                                                  (the string (subseq (the string document) 0 0)))))))
                    (when (and (not (typep printer-input 'json/number))
                               (typep gesture 'gesture/keyboard/type-in)
                               (digit-char-p (character-of gesture)))
                      (make-command gesture
                                    (make-operation/replace-target printer-input nil (json/number (:selection '((the document/number (value-of (the json/number document)))
                                                                                                                (the string (write-to-string (the document/number document)))
                                                                                                                (the string (subseq (the string document) 1 1))))
                                                                                       (parse-integer (string (character-of gesture)))))
                                    :domain "JSON"
                                    :description "Replaces the selected element with a number")))))

(def reader json/insertion->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                ;; TODO: do nothing
                                (make-operation/functional (lambda ())))))))
    (merge-commands (json/read-command printer-iomap (gesture-of input))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/insertion->tree/leaf operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/null->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range nil)))))
    (merge-commands (json/read-command printer-iomap (gesture-of input))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/null->tree/leaf operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/boolean->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (json/read-command printer-iomap (gesture-of input))
                  (command/read-backward recursion input printer-iomap 'backward-mapper/json/boolean->tree/leaf nil)
                  (make-nothing-command (gesture-of input))))

(def reader json/number->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the document/number (value-of (the json/number document)))
                                    (the string (write-to-string (the document/number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range printer-input selection (replacement-of operation))))
                                  (((the tree/leaf (printer-output (the json/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range printer-input
                                                                          `((the document/number (value-of (the json/number document)))
                                                                            (the string (write-to-string (the document/number document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation))))))))))
    (merge-commands (json/read-command printer-iomap (gesture-of input))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/number->tree/leaf operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (value-of (the json/string document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the json/string document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range printer-input
                                                                        '((the string (value-of (the json/string document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/json/string->tree/leaf operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/array->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         ;; TODO: make this reusable and generic
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/show-annotation
                                (pattern-case selection
                                  (((the tree/node (printer-output (the json/array document) ?projection ?recursion)) . ?rest)
                                   (make-instance 'operation/show-annotation
                                                  :document (document-of operation)
                                                  :selection '((the json/array document))))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/json/array->tree/node 'backward-mapper/json/array->tree/node)
                    (gesture-case (gesture-of input)
                      ((make-type-in-gesture #\,)
                       :domain "JSON" :description "Starts a JSON object insertion into the elements of the JSON array"
                       :operation (bind ((index (length (elements-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input
                                                                                                          `((the sequence (elements-of (the json/array document)))
                                                                                                            (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                          (list (json/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (elements-of (the json/array document)))
                                                                                                                     (the json/insertion (elt (the sequence document) ,index))
                                                                                                                     (the string (value-of (the json/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((make-key-press-gesture :scancode-insert)
                       :domain "JSON" :description "Starts a generic insertion into the elements of the JSON array"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the json/array document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                          (list (document/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (elements-of (the json/array document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,elements-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/array->tree/node operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/object-entry->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (key-of (the json/object-entry document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/node (printer-output (the json/object-entry document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range printer-input
                                                                        '((the string (key-of (the json/object-entry document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/json/object-entry->tree/node 'backward-mapper/json/object-entry->tree/node)
                    (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-tab)
                       :domain "JSON" :description "Moves the selection to the value of the JSON object entry"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the string (key-of (the json/object-entry document)))
                                      (the string (subseq (the string document) ?start-index ?end-index)))
                                     ;; TODO: this is quite fragile
                                     (bind ((command (recurse-reader recursion (make-command (gesture-of input) (make-operation/replace-selection (value-of printer-input) `((the text/text (content-of (the tree/leaf document)))
                                                                                                                                                                             (the text/text (text/subseq (the text/text document) 0 0))))
                                                                                             :domain "JSON")
                                                                     (content-iomap-of printer-iomap)))
                                            (operation (operation-of command)))
                                       (when (typep operation 'operation/replace-selection)
                                         (make-operation/replace-selection printer-input (append `((the ,(document-type (value-of printer-input)) (value-of (the json/object-entry document)))) (selection-of operation)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/object-entry->tree/node operation-mapper)
                    (make-nothing-command (gesture-of input)))))

(def reader json/object->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/json/object->tree/node 'backward-mapper/json/object->tree/node)
                    (gesture-case (gesture-of input)
                      ((make-type-in-gesture #\,)
                       :domain "JSON" :description "Inserts a new entry into the entries of the JSON object"
                       :operation (bind ((index (length (entries-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input
                                                                                                          `((the sequence (entries-of (the json/object document)))
                                                                                                            (the sequence (subseq (the sequence document) ,index ,index)))
                                                                                                          (list (json/object-entry () "" (json/insertion ()))))
                                                                   (make-operation/replace-selection printer-input `((the sequence (entries-of (the json/object document)))
                                                                                                                     (the json/object-entry (elt (the sequence document) ,index))
                                                                                                                     (the string (key-of (the json/object-entry document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((make-key-press-gesture :scancode-insert)
                       :domain "JSON" :description "Starts a generic insertion into the entries of the JSON object"
                       :operation (bind ((entries-length (length (entries-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (entries-of (the json/object document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,entries-length ,entries-length)))
                                                                                                          (list (document/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (entries-of (the json/object document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,entries-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/json/object->tree/node nil)
                    (make-nothing-command (gesture-of input)))))
