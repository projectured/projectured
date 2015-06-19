;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/sequence->tree/node ()
  ())

(def projection t/object->tree/node ()
  ((slot-provider :type function)))

;;;;;;
;;; Construction

(def function make-projection/t/sequence->tree/node ()
  (make-projection 't/sequence->tree/node))

(def function make-projection/t/object->tree/node (&key slot-provider)
  (make-projection 't/object->tree/node :slot-provider (or slot-provider (compose 'class-slots 'class-of))))

;;;;;;
;;; Construction

(def macro t/sequence->tree/node ()
  '(make-projection/t/sequence->tree/node))

(def macro t/object->tree/node (q&key slot-provider)
  `(make-projection/t/object->tree/node :slot-provider ,slot-provider))

;;;;;;
;;; Forward mapper

(def function forward-mapper/t/sequence->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence document))
       `((the tree/node document)))
      (((the string (elt (the sequence document) ?index))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/node (elt (the sequence document) ,(1+ ?index)))
         (the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 1))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the ?element-type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?index)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the tree/node (elt (the sequence document) ,(1+ ?index)))
                   (the sequence (children-of (the tree/node document)))
                   (the ,(form-type (output-of element-iomap)) (elt (the sequence document) 1)))
                 ?rest
                 element-iomap)))
      (((the tree/node (printer-output (the sequence document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/t/object->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap))
         (class (class-of printer-input))
         (slots (funcall (slot-provider-of projection) printer-input))
         (slot-readers (mapcar (curry 'find-slot-reader class) slots)))
    (pattern-case reference
      (((the ?type document))
       `((the tree/node document)))
      (((the string (?slot-reader (the ?input-type document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (bind ((index (position ?slot-reader slot-readers)))
         (when index
           `((the sequence (children-of (the tree/node document)))
             (the tree/node (elt (the sequence document) ,(1+ index)))
             (the sequence (children-of (the tree/node document)))
             (the tree/leaf (elt (the sequence document) 1))
             (the text/text (content-of (the tree/leaf document)))
             (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))
      (((the number (?slot-reader (the ?input-type document)))
        (the string (write-to-string (the number document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (bind ((index (position ?slot-reader slot-readers)))
         (when index
           `((the sequence (children-of (the tree/node document)))
             (the tree/node (elt (the sequence document) ,(1+ index)))
             (the sequence (children-of (the tree/node document)))
             (the tree/leaf (elt (the sequence document) 1))
             (the text/text (content-of (the tree/leaf document)))
             (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))
      (((the ?slot-value-type (?slot-reader (the ?input-type document)))
        . ?rest)
       (bind ((index (position ?slot-reader slot-readers))
              (slot-iomap (elt (child-iomaps-of printer-iomap) index)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the tree/node (elt (the sequence document) ,(1+ index)))
                   (the sequence (children-of (the tree/node document)))
                   (the ,(form-type (output-of slot-iomap)) (elt (the sequence document) 1)))
                 ?rest
                 slot-iomap)))
      (((the tree/node (printer-output (the ?type document) ?projection ?recursion)) . ?rest)
       (when (and (eq ?type (form-type printer-input)) (eq projection ?projection))
         ?rest)))))

;;;;;;
;;; Forward mapper

(def function backward-mapper/t/sequence->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       `((the sequence document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?index))
        (the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 1))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (elt (the sequence document) ,(1- ?index)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?index))
        (the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) 1))
        . ?rest)
       (bind ((index (1- ?index))
              (element-iomap (elt (child-iomaps-of printer-iomap) index)))
         (values `((the ,(form-type (input-of element-iomap)) (elt (the sequence document) ,index)))
                 ?rest
                 element-iomap)))
      (?a
       (append `((the tree/node (printer-output (the sequence document) ,projection ,recursion))) reference)))))

(def function backward-mapper/t/object->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       `((the ,(form-type printer-input) document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?child-index))
        (the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 1))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (> ?child-index 0)
           (bind ((slots (funcall (slot-provider-of projection) printer-input))
                  (slot-index (- ?child-index 1))
                  (slot-reader (find-slot-reader (class-of printer-input) (elt slots slot-index)))
                  (slot-value (input-of (elt (child-iomaps-of printer-iomap) slot-index))))
             (typecase slot-value
               (string
                (if (string= slot-value "")
                    (append `((the tree/node (printer-output (the ,(form-type printer-input)  document) ,projection ,recursion))) reference)
                    `((the string (,slot-reader (the ,(form-type printer-input) document)))
                      (the string (subseq (the string document) ,?start-index ,?end-index)))))
               (number
                `((the number (,slot-reader (the ,(form-type printer-input) document)))
                  (the string (write-to-string (the number document)))
                  (the string (subseq (the string document) ,?start-index ,?end-index))))
               (t
                (append `((the tree/node (printer-output (the ,(form-type printer-input)  document) ,projection ,recursion))) reference))))
           (append `((the tree/node (printer-output (the ,(form-type printer-input)  document) ,projection ,recursion))) reference)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?index))
        (the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) 1))
        . ?rest)
       (bind ((index (1- ?index))
              (slots (funcall (slot-provider-of projection) printer-input))
              (slot (elt slots index))
              (slot-reader (find-slot-reader (class-of printer-input) slot))
              (slot-iomap (elt (child-iomaps-of printer-iomap) index))
              (slot-value (input-of slot-iomap)))
         (values `((the ,(form-type slot-value) (,slot-reader (the ,(form-type printer-input) document))))
                 ?rest
                 slot-iomap)))
      (?a
       (append `((the tree/node (printer-output (the ,(form-type printer-input) document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer t/sequence->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence input)
                                   (collect (recurse-printer recursion element
                                                             `((elt (the ,(form-type input) document) ,index)
                                                               ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (when (typep input 'document)
                                 (print-selection (make-iomap/compound projection recursion input input-reference nil element-iomaps)
                                                  (selection-of input)
                                                  'forward-mapper/t/sequence->tree/node))))
         (output (as (if (emptyp input)
                         (tree/leaf (:selection output-selection)
                           (text/text (:selection (as (nthcdr 1 (va output-selection))))
                             (text/string "")))
                         (make-tree/node (list* (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                  (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                    (text/string "SEQUENCE" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*)))
                                                (iter (for index :from 0)
                                                      (for element-iomap :in (va element-iomaps))
                                                      (for element-iomap-output = (output-of element-iomap))
                                                      (collect (tree/node (:selection (as (nthcdr 2 (va output-selection))) :indentation 1 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24*)))
                                                                 (tree/leaf (:selection (as (nthcdr 4 (va output-selection))))
                                                                   (text/text (:selection (as (nthcdr 5 (va output-selection))))
                                                                     (text/string (write-to-string index) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                                                 (tree/clone element-iomap-output :selection (as (nthcdr 4 (va output-selection))))))))
                                         :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24*))
                                         :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer t/object->tree/node (projection recursion input input-reference)
  (bind ((class (class-of input))
         (slots (funcall (slot-provider-of projection) input))
         (slot-readers (mapcar (curry 'find-slot-reader class) slots))
         (slot-iomaps (as (iter (for slot :in slots)
                                (for slot-reader :in slot-readers)
                                (for slot-reference = `((,slot-reader (the ,(form-type input) document))
                                                        ,@(typed-reference (form-type input) input-reference)))
                                (collect (if (slot-boundp-using-class class input slot)
                                             (recurse-printer recursion (slot-value-using-class class input slot) slot-reference)
                                             (make-iomap/object recursion recursion nil slot-reference (tree/leaf ()
                                                                                                         (text/make-simple-text "<unbound>" :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/gray*))))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil slot-iomaps)
                                                (selection-of input)
                                                'forward-mapper/t/object->tree/node)))
         (output (as (make-tree/node (list* (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                              (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                (text/string (symbol-name (class-name (class-of input))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*)))
                                            (iter (for slot :in slots)
                                                  (for slot-iomap :in (va slot-iomaps))
                                                  (for slot-iomap-output = (output-of slot-iomap))
                                                  (collect (tree/node (:selection (as (nthcdr 2 (va output-selection))) :separator (text/make-simple-text " " :font *font/ubuntu/monospace/regular/24*) :indentation 1)
                                                             (tree/leaf (:selection (as (nthcdr 4 (va output-selection))))
                                                               (text/text (:selection (as (nthcdr 5 (va output-selection))))
                                                                 (text/string (symbol-name (slot-definition-name slot)) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/cyan*)))
                                                             (tree/clone slot-iomap-output :selection (as (nthcdr 4 (va output-selection))) :indentation 1)))))
                                     :collapsed (when (find-slot (class-of input) 'collapsed :otherwise nil)
                                                  (as (collapsed-p input)))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output slot-iomaps)))

;;;;;;
;;; Reader

(def reader t/sequence->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (elt (the sequence document) ?index))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/t/sequence->tree/node 'backward-mapper/t/sequence->tree/node)
                    (command/read-backward recursion input printer-iomap 'backward-mapper/t/sequence->tree/node operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader t/object->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (?reader (the ?type document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the number (?reader (the ?type document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range printer-input selection (replacement-of operation))))
                                  (((the tree/node (printer-output (the ?type document) ?projection ?recursion))
                                    (the sequence (children-of (the tree/node document)))
                                    (the tree/node (elt (the sequence document) ?child-index))
                                    (the sequence (children-of (the tree/node document)))
                                    (the tree/leaf (elt (the sequence document) 1))
                                    (the text/text (content-of (the tree/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                   (when (> ?child-index 0)
                                     (bind ((slots (funcall (slot-provider-of projection) printer-input))
                                            (slot-index (- ?child-index 1))
                                            (slot-reader (find-slot-reader (class-of printer-input) (elt slots slot-index))))
                                       (make-operation/sequence/replace-range printer-input
                                                                              `((the string (,slot-reader (the ,?type document)))
                                                                                (the string (subseq (the string document) 0 0)))
                                                                              (replacement-of operation)))))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/t/object->tree/node 'backward-mapper/t/object->tree/node)
                    (command/read-backward recursion input printer-iomap 'backward-mapper/t/object->tree/node operation-mapper)
                    (make-command/nothing (gesture-of input)))))
