;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/sequence->table/table ()
  ())

(def projection t/hash-table->table/table ()
  ())

(def projection t/function->table/table ()
  ())

(def projection t/object->table/table ()
  ((slot-provider :type function)))

;;;;;;
;;; Construction

(def function make-projection/t/sequence->table/table ()
  (make-projection 't/sequence->table/table))

(def function make-projection/t/hash-table->table/table ()
  (make-projection 't/hash-table->table/table))

(def function make-projection/t/function->table/table ()
  (make-projection 't/function->table/table))

(def function make-projection/t/object->table/table (&key slot-provider)
  (make-projection 't/object->table/table :slot-provider (or slot-provider (compose 'class-slots 'class-of))))

;;;;;;
;;; Construction

(def macro t/sequence->table/table ()
  '(make-projection/t/sequence->table/table))

(def macro t/hash-table->table/table ()
  '(make-projection/t/hash-table->table/table))

(def macro t/function->table/table ()
  '(make-projection/t/function->table/table))

(def macro t/object->table/table (&key slot-provider)
  `(make-projection/t/object->table/table :slot-provider ,slot-provider))

;;;;;;
;;; IO map

(def iomap iomap/sequence->table/table ()
  ((element-iomaps :type sequence)))


(def iomap iomap/object->table/table ()
  ((slot-iomaps :type sequence)))

;;;;;;
;;; Printer

(def function object-class-label (object)
  (declare (ignore object))
  "TYPE")

(def printer t/sequence->table/table (projection recursion input input-reference)
  (bind ((element-iomaps nil)
         (child-iomaps nil)
         #+nil ;; TODO: move to inspector
         (output-selection (pattern-case (selection-of input)
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (object-class-label (the sequence document))))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the table/cell document)))
                                (the table/cell (elt (the sequence document) 0))
                                (the sequence (cells-of (the table/row document)))
                                (the table/row (elt (the sequence document) 0))
                                (the sequence (rows-of (the table/table document)))))
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (object-class-symbol-name (the sequence document))))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the table/cell document)))
                                (the table/cell (elt (the sequence document) 1))
                                (the sequence (cells-of (the table/row document)))
                                (the table/row (elt (the sequence document) 0))
                                (the sequence (rows-of (the table/table document)))))))
         (output (make-table/table (list* (table/row ()
                                            (table/cell ()
                                              (text/text ()
                                                (text/string (object-class-label input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                            (table/cell ()
                                              (text/text ()
                                                (text/string (if (consp input) "LIST" "SEQUENCE") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
                                          (iter (for index :from 0)
                                                (for element :in-sequence input)
                                                (for element-iomap = (recurse-printer recursion (elt input index)
                                                                                      `((elt (the ,(form-type input) document) ,index)
                                                                                        ,@(typed-reference (form-type input) input-reference))))
                                                (for index-string = (write-to-string index))
                                                (for index-text = (text/text ()
                                                                    (text/string index-string :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                                (push element-iomap element-iomaps)
                                                (collect (table/row ()
                                                           (table/cell ()
                                                             index-text)
                                                           (table/cell ()
                                                             (output-of element-iomap)))))))))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output))
                                 (nreverse child-iomaps)
                                 (nreverse element-iomaps)))))

(def printer t/hash-table->table/table (projection recursion input input-reference)
  ;; TODO:
  (bind ((output (text/text () (text/string "hash table" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/function->table/table (projection recursion input input-reference)
  (bind ((output (text/text () (text/string "function" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/object->table/table (projection recursion input input-reference)
  (bind ((slot-iomaps nil)
         (child-iomaps nil)
         (output (make-table/table (list* (table/row ()
                                            (table/cell ()
                                              (text/text ()
                                                (text/string (object-class-label input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                            (table/cell ()
                                              (text/text ()
                                                (text/string (symbol-name (class-name (class-of input))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
                                          (iter (with class = (class-of input))
                                                (for index :from 0)
                                                (for slot :in (funcall (slot-provider-of projection) input))
                                                (for slot-reader = (find-slot-reader class slot))
                                                (for slot-bound? = (slot-boundp-using-class class input slot))
                                                (for slot-value = (when slot-bound? (slot-value-using-class class input slot)))
                                                (when slot-bound?
                                                  (for slot-iomap = (recurse-printer recursion slot-value
                                                                                     `((,slot-reader (the ,(form-type input) document))
                                                                                       ,@(typed-reference (form-type input) input-reference))))
                                                  (push slot-iomap slot-iomaps))
                                                (collect (table/row ()
                                                           (table/cell ()
                                                             (text/text ()
                                                               (text/string (symbol-name (slot-definition-name slot)) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                                           (table/cell ()
                                                             (if slot-bound?
                                                                 (output-of slot-iomap)
                                                                 (text/text ()
                                                                   (text/string "<unbound>" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))))))))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output))
                                 (nreverse child-iomaps)
                                 (nreverse slot-iomaps)))))

;;;;;;
;;; Reader

(def reader t/sequence->table/table (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/functional operation)
                 (operation/replace-selection
                  (make-operation/replace-selection printer-input
                                                    (pattern-case (selection-of operation)
                                                      (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                                        (the text/text (content-of (the table/cell document)))
                                                        (the table/cell (elt (the sequence document) ?cell-index))
                                                        (the sequence (cells-of (the table/row document)))
                                                        (the table/row (elt (the sequence document) ?row-index))
                                                        (the sequence (rows-of (the table/table document))))
                                                       (if (zerop ?row-index)
                                                           (ecase ?cell-index
                                                             (0 `((the string (subseq (the string document) ,?character-index ,?character-index))
                                                                  (the string (object-class-label (the sequence document)))))
                                                             (1 `((the string (subseq (the string document) ,?character-index ,?character-index))
                                                                  (the string (object-class-symbol-name (the sequence document)))))))))))
                 (operation/sequence/replace-element-range)
                 (operation/describe)
                 (operation/compound
                  (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null child-operations)
                      (make-operation/compound child-operations)))))))
      (recurse (operation-of input)))))

(def reader t/hash-table->table/table (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader t/function->table/table (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader t/object->table/table (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
