;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/null->text/text ()
  ())

(def projection t/number->text/text ()
  ())

(def projection t/string->text/text ()
  ())

(def projection t/symbol->text/text ()
  ())

(def projection t/sequence->table/table ()
  ())

(def projection t/hash-table->table/table ()
  ())

(def projection t/function->table/table ()
  ())

(def projection t/object->table/table ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/t/null->text/text ()
  (make-projection 't/null->text/text))

(def (function e) make-projection/t/number->text/text ()
  (make-projection 't/number->text/text))

(def (function e) make-projection/t/string->text/text ()
  (make-projection 't/string->text/text))

(def (function e) make-projection/t/symbol->text/text ()
  (make-projection 't/symbol->text/text))

(def (function e) make-projection/t/sequence->table/table ()
  (make-projection 't/sequence->table/table))

(def (function e) make-projection/t/hash-table->table/table ()
  (make-projection 't/hash-table->table/table))

(def (function e) make-projection/t/function->table/table ()
  (make-projection 't/function->table/table))

(def (function e) make-projection/t/object->table/table ()
  (make-projection 't/object->table/table))

;;;;;;
;;; Construction

(def (macro e) t/null->text/text ()
  '(make-projection/t/null->text/text))

(def (macro e) t/number->text/text ()
  '(make-projection/t/number->text/text))

(def (macro e) t/string->text/text ()
  '(make-projection/t/string->text/text))

(def (macro e) t/symbol->text/text ()
  '(make-projection/t/symbol->text/text))

(def (macro e) t/sequence->table/table ()
  '(make-projection/t/sequence->table/table))

(def (macro e) t/hash-table->table/table ()
  '(make-projection/t/hash-table->table/table))

(def (macro e) t/function->table/table ()
  '(make-projection/t/function->table/table))

(def (macro e) t/object->table/table ()
  '(make-projection/t/object->table/table))

;;;;;;
;;; Printer

(def printer t/null->text/text (projection recursion input input-reference)
  (bind ((output (make-text/text (list (make-text/string "NIL" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))))
    (make-iomap/object projection recursion input input-reference output nil)))

(def printer t/number->text/text (projection recursion input input-reference)
  (bind ((output (make-text/text (list (make-text/string (write-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/object projection recursion input input-reference output nil)))

(def printer t/string->text/text (projection recursion input input-reference)
  (bind ((output (text/text ()
                   (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                   (text/string input :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                   (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output nil
                         #+nil
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               (make-iomap/text projection recursion
                                                input input-reference 0
                                                output output-reference 1
                                                (length input))))))

(def printer t/symbol->text/text (projection recursion input input-reference)
  (bind ((output (make-text/text (list (make-text/string (symbol-name input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/object projection recursion input input-reference output nil)))

(def iomap iomap/sequence->table/table (iomap)
  ((element-iomaps :type sequence)))

(def reference-applier iomap/sequence->table/table (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

(def forward-mapper iomap/sequence->table/table (iomap input-reference function)
  (pattern-case input-reference
    (((the ?type (elt (the list document) ?index)) . ?rest)
     (map-forward (elt (element-iomaps-of iomap) ?index)
                  ?rest
                  (lambda (element-iomap output-reference)
                    (funcall function element-iomap `((the text/text (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table document))) ,(1+ ?index))))) 1))))
                                                      ,@output-reference)))))))

(def backward-mapper iomap/sequence->table/table (iomap output-reference function)
  (pattern-case output-reference
    (((the ?type (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table document))) ?row-index)))) ?column-index)))) . ?rest)
     (map-backward (elt (element-iomaps-of iomap) (1- ?row-index))
                   ?rest
                   (lambda (element-iomap input-reference)
                     (funcall function element-iomap `((the ,(form-type (elt (input-of iomap) (1- ?row-index))) (elt (the list document) ,(1- ?row-index))) ,@input-reference)))))))

(def printer t/sequence->table/table (projection recursion input input-reference)
  (bind ((element-iomaps nil)
         (child-iomaps nil)
         (type-label (object-class-label input))
         (type-label-text (make-text/text (list (make-text/string type-label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
         (class-name (if (consp input) "LIST" "SEQUENCE"))
         (type-text (make-text/text (list (make-text/string class-name :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
         #+nil ;; TODO: move to inspector
         (output-selection (pattern-case (selection-of input)
                             (((the sequence-position (pos (the string document) ?character-index))
                               (the string (object-class-label (the sequence document))))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the table/cell document)))
                                (the table/cell (elt (the list document) 0))
                                (the list (cells-of (the table/row document)))
                                (the table/row (elt (the list document) 0))
                                (the list (rows-of (the table/table document)))))
                             (((the sequence-position (pos (the string document) ?character-index))
                               (the string (object-class-symbol-name (the sequence document))))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the table/cell document)))
                                (the table/cell (elt (the list document) 1))
                                (the list (cells-of (the table/row document)))
                                (the table/row (elt (the list document) 0))
                                (the list (rows-of (the table/table document)))))))
         (output (make-table/table (list* (make-table/row (list (make-table/cell type-label-text) (make-table/cell type-text)))
                                          (iter (for index :from 0)
                                                (for element :in-sequence input)
                                                (for element-iomap = (recurse-printer recursion (elt input index)
                                                                                      `((elt (the ,(form-type input) document) ,index)
                                                                                        ,@(typed-reference (form-type input) input-reference))))
                                                (for index-string = (write-to-string index))
                                                (for index-text = (make-text/text (list (make-text/string index-string :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                #+nil
                                                (push (make-iomap/text projection recursion
                                                                       index-string `((write-to-string (the integer ,index)) ,@(typed-reference (form-type input) input-reference)) 0
                                                                       index-text `((content-of (the table/cell document))
                                                                                    (the table/cell (elt (the list document) 0))
                                                                                    (the list (cells-of (the table/row document)))
                                                                                    (the table/row (elt (the list document) ,(1+ index)))
                                                                                    (the list (rows-of (the table/table document)))
                                                                                    ,@(typed-reference 'table/table output-reference)) 0
                                                                                    (length index-string))
                                                      child-iomaps)
                                                (push element-iomap element-iomaps)
                                                (collect (make-table/row (list (make-table/cell index-text)
                                                                               (make-table/cell (output-of element-iomap))))))))))
    #+nil
    (push (make-iomap/text projection recursion
                           type-label `(,type-label ,@(typed-reference (form-type input) input-reference)) 0
                           type-label-text `((content-of (the table/cell document))
                                             (the table/cell (elt (the list document) 0))
                                             (the list (cells-of (the table/row document)))
                                             (the table/row (elt (the list document) 0))
                                             (the list (rows-of (the table/table document)))
                                             ,@(typed-reference 'table/table output-reference)) 0
                                             (length type-label))
          child-iomaps)
    #+nil
    (push (make-iomap/text projection recursion
                           class-name `((class-name (the ,(form-type input) document)) ,@(typed-reference (form-type input) input-reference)) 0
                           type-text `((content-of (the table/cell document))
                                       (the table/cell (elt (the list document) 1))
                                       (the list (cells-of (the table/row document)))
                                       (the table/row (elt (the list document) 0))
                                       (the list (rows-of (the table/table document)))
                                       ,@(typed-reference 'table/table output-reference)) 0
                                       (length class-name))
          child-iomaps)
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output nil))
                                 (nreverse child-iomaps)
                                 (nreverse element-iomaps)))))

(def printer t/hash-table->table/table (projection recursion input input-reference)
  ;; TODO:
  (bind ((output (make-text/text (list (make-text/string "hash table" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/function->table/table (projection recursion input input-reference)
  (bind ((output (make-text/text (list (make-text/string "function" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def iomap iomap/object->table/table (iomap)
  ((slot-iomaps :type sequence)))

(def reference-applier iomap/object->table/table (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

(def forward-mapper iomap/object->table/table (iomap input-reference function)
  (pattern-case input-reference
    (((the ?type (slot-value (the ?a document) '?slot-name)) . ?rest)
     (bind ((slot-index (position ?slot-name (class-slots (class-of (input-of iomap))) :key 'slot-definition-name)))
       (map-forward (elt (slot-iomaps-of iomap) slot-index)
                    ?rest
                    (lambda (element-iomap output-reference)
                      (funcall function element-iomap `((the text/text (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table document))) ,(1+ slot-index))))) 1))))
                                                        ,@output-reference))))))))

(def backward-mapper iomap/object->table/table (iomap output-reference function)
  (pattern-case output-reference
    (((the ?type (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table document))) ?row-index)))) ?column-index)))) . ?rest)
     (bind ((input (input-of iomap))
            (slot-name (slot-definition-name (elt (class-slots (class-of input)) (1- ?row-index)))))
       (map-backward (elt (slot-iomaps-of iomap) (1- ?row-index))
                     ?rest
                     (lambda (element-iomap input-reference)
                       (funcall function element-iomap `((the ,(form-type (slot-value input slot-name)) (slot-value (the ,(form-type input) document) ',slot-name)) ,@input-reference))))))))

(def function object-class-label (object)
  (declare (ignore object))
  "TYPE")

(def printer t/object->table/table (projection recursion input input-reference)
  (bind ((slot-iomaps nil)
         (child-iomaps nil)
         (class-name (class-name (class-of input)))
         (type-label (object-class-label input))
         (type-label-text (make-text/text (list (make-text/string type-label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
         (type-text (make-text/text (list (make-text/string (symbol-name class-name) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
         (output (make-table/table (list* (make-table/row (list (make-table/cell type-label-text) (make-table/cell type-text)))
                                          (iter (with class = (class-of input))
                                                (for index :from 0)
                                                (for slot :in (class-slots class))
                                                (for slot-name = (slot-definition-name slot))
                                                (for slot-bound? = (slot-boundp-using-class class input slot))
                                                (for slot-value = (when slot-bound? (slot-value-using-class class input slot)))
                                                ;; KLUDGE: allow filtering slots
                                                (when (and (not (eq (slot-definition-name slot) 'raw))
                                                           (or (not slot-bound?)
                                                               (typep slot-value '(or number string symbol sequence standard-object))))
                                                  (for slot-name-text = (make-text/text (list (make-text/string (symbol-name slot-name) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                  (when slot-bound?
                                                    (for slot-iomap = (recurse-printer recursion slot-value
                                                                                       `((slot-value (the ,(form-type input) document) ',slot-name)
                                                                                         ,@(typed-reference (form-type input) input-reference))))
                                                    #+nil
                                                    (push (make-iomap/text projection recursion
                                                                           (symbol-name slot-name) `((slot-name (the ,(form-type input) document) ',slot-name)
                                                                                                     ,@(typed-reference (form-type input) input-reference)) 0
                                                                                                     slot-name-text `((content-of (the table/cell document))
                                                                                                                      (the table/cell (elt (the list document) 0))
                                                                                                                      (the list (cells-of (the table/row document)))
                                                                                                                      (the table/row (elt (the list document) ,(1+ index)))
                                                                                                                      (the list (rows-of (the table/table document)))
                                                                                                                      ,@(typed-reference 'table/table output-reference)) 0
                                                                                                                      (length (symbol-name slot-name)))
                                                          child-iomaps)
                                                    (push slot-iomap slot-iomaps))
                                                  (collect (make-table/row (list (make-table/cell slot-name-text)
                                                                                 (make-table/cell (if slot-bound?
                                                                                                      (output-of slot-iomap)
                                                                                                      (make-text/text (list (make-text/string "<unbound>" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))))))))))))
    #+nil
    (push (make-iomap/text projection recursion
                           type-label `(,type-label ,@(typed-reference (form-type input) input-reference)) 0
                           type-label-text `((content-of (the table/cell document))
                                             (the table/cell (elt (the list document) 0))
                                             (the list (cells-of (the table/row document)))
                                             (the table/row (elt (the list document) 0))
                                             (the list (rows-of (the table/table document)))
                                             ,@(typed-reference 'table/table output-reference)) 0
                                             (length type-label))
          child-iomaps)
    #+nil
    (push (make-iomap/text projection recursion
                           (symbol-name class-name) `((class-name (the ,(form-type input) document)) ,@(typed-reference (form-type input) input-reference)) 0
                           type-text `((content-of (the table/cell document))
                                       (the table/cell (elt (the list document) 1))
                                       (the list (cells-of (the table/row document)))
                                       (the table/row (elt (the list document) 0))
                                       (the list (rows-of (the table/table document)))
                                       ,@(typed-reference 'table/table output-reference)) 0
                                       (length (symbol-name class-name)))
          child-iomaps)
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output nil))
                                 (nreverse child-iomaps)
                                 (nreverse slot-iomaps)))))

;;;;;;
;;; Reader

(def reader t/null->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/number->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/string->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/symbol->text/text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/sequence->table/table (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit
                  operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (selection-of operation)
                                                      (((the sequence-position (text/pos (the text/text document) ?character-index))
                                                        (the text/text (content-of (the table/cell document)))
                                                        (the table/cell (elt (the list document) ?cell-index))
                                                        (the list (cells-of (the table/row document)))
                                                        (the table/row (elt (the list document) ?row-index))
                                                        (the list (rows-of (the table/table document))))
                                                       (if (zerop ?row-index)
                                                           (ecase ?cell-index
                                                             (0 `((the sequence-position (pos (the string document) ,?character-index))
                                                                  (the string (object-class-label (the sequence document)))))
                                                             (1 `((the sequence-position (pos (the string document) ,?character-index))
                                                                  (the string (object-class-symbol-name (the sequence document)))))))))))
                 (operation/sequence/replace-element-range)
                 (operation/describe)
                 (operation/compound
                  (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null child-operations)
                      (make-operation/compound child-operations)))))))
      (recurse operation))))

(def reader t/hash-table->table/table (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/function->table/table (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))

(def reader t/object->table/table (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  (operation/read-backward operation projection-iomap))
