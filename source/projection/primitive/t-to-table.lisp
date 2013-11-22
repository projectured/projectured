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

(def printer t/null->text/text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-text/text (list (make-text/string "NIL" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/number->text/text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-text/text (list (make-text/string (write-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/string->text/text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-text/text (list (make-text/string (string+ "\"" input "\"") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/symbol->text/text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-text/text (list (make-text/string (symbol-name input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/sequence->table/table (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-table/table (list* (make-table/row (list (make-table/cell (make-text/text (list (make-text/string "TYPE" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                                (make-table/cell (make-text/text (list (make-text/string (if (consp input)
                                                                                                                                                   "LIST"
                                                                                                                                                   "SEQUENCE")
                                                                                                                                               :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))))
                                          (iter (for index :from 0)
                                                (for element :in-sequence input)
                                                (collect (make-table/row (list (make-table/cell (make-text/text (list (make-text/string (write-to-string index) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                                               (make-table/cell (output-of (recurse-printer recursion iomap (elt input index)
                                                                                                                            `(elt ,typed-input-reference ,index)
                                                                                                                            `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table ,output-reference))) ,(1+ index))))) 1))))))))))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/hash-table->table/table (projection recursion iomap input input-reference output-reference)
  ;; TODO:
  (bind ((output (make-text/text (list (make-text/string "hash table" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/function->table/table (projection recursion iomap input input-reference output-reference)
  (bind ((output (make-text/text (list (make-text/string "function" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/object->table/table (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-table/table (list* (make-table/row (list (make-table/cell (make-text/text (list (make-text/string "TYPE" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                                (make-table/cell (make-text/text (list (make-text/string (symbol-name (class-name (class-of input))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))))
                                          (iter (with class = (class-of input))
                                                (for index :from 0)
                                                (for slot :in (class-slots class))
                                                (for slot-name = (slot-definition-name slot))
                                                ;; KLUDGE: allow filtering slots
                                                (when (and (not (eq (slot-definition-name slot) 'raw))
                                                           (or (not (slot-boundp-using-class class input slot))
                                                               (typep (slot-value-using-class class input slot) '(or number string symbol sequence standard-object))))
                                                  (collect (make-table/row (list (make-table/cell (make-text/text (list (make-text/string (symbol-name slot-name) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                                                 (make-table/cell (if (slot-boundp-using-class class input slot)
                                                                                                      (output-of (recurse-printer recursion iomap (slot-value-using-class class input slot)
                                                                                                                                  `(slot-value ,typed-input-reference ,slot-name)
                                                                                                                                  `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table ,output-reference))) ,(1+ index))))) 1)))))
                                                                                                      (make-text/text (list (make-text/string "<unbound>" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))))))))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

;;;;;;
;;; Reader

(def reader t/null->text/text (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/number->text/text (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/string->text/text (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/symbol->text/text (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/sequence->table/table (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/hash-table->table/table (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/function->table/table (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader t/object->table/table (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
