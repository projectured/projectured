;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) t/null->string ()
  ())

(def (projection e) t/number->string ()
  ())

(def (projection e) t/string->string ()
  ())

(def (projection e) t/symbol->string ()
  ())

(def (projection e) t/sequence->table/table ()
  ())

(def (projection e) t/hash-table->table/table ()
  ())

(def (projection e) t/function->table/table ()
  ())

(def (projection e) t/object->table ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/t/null->string ()
  (make-projection 't/null->string))

(def (function e) make-projection/t/number->string ()
  (make-projection 't/number->string))

(def (function e) make-projection/t/string->string ()
  (make-projection 't/string->string))

(def (function e) make-projection/t/symbol->string ()
  (make-projection 't/symbol->string))

(def (function e) make-projection/t/sequence->table/table ()
  (make-projection 't/sequence->table/table))

(def (function e) make-projection/t/hash-table->table/table ()
  (make-projection 't/hash-table->table/table))

(def (function e) make-projection/t/function->table/table ()
  (make-projection 't/function->table/table))

(def (function e) make-projection/t/object->table ()
  (make-projection 't/object->table))

;;;;;;
;;; Construction

(def (macro e) t/null->string ()
  '(make-projection/t/null->string))

(def (macro e) t/number->string ()
  '(make-projection/t/number->string))

(def (macro e) t/string->string ()
  '(make-projection/t/string->string))

(def (macro e) t/symbol->string ()
  '(make-projection/t/symbol->string))

(def (macro e) t/sequence->table/table ()
  '(make-projection/t/sequence->table/table))

(def (macro e) t/hash-table->table/table ()
  '(make-projection/t/hash-table->table/table))

(def (macro e) t/function->table/table ()
  '(make-projection/t/function->table/table))

(def (macro e) t/object->table ()
  '(make-projection/t/object->table))

;;;;;;
;;; Printer

(def printer t/null->string (projection recursion iomap input input-reference output-reference)
  (bind ((output "<empty list>"))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/number->string (projection recursion iomap input input-reference output-reference)
  (bind ((output (write-to-string input)))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/string->string (projection recursion iomap input input-reference output-reference)
  (bind ((output (string+ "\"" input "\"")))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/symbol->string (projection recursion iomap input input-reference output-reference)
  (bind ((output (symbol-name input)))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/sequence->table/table (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-table/table (list* (make-table/row (list (make-table/cell "TYPE")
                                                                (make-table/cell (if (consp input)
                                                                                     "LIST"
                                                                                     "SEQUENCE"))))
                                          (iter (for index :from 0)
                                                (for element :in-sequence input)
                                                (collect (make-table/row (list (make-table/cell (write-to-string index))
                                                                               (make-table/cell (output-of (recurse-printer recursion iomap (elt input index)
                                                                                                                            `(elt ,typed-input-reference ,index)
                                                                                                                            `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table ,output-reference))) ,(1+ index))))) 1))))))))))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/hash-table->table/table (projection recursion iomap input input-reference output-reference)
  ;; TODO:
  (bind ((output "hash table"))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/function->table/table (projection recursion iomap input input-reference output-reference)
  (bind ((output "function"))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer t/object->table (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-table/table (list* (make-table/row (list (make-table/cell "TYPE")
                                                                (make-table/cell (symbol-name (class-name (class-of input))))))
                                          (iter (with class = (class-of input))
                                                (for index :from 0)
                                                (for slot :in (class-slots class))
                                                (for slot-name = (slot-definition-name slot))
                                                (collect (make-table/row (list (make-table/cell (symbol-name slot-name))
                                                                               (make-table/cell (if (slot-boundp-using-class class input slot)
                                                                                                    (output-of (recurse-printer recursion iomap (slot-value-using-class class input slot)
                                                                                                                                `(slot-value ,typed-input-reference ,slot-name)
                                                                                                                                `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table ,output-reference))) ,(1+ index))))) 1)))))
                                                                                                    "<unbound>"))))))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

;;;;;;
;;; Reader

(def reader t/null->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/number->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/string->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/symbol->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/sequence->table/table (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/hash-table->table/table (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/function->table/table (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader t/object->table (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def (function e) t-color-provider (iomap reference)
  (block nil
    (map-backward iomap reference
                  (lambda (iomap reference)
                    (declare (ignore iomap))
                    (pattern-case reference
                      ;; class names
                      ((the character (elt (the string (symbol-name (form-type ?a))) ?b))
                       (return (make-style/color 255 0 0 196)))
                      ;; slot names
                      ((the character (elt (the string (symbol-name (slot-definition-name ?a))) ?b))
                       (return (make-style/color 255 196 0 0)))
                      ;; number
                      ((the character (elt (the string (write->string (the ?type (?if (subtypep ?type 'number)) ?a))) ?b))
                       (return (make-style/color 255 0 196 0)))
                      ;; string
                      ((the character (elt (the string (slot-value ?a ?b)) ?c))
                       (return (make-style/color 255 0 196 0))))))))
