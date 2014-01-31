;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection inspector/object->table/table ()
  ())

(def projection inspector/object-slot->table/row ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/inspector/object->table/table ()
  (make-projection 'inspector/object->table/table))

(def (function e) make-projection/inspector/object-slot->table/row ()
  (make-projection 'inspector/object-slot->table/row))

;;;;;;
;;; Construction

(def (macro e) inspector/object->table/table ()
  '(make-projection/inspector/object->table/table))

(def (macro e) inspector/object-slot->table/row ()
  '(make-projection/inspector/object-slot->table/row))

;;;;;;
;;; Printer

(def printer inspector/object->table/table (projection recursion input input-reference)
  (bind ((instance (instance-of input))
         (expanded? (expanded-p input))
         (class-name (class-name (class-of instance)))
         (type-label "TYPE")
         (type-label-text (make-text/text (list (make-text/string type-label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
         (type-text (make-text/text (list (make-text/string (symbol-name class-name) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
         (output (if expanded?
                     (make-table/table (list* (make-table/row (list (make-table/cell type-label-text) (make-table/cell type-text)))
                                              (iter (for index :from 0)
                                                    (for slot-value :in-sequence (slot-values-of input))
                                                    (for slot-value-iomap = (recurse-printer recursion slot-value
                                                                                             `((elt (the sequence document) ,index)
                                                                                               (the sequence (slot-values-of (the inspector/object document)))
                                                                                               ,@(typed-reference (form-type input) input-reference))))
                                                    (collect (output-of slot-value-iomap)))))
                     (text/text () (text/string (write-to-string instance))))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer inspector/object-slot->table/row (projection recursion input input-reference)
  (bind ((slot (slot-of input))
         (slot-name (slot-definition-name slot))
         (value-iomap (recurse-printer recursion (value-of input)
                                       `((value-of (the inspector/object-slot document))
                                         ,@(typed-reference (form-type input) input-reference))))
         (output (table/row ()
                   (table/cell ()
                     (make-text/text (list (make-text/string (symbol-name slot-name) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                   (table/cell ()
                     (output-of value-iomap)))))
    (make-iomap/compound projection recursion input input-reference output nil)))

;;;;;;
;;; Reader

(def reader inspector/object->table/table (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (operation/read-backward (operation-of input) printer-iomap))

(def reader inspector/object-slot->table/row (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (operation/read-backward (operation-of input) printer-iomap))
