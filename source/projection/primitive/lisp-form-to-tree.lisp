;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) lisp-form/number->string ()
  ())

(def (projection e) lisp-form/symbol->string ()
  ())

(def (projection e) lisp-form/string->string ()
  ())

(def (projection e) lisp-form/cons->tree/node ()
  ())

(def (projection e) lisp-form/object->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/lisp-form/number->string ()
  (make-projection 'lisp-form/number->string))

(def (function e) make-projection/lisp-form/symbol->string ()
  (make-projection 'lisp-form/symbol->string))

(def (function e) make-projection/lisp-form/string->string ()
  (make-projection 'lisp-form/string->string))

(def (function e) make-projection/lisp-form/cons->tree/node ()
  (make-projection 'lisp-form/cons->tree/node))

(def (function e) make-projection/lisp-form/object->string ()
  (make-projection 'lisp-form/object->string))

;;;;;;
;;; Construction

(def (macro e) lisp-form/number->string ()
  '(make-projection/lisp-form/number->string))

(def (macro e) lisp-form/symbol->string ()
  '(make-projection/lisp-form/symbol->string))

(def (macro e) lisp-form/string->string ()
  '(make-projection/lisp-form/string->string))

(def (macro e) lisp-form/cons->tree/node ()
  '(make-projection/lisp-form/cons->tree/node))

(def (macro e) lisp-form/object->tree/node ()
  '(make-projection/lisp-form/object->string))

;;;;;;
;;; Printer

(def printer lisp-form/number->string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (write-to-string (value-of input))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (write-to-string (the number (value-of ,typed-input-reference)))) 0
                                                    output `(the string ,output-reference) 0
                                                    (length output))))))

(def printer lisp-form/symbol->string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (string-downcase (value-of input))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (string-downcase (the symbol (value-of ,typed-input-reference)))) 0
                                                    output `(the string ,output-reference) 0
                                                    (length output))))))

(def printer lisp-form/string->string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (value-of input))
         (output (string+ "\"" value "\"")))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/string* value `(the string (value-of ,typed-input-reference)) 0
                                                    output `(the string ,output-reference) 1
                                                    (length value))
                                (make-iomap/object* projection recursion input `(the string (value-of ,typed-input-reference))
                                                    output `(the string ,output-reference))))))

(def printer lisp-form/cons->tree/node (projection recursion input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in (elements-of input))
                                       (for iomap = (recurse-printer recursion element
                                                                     `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push iomap child-iomaps)
                                       (collect (output-of iomap))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer lisp-form/object->string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (write-to-string input)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (write-to-string (value-of ,typed-input-reference))) 0
                                                    output `(the string ,output-reference) 0
                                                    (length output))))))

;;;;;;
;;; Reader

(def reader lisp-form/number->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/symbol->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/string->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/cons->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/object->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
