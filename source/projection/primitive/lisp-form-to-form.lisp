;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection lisp-form/number->number ()
  ())

(def projection lisp-form/string->string ()
  ())

(def projection lisp-form/symbol->symbol ()
  ())

(def projection lisp-form/quote->list ()
  ())

(def projection lisp-form/list->list ()
  ())

(def projection lisp-form/toplevel->list ()
  ())

;;;;;;
;;; Construction

(def function make-projection/lisp-form/number->number ()
  (make-projection 'lisp-form/number->number))

(def function make-projection/lisp-form/string->string ()
  (make-projection 'lisp-form/string->string))

(def function make-projection/lisp-form/symbol->symbol ()
  (make-projection 'lisp-form/symbol->symbol))

(def function make-projection/lisp-form/quote->list ()
  (make-projection 'lisp-form/quote->list))

(def function make-projection/lisp-form/list->list ()
  (make-projection 'lisp-form/list->list))

(def function make-projection/lisp-form/toplevel->list ()
  (make-projection 'lisp-form/toplevel->list))

;;;;;;
;;; Printer

(def printer lisp-form/number->number (projection recursion input input-reference)
  (bind ((output (output-of (recurse-printer recursion (value-of input) `((value-of (the sequence document))
                                                                          ,@(typed-reference (document-type input) input-reference))))))
    (make-iomap projection recursion input input-reference output)))

(def printer lisp-form/string->string (projection recursion input input-reference)
  (bind ((output (output-of (recurse-printer recursion (value-of input) `((value-of (the sequence document))
                                                                          ,@(typed-reference (document-type input) input-reference))))))
    (make-iomap projection recursion input input-reference output)))

(def printer lisp-form/symbol->symbol (projection recursion input input-reference)
  (bind ((output (intern (string-upcase (name-of input)) (package-of input))))
    (make-iomap projection recursion input input-reference output)))

(def printer lisp-form/quote->list (projection recursion input input-reference)
  (bind ((output (list 'quote (output-of (recurse-printer recursion (value-of input) `((value-of (the sequence document))
                                                                                       ,@(typed-reference (document-type input) input-reference)))))))
    (make-iomap projection recursion input input-reference output)))

(def printer lisp-form/list->list (projection recursion input input-reference)
  (bind ((output (iter (for index :from 0)
                       (for element :in-sequence (elements-of input))
                       (collect (output-of (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                                ,@(typed-reference (document-type input) input-reference))))))))
    (make-iomap projection recursion input input-reference output)))

(def printer lisp-form/toplevel->list (projection recursion input input-reference)
  (bind ((output (list* 'progn
                        (iter (for index :from 0)
                              (for element :in-sequence (elements-of input))
                              (collect (output-of (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                                       ,@(typed-reference (document-type input) input-reference)))))))))
    (make-iomap projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader lisp-form/number->number (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/string->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/symbol->symbol (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/quote->list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/list->list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/toplevel->list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
