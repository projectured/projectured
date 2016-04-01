;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection s-expression/number->number ()
  ())

(def projection s-expression/string->string ()
  ())

(def projection s-expression/symbol->symbol ()
  ())

(def projection s-expression/quote->list ()
  ())

(def projection s-expression/list->list ()
  ())

(def projection s-expression/toplevel->list ()
  ())

;;;;;;
;;; Construction

(def function make-projection/s-expression/number->number ()
  (make-projection 's-expression/number->number))

(def function make-projection/s-expression/string->string ()
  (make-projection 's-expression/string->string))

(def function make-projection/s-expression/symbol->symbol ()
  (make-projection 's-expression/symbol->symbol))

(def function make-projection/s-expression/quote->list ()
  (make-projection 's-expression/quote->list))

(def function make-projection/s-expression/list->list ()
  (make-projection 's-expression/list->list))

(def function make-projection/s-expression/toplevel->list ()
  (make-projection 's-expression/toplevel->list))

;;;;;;
;;; Printer

(def printer s-expression/number->number ()
  (bind ((output (output-of (recurse-printer -recursion- (value-of -input-) `((value-of (the sequence document))
                                                                              ,@(typed-reference (document-type -input-) -input-reference-))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/string->string ()
  (bind ((output (output-of (recurse-printer -recursion- (value-of -input-) `((value-of (the sequence document))
                                                                              ,@(typed-reference (document-type -input-) -input-reference-))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/symbol->symbol ()
  (bind ((output (intern (string-upcase (name-of -input-)) (package-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/quote->list ()
  (bind ((output (list 'quote (output-of (recurse-printer -recursion- (value-of -input-) `((value-of (the sequence document))
                                                                                           ,@(typed-reference (document-type -input-) -input-reference-)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/list->list ()
  (bind ((output (iter (for index :from 0)
                       (for element :in-sequence (elements-of -input-))
                       (collect (output-of (recurse-printer -recursion- element `((elt (the sequence document) ,index)
                                                                                  ,@(typed-reference (document-type -input-) -input-reference-))))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/toplevel->list ()
  (bind ((output (list* 'progn
                        (iter (for index :from 0)
                              (for element :in-sequence (elements-of -input-))
                              (collect (output-of (recurse-printer -recursion- element `((elt (the sequence document) ,index)
                                                                                         ,@(typed-reference (document-type -input-) -input-reference-)))))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader s-expression/number->number ()
  -input-)

(def reader s-expression/string->string ()
  -input-)

(def reader s-expression/symbol->symbol ()
  -input-)

(def reader s-expression/quote->list ()
  -input-)

(def reader s-expression/list->list ()
  -input-)

(def reader s-expression/toplevel->list ()
  -input-)
