;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Construction

(def function make-projection/t->text ()
  (type-dispatching
    (text/text (preserving))
    (document
     (sequential
       (recursive
         (type-dispatching
           (document/base (document->tree 'initial-factory))
           (document/sequence (copying))
           (book/base (book->tree))
           (json/base (json->tree))
           (xml/base (xml->tree))))
       (recursive
         (type-dispatching
           (tree/base (tree->text))
           (text/text (preserving))))))))

(def function make-projection/t->string ()
  (sequential
    (make-projection/t->text)
    (text->string)))

;;;;;;
;;; Construction

(def macro t->text ()
  '(make-projection/t->text))

(def macro t->string ()
  '(make-projection/t->string))
