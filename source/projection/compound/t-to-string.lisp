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
           (document/base (document->syntax 'default-factory 'default-searcher))
           (collection/sequence (copying))
           (book/base (book->syntax))
           (json/base (json->syntax))
           (xml/base (xml->syntax))
           (common-lisp/base (sequential
                               (common-lisp->s-expression)
                               (s-expression->syntax)))
           (s-expression/base (s-expression->syntax))
           (syntax/base (preserving))
           (syntax/base (preserving))))
       (recursive
         (type-dispatching
           (syntax/base (syntax->text))
           (syntax/base (syntax->text))
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
