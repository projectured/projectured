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
           (document/base (document->tree 'default-factory 'default-searcher))
           (document/sequence (copying))
           (book/base (book->tree))
           (json/base (json->tree))
           (xml/base (xml->tree))
           (common-lisp/base (sequential
                               (common-lisp->lisp-form)
                               (lisp-form->tree)))
           (lisp-form/base (lisp-form->tree))
           (tree/base (preserving))))
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
