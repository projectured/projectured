;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def (function e) make-projection/t->text ()
  (sequential
    (recursive
      (type-dispatching
        (string (string->text))
        (text/text (text/text->tree/leaf))
        (book/base (book->tree))
        (xml/base (xml->tree))
        (json/base (json->tree))
        (java/base (java->tree))
        (javascript/base (javascript->tree))
        (common-lisp/base (sequential
                            (common-lisp->lisp-form)
                            (lisp-form->tree)))
        (lisp-form/base (lisp-form->tree))
        (tree/base (preserving))))
    (recursive
      (type-dispatching
        (tree/base (tree->text))
        (text/text (preserving))))))

(def (function e) make-projection/t->string ()
  (sequential
    (make-projection/t->text)
    (text->string)))
