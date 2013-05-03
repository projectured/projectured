;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def (function e) make-projection/t->string ()
  (sequential
    (recursive
      (type-dispatching
        (string (preserving))
        (text/base (text->tree))
        (text/base (styled-string->tree))
        (book/base (book->tree))
        (xml/base (xml->tree))
        (json/base (json->tree))
        (java/base (java->tree))
        (javascript/base (javascript->tree))
        (lisp-form/base (lisp-form->tree))))
    (recursive
      (tree->styled-string))
    (styled-string->string)))
