;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; TODO: rename and use in t-to-string and product projection?
(def function make-projection/t->tree!!!rename ()
  (recursive
    (type-dispatching
      (string (string->text))
      (text/base (text/text->tree/leaf))
      (book/base (book->tree))
      (xml/base (xml->tree))
      (json/base (json->tree))
      (java/base (java->tree))
      (css/base (css->tree))
      (javascript/base (javascript->tree))
      (common-lisp/base (sequential
                          (common-lisp->lisp-form)
                          (lisp-form->tree)))
      (lisp-form/base (lisp-form->tree))
      (tree/base (preserving)))))
