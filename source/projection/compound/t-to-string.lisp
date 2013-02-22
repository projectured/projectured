;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def method make-projection/t->string ((instance string))
  (preserving))

(def method make-projection/t->string ((instance text/base))
  (recursive (text->string)))

(def method make-projection/t->string ((instance list/base))
  (recursive (list->string)))

(def method make-projection/t->string ((instance table/table))
  (recursive (table->string)))

(def method make-projection/t->string ((instance tree/node))
  (tree->string))

(def method make-projection/t->string ((instance book/base))
  (sequential
    (recursive (book->tree))
    (tree->string :delimiter-provider 'book-delimiter-provider :separator-provider 'book-separator-provider :indentation-provider 'book-indentation-provider)))

(def method make-projection/t->string ((instance xml/base))
  (sequential
    (recursive (xml->tree))
    (tree->string :delimiter-provider 'xml-delimiter-provider :separator-provider 'xml-separator-provider :indentation-provider 'xml-indentation-provider)))

(def method make-projection/t->string ((instance java/base))
  (sequential
    (recursive (java->tree))
    (tree->string :delimiter-provider 'java-delimiter-provider :separator-provider 'java-separator-provider :indentation-provider 'java-indentation-provider)))

(def method make-projection/t->string ((instance json/base))
  (sequential
    (recursive (json->tree))
    (tree->string :delimiter-provider 'json-delimiter-provider :separator-provider 'json-separator-provider :indentation-provider 'json-indentation-provider)))

(def method make-projection/t->string ((instance sequence))
  (sequential
    (recursive (lisp-form->tree))
    (tree->string :delimiter-provider 'lisp-form-delimiter-provider :separator-provider 'lisp-form-separator-provider :indentation-provider 'lisp-form-indentation-provider)))

(def method make-projection/t->string ((instance hu.dwim.walker::walked-form))
  (sequential
    (recursive (walked-lisp-form->lisp-form))
    (recursive (lisp-form->tree))
    (tree->string :indentation-provider 'walked-lisp-form-indentation-provider :separator-provider 'walked-lisp-form-separator-provider :indentation-provider 'walked-lisp-form-indentation-provider)))
