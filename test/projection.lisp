;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Projection

(def suite* (test/projection :in test))

(def function make-test-projection (projection)
  (nesting
    (widget->graphics)
    (reference-dispatching ()
      ((elt (the list (elements-of (the projectured::widget/composite document))) 0)
       projection)
      ((elt (the list (elements-of (the projectured::widget/composite document))) 1)
       (nesting
         (widget->graphics)
         (sequential
           (reference->string)
           (make-test-projection/string->output)))))))

(def macro test-projection (&body projection)
  `(make-test-projection ,(first projection)))

(def function make-test-projection/string->output (&key color-provider font-provider)
  ;; KLUDGE:
  (if (search "SLIME" (symbol-name (class-name (class-of (make-editor)))))
      (sequential
        (string->styled-string :color-provider color-provider :font-provider font-provider)
        ;; TODO:
        #+nil(word-wrapping :wrap-width 800))
      (sequential
        (string->styled-string :color-provider color-provider :font-provider font-provider)
        ;; TODO:
        #+nil (word-wrapping :wrap-width 800)
        (styled-string->graphics))))

;;;;;;
;;; Graphics

(def function make-test-projection/graphics->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (document->graphics)
      (preserving))))

;;;;;;
;;; String

(def function make-test-projection/string->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (document->graphics)
      (make-test-projection/string->output))))

(def function make-test-projection/string->graphics/delimited ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (string->delimited-string "(" ")"))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

(def function make-test-projection/string->graphics/removing ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (removing #\s 'char= 'identity))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

(def function make-test-projection/string->graphics/sorting ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (sorting 'identity 'char<))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

;;;;;;
;;; Text

(def function make-test-projection/text->string ()
  (text->string))

(def function make-test-projection/text->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (text->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

;;;;;;
;;; List

(def function make-test-projection/list->string ()
  (list->string))

(def function make-test-projection/list->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/list->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider 'list-color-provider))))))

;;;;;;
;;; Table

(def function make-test-projection/table->string ()
  (recursive
    (type-dispatching
      (table/base (table->string))
      (t (preserving)))))

(def function make-test-projection/table->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/table->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider 'table-color-provider))))))

;;;;;;
;;; Tree

(def function make-test-projection/tree->string ()
  (tree->string :delimiter-provider 'tree-delimiter-provider :separator-provider 'tree-separator-provider :indentation-provider 'tree-indentation-provider ))

(def function make-test-projection/tree->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/tree->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'tree-color-provider)))))))

;;;;;;
;;; Book

(def function make-test-projection/book->string ()
  (sequential
    (recursive (book->tree))
    (tree->string :delimiter-provider 'book-delimiter-provider :separator-provider 'book-separator-provider :indentation-provider 'book-indentation-provider)))

(def function make-test-projection/book->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/book->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider 'book-color-provider :font-provider 'book-font-provider))))))

;;;;;;
;;; XML

(def function make-test-projection/xml->string ()
  (sequential
    (recursive (xml->tree))
    (tree->string :delimiter-provider 'xml-delimiter-provider :separator-provider 'xml-separator-provider :indentation-provider 'xml-indentation-provider)))

(def function make-test-projection/xml->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/xml->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'xml-color-provider)))))))

;;;;;;
;;; JSON

(def function make-test-projection/json->string ()
  (sequential
    (recursive (json->tree))
    (tree->string :delimiter-provider 'json-delimiter-provider :separator-provider 'json-separator-provider :indentation-provider 'json-indentation-provider)))

(def function make-test-projection/json->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/json->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'json-color-provider)))))))

(def function make-test-projection/json->graphics/focusing ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (focusing '(elt (elements-of (the json/array document)) 5)))
        (nesting
          (document->document)
          (make-test-projection/json->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'json-color-provider)))))))

(def function make-test-projection/json->graphics/removing ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (copying)
          (removing (find-class 'json/boolean) 'eq 'class-of)
          (preserving))
        (nesting
          (document->document)
          (make-test-projection/json->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'json-color-provider)))))))

(def function make-test-projection/json->graphics/sorting ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (copying)
          (sorting (compose (lambda (e) (symbol-name (class-name (class-of e))))) 'string>)
          (preserving))
        (nesting
          (document->document)
          (make-test-projection/json->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'json-color-provider)))))))

;;;;;;
;;; Java

(def function make-test-projection/java->tree ()
  (java->tree))

(def function make-test-projection/java->string ()
  (sequential
    (recursive (make-test-projection/java->tree))
    (tree->string :delimiter-provider 'java-delimiter-provider :separator-provider 'java-separator-provider :indentation-provider 'java-indentation-provider)))

(def function make-test-projection/java->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/java->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'java-color-provider)))))))

;;;;;;
;;; Lisp form

(def function make-test-projection/lisp-form->tree ()
  (recursive (lisp-form->tree)))

(def function make-test-projection/lisp-form->string ()
  (sequential
    (make-test-projection/lisp-form->tree)
    (tree->string :delimiter-provider 'lisp-form-delimiter-provider :separator-provider 'lisp-form-separator-provider :indentation-provider 'lisp-form-indentation-provider)))

(def function make-test-projection/lisp-form->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/lisp-form->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'lisp-form-color-provider)))))))

;;;;;;
;;; Walked lisp form

(def function make-test-projection/walked-lisp-form->lisp-form ()
  (recursive (walked-lisp-form->lisp-form)))

(def function make-test-projection/walked-lisp-form->string ()
  (sequential
    (make-test-projection/walked-lisp-form->lisp-form)
    (make-test-projection/lisp-form->tree)
    (tree->string :delimiter-provider 'walked-lisp-form-delimiter-provider :separator-provider 'walked-lisp-form-separator-provider :indentation-provider 'walked-lisp-form-indentation-provider)))

(def function make-test-projection/walked-lisp-form->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/walked-lisp-form->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'walked-lisp-form-color-provider)))))))

;;;;;;
;;; Evaluator

(def function make-test-projection/evaluator ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (evaluator))
        (nesting
          (document->document)
          (sequence->list)
          (make-test-projection/walked-lisp-form->string))
        (nesting
          (document->document)
          (list->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider 'walked-lisp-form-color-provider))))))

;;;;;;
;;; Test

(def function make-test-projection/test->string ()
  (sequential
    (nesting
      (document->document)
      (test->test-result))
    (nesting
      (document->document)
      (test-result->table))
    (nesting
      (document->document)
      (recursive
        (table->string)))))

(def function make-test-projection/test->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (make-test-projection/test->string)
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider 'walked-lisp-form-color-provider))))))

;;;;;;
;;; T

(def function make-test-projection/t->string ()
  (sequential
    (recursive (t->table))
    (recursive
      (type-dispatching
        (table/base (table->string))
        (t (preserving))))))

(def function make-test-projection/t->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/t->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'table-color-provider 't-color-provider)))))))

;;;;;;
;;; Nested

(def function make-test-projection/nested->string ()
  (sequential
    (recursive
      (type-dispatching
        (hu.dwim.walker:walked-form (walked-lisp-form->lisp-form))
        (t (preserving))))
    (recursive
      (type-dispatching
        (lisp-form/base (lisp-form->tree))
        (xml/base (xml->tree))
        (json/base (json->tree))))
    (tree->string :delimiter-provider (provider-combinator 'walked-lisp-form-delimiter-provider 'json-delimiter-provider 'xml-delimiter-provider)
                  :separator-provider (provider-combinator 'walked-lisp-form-separator-provider 'json-separator-provider 'xml-separator-provider)
                  :indentation-provider (provider-combinator 'walked-lisp-form-indentation-provider 'json-indentation-provider 'xml-indentation-provider))))

(def function make-test-projection/nested->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/nested->string))
        (nesting
          (document->document)
          (string->line-numbered-string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'line-number-color-provider 'walked-lisp-form-color-provider 'json-color-provider 'xml-color-provider)))))))

;;;;;;
;;; Complex

(def function make-test-projection/complex->string ()
  (nesting
    (table->string)
    (type-dispatching
      (xml/base (sequential
                  (recursive (xml->tree))
                  (tree->string :delimiter-provider 'xml-delimiter-provider :separator-provider 'xml-separator-provider :indentation-provider 'xml-indentation-provider)))
      (json/base (sequential
                   (recursive (json->tree))
                   (tree->string :delimiter-provider 'json-delimiter-provider :separator-provider 'json-separator-provider :indentation-provider 'json-indentation-provider)))
      (java/base (sequential
                   (recursive (java->tree))
                   (tree->string :delimiter-provider 'java-delimiter-provider :separator-provider 'java-separator-provider :indentation-provider 'java-indentation-provider)))
      (hu.dwim.walker:walked-form (sequential
                                    (recursive (walked-lisp-form->lisp-form))
                                    (recursive (lisp-form->tree))
                                    (tree->string :delimiter-provider 'walked-lisp-form-delimiter-provider :separator-provider 'walked-lisp-form-separator-provider :indentation-provider 'walked-lisp-form-indentation-provider))))))

(def function make-test-projection/complex->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/complex->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'table-color-provider 'xml-color-provider 'json-color-provider 'java-color-provider)))))))

;;;;;;
;;; Wow

(def function make-test-projection/wow->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (recursive
            (type-dispatching
              (json/base (json->tree))
              (xml/base (xml->tree))
              (book/base (book->tree))
              (java/base (java->tree))
              (lisp-form/base (lisp-form->tree))
              (hu.dwim.walker::walked-form
               (sequential
                 (recursive (walked-lisp-form->lisp-form))
                 (recursive (lisp-form->tree))))
              (table/base (table->string))
              (text/base (text->string))
              (list/base (list->string))
              (t (preserving)))))
        (nesting
          (document->document)
          (tree->string :delimiter-provider (provider-combinator 'book-delimiter-provider 'json-delimiter-provider 'xml-delimiter-provider 'java-delimiter-provider 'walked-lisp-form-delimiter-provider 'lisp-form-delimiter-provider) ;  'tree-delimiter-provider)
                        :separator-provider (provider-combinator 'book-separator-provider 'json-separator-provider 'xml-separator-provider 'java-separator-provider 'walked-lisp-form-separator-provider 'lisp-form-separator-provider) ;  'tree-separator-provider)
                        :indentation-provider (provider-combinator 'book-indentation-provider 'json-indentation-provider 'xml-indentation-provider 'java-indentation-provider 'walked-lisp-form-indentation-provider 'lisp-form-indentation-provider))) ; 'tree-indentation-provider)))
        (nesting
          (document->graphics)
          (make-test-projection/string->output :color-provider (provider-combinator 'book-color-provider 'json-color-provider 'xml-color-provider 'java-color-provider 'walked-lisp-form-color-provider 'lisp-form-color-provider 'tree-color-provider)
                                               :font-provider 'book-font-provider))))))

;;;;;;
;;; Test

(def test test/projection/string->graphics ()
  (finishes (apply-printer (make-test-document/string) (make-test-projection/string->graphics))))

(def test test/projection/text->graphics ()
  (finishes (apply-printer (make-test-document/text) (make-test-projection/text->graphics))))

(def test test/projection/list->string ()
  (finishes (apply-printer (make-test-content/list) (make-test-projection/list->string))))

(def test test/projection/list->graphics ()
  (finishes (apply-printer (make-test-document/list) (make-test-projection/list->graphics))))

(def test test/projection/table->string ()
  (finishes (apply-printer (make-test-content/table) (make-test-projection/table->string))))

(def test test/projection/table->graphics ()
  (finishes (apply-printer (make-test-document/table) (make-test-projection/table->graphics))))

(def test test/projection/tree->string ()
  (finishes (apply-printer (make-test-content/tree) (make-test-projection/tree->string))))

(def test test/projection/tree->graphics ()
  (finishes (apply-printer (make-test-document/tree) (make-test-projection/tree->graphics))))

(def test test/projection/book->string ()
  (finishes (apply-printer (make-test-content/book) (make-test-projection/book->string))))

(def test test/projection/book->graphics ()
  (finishes (apply-printer (make-test-document/book) (make-test-projection/book->graphics))))

(def test test/projection/xml->string ()
  (finishes (apply-printer (make-test-content/xml) (make-test-projection/xml->string))))

(def test test/projection/xml->graphics ()
  (finishes (apply-printer (make-test-document/xml) (make-test-projection/xml->graphics))))

(def test test/projection/json->string ()
  (finishes (apply-printer (make-test-content/json) (make-test-projection/json->string))))

(def test test/projection/json->graphics ()
  (finishes (apply-printer (make-test-document/json) (make-test-projection/json->graphics))))

(def test test/projection/lisp-form->tree ()
  (finishes (apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->tree))))

(def test test/projection/lisp-form->string ()
  (finishes (apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->string))))

(def test test/projection/lisp-form->graphics ()
  (finishes (apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics))))

(def test test/projection/walked-lisp-form->lisp-form ()
  (finishes (apply-printer (make-test-content/walked-lisp-form) (make-test-projection/walked-lisp-form->lisp-form))))

(def test test/projection/walked-lisp-form->string ()
  (finishes (apply-printer (make-test-content/walked-lisp-form) (make-test-projection/walked-lisp-form->string))))

(def test test/projection/walked-lisp-form->graphics ()
  (finishes (apply-printer (make-test-document/walked-lisp-form) (make-test-projection/walked-lisp-form->graphics))))

(def test test/projection/evaluator ()
  (finishes (apply-printer (make-test-document/evaluator) (make-test-projection/evaluator))))

(def test test/projection/test->graphics ()
  (finishes (apply-printer (make-test-document/test) (make-test-projection/test->graphics))))

(def test test/projection/t->string ()
  (finishes (apply-printer (make-test-document/t) (make-test-projection/t->string))))

(def test test/projection/t->graphics ()
  (finishes (apply-printer (make-test-document/t) (make-test-projection/t->graphics))))

(def test test/projection/nested->string ()
  (finishes (apply-printer (make-test-content/nested) (make-test-projection/nested->string))))

(def test test/projection/nested->graphics ()
  (finishes (apply-printer (make-test-document/nested) (make-test-projection/nested->graphics))))

(def test test/projection/complex->string ()
  (finishes (apply-printer (make-test-content/complex) (make-test-projection/complex->string))))

(def test test/projection/complex->graphics ()
  (finishes (apply-printer (make-test-document/complex) (make-test-projection/complex->graphics))))
