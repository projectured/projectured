;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Projection

(def suite* (test/projection :in test))

(def test test/projection/apply-printer (document projection)
  (finishes (apply-printer document projection)))

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

(def function make-test-projection/string->output (&key font-provider font-color-provider fill-color-provider line-color-provider)
  ;; KLUDGE:
  (bind ((string->styled-string (string->styled-string :font-provider font-provider
                                                       :font-color-provider font-color-provider
                                                       :fill-color-provider fill-color-provider
                                                       :line-color-provider line-color-provider)))
    (if (search "SLIME" (symbol-name (class-name (class-of (make-editor)))))
        (sequential
          string->styled-string
          ;; TODO:
          #+nil(word-wrapping :wrap-width 800))
        (sequential
          string->styled-string
          ;; TODO:
          #+nil(word-wrapping :wrap-width 100)
          (styled-string->graphics)))))

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
          (make-test-projection/string->output :font-color-provider 'list-font-color-provider))))))

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
          (make-test-projection/string->output :font-color-provider 'table-font-color-provider))))))

;;;;;;
;;; Tree

;; TODO: factor
(def function make-test-projection/tree->string ()
  (tree->string :delimiter-provider (make-alternative-function (list 'tree-delimiter-provider
                                                                     (provider-combinator 'tree-delimiter-provider (make-delimiter-provider "\"" "\""))
                                                                     (make-delimiter-provider "" "")))
                :separator-provider (make-alternative-function (list 'tree-separator-provider (make-separator-provider "")))
                :indentation-provider (make-alternative-function (list 'tree-indentation-provider
                                                                       (make-indentation-provider :indentation-width 1)
                                                                       (make-indentation-provider :indentation-width 0)
                                                                       (constantly nil)))))

(def function make-test-projection/tree->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (make-test-projection/tree->string))
          #+nil
          (nesting
            (document->document)
            (string->line-numbered-string))
          (nesting
            (document->graphics)
            (make-test-projection/string->output :font-provider (make-alternative-function (list 'tree-font-provider
                                                                                                 (make-font-provider *font/default*)
                                                                                                 (make-font-provider *font/ubuntu/regular/18*)
                                                                                                 (make-font-provider *font/ubuntu/bold/24*)))
                                                 :font-color-provider (make-alternative-function (list (provider-combinator 'line-number-font-color-provider 'tree-font-color-provider)
                                                                                                       (make-color-provider *color/black*))))))
        (nesting
          (document->graphics)
          (tree->graphics))))))

;; TODO: factor
(def function make-test-projection/tree->graphics/removing ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            ;; TODO: make really recursive
            (recursive
              (type-dispatching
                (list (removing #\s 'find (lambda (element) (when (typep element 'tree/leaf) (content-of element)))))
                (t (copying)))))
          (nesting
            (document->document)
            (make-test-projection/tree->string))
          #+nil
          (nesting
            (document->document)
            (string->line-numbered-string))
          (nesting
            (document->graphics)
            (make-test-projection/string->output :font-provider (make-alternative-function (list (make-font-provider *font/default*)
                                                                                                 (make-font-provider *font/ubuntu/regular/18*)
                                                                                                 (make-font-provider *font/ubuntu/bold/24*)))
                                                 :font-color-provider (make-alternative-function (list (provider-combinator 'line-number-font-color-provider 'tree-font-color-provider)
                                                                                                       (make-color-provider *color/black*))))))
        (nesting
          (document->graphics)
          (tree->graphics))))))

(def function make-test-projection/tree->graphics/sorting ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            ;; TODO: make really recursive
            (recursive
              (type-dispatching
                (list (sorting (lambda (element) (when (typep element 'tree/leaf) (content-of element))) 'string<))
                (t (copying)))))
          (nesting
            (document->document)
            (make-test-projection/tree->string))
          #+nil
          (nesting
            (document->document)
            (string->line-numbered-string))
          (nesting
            (document->graphics)
            (make-test-projection/string->output :font-provider (make-alternative-function (list (make-font-provider *font/default*)
                                                                                                 (make-font-provider *font/ubuntu/regular/18*)
                                                                                                 (make-font-provider *font/ubuntu/bold/24*)))
                                                 :font-color-provider (make-alternative-function (list (provider-combinator 'line-number-font-color-provider 'tree-font-color-provider)
                                                                                                       (make-color-provider *color/black*))))))
        (nesting
          (document->graphics)
          (tree->graphics))))))

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
          (make-test-projection/string->output :font-color-provider 'book-font-color-provider :font-provider 'book-font-provider))))))

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
      (alternative
        (sequential
          (nesting
            (document->document)
            (make-test-projection/xml->string))
          (nesting
            (document->document)
            (string->line-numbered-string))
          (nesting
            (document->graphics)
            (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'xml-font-color-provider))))
        (nesting
          (document->graphics)
          (sequential
            (recursive (xml->tree))
            (tree->graphics)))))))

;;;;;;
;;; JSON

(def function make-test-projection/json->string ()
  (sequential
    (recursive (json->tree))
    (tree->string :delimiter-provider (make-alternative-function (list 'json-delimiter-provider (make-delimiter-provider "" "")))
                  :separator-provider (make-alternative-function (list 'json-separator-provider (make-separator-provider "")))
                  :indentation-provider (make-alternative-function (list 'json-indentation-provider (make-indentation-provider :indentation-width 0))))))

(def function make-test-projection/json->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (make-test-projection/json->string))
          (nesting
            (document->document)
            (string->line-numbered-string))
          (nesting
            (document->graphics)
            (make-test-projection/string->output :font-color-provider (make-alternative-function
                                                                       (list (provider-combinator 'line-number-font-color-provider 'json-font-color-provider)
                                                                             (make-color-provider *color/black*))))))
        (nesting
          (document->graphics)
          (sequential
            (recursive (json->tree))
            (tree->graphics)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'json-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'json-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'json-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'java-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'lisp-form-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'walked-lisp-form-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider 'walked-lisp-form-font-color-provider))))))

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
          (make-test-projection/string->output :font-color-provider 'walked-lisp-form-font-color-provider))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'table-font-color-provider 't-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'line-number-font-color-provider 'walked-lisp-form-font-color-provider 'json-font-color-provider 'xml-font-color-provider)))))))

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
          (make-test-projection/string->output :font-color-provider (provider-combinator 'table-font-color-provider 'xml-font-color-provider 'json-font-color-provider 'java-font-color-provider)))))))

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
          (make-test-projection/string->output :font-provider 'book-font-provider
                                               :font-color-provider (provider-combinator 'book-font-color-provider 'json-font-color-provider 'xml-font-color-provider 'java-font-color-provider 'walked-lisp-form-font-color-provider 'lisp-form-font-color-provider 'tree-font-color-provider)))))))

;;;;;;
;;; Test

(def test test/projection/string->graphics ()
  (test/projection/apply-printer (make-test-document/string) (make-test-projection/string->graphics)))

(def test test/projection/text->graphics ()
  (test/projection/apply-printer (make-test-document/text) (make-test-projection/text->graphics)))

(def test test/projection/list->string ()
  (test/projection/apply-printer (make-test-content/list) (make-test-projection/list->string)))

(def test test/projection/list->graphics ()
  (test/projection/apply-printer (make-test-document/list) (make-test-projection/list->graphics)))

(def test test/projection/table->string ()
  (test/projection/apply-printer (make-test-content/table) (make-test-projection/table->string)))

(def test test/projection/table->graphics ()
  (test/projection/apply-printer (make-test-document/table) (make-test-projection/table->graphics)))

(def test test/projection/tree->string ()
  (test/projection/apply-printer (make-test-content/tree) (make-test-projection/tree->string)))

(def test test/projection/tree->graphics ()
  (test/projection/apply-printer (make-test-document/tree) (make-test-projection/tree->graphics)))

(def test test/projection/book->string ()
  (test/projection/apply-printer (make-test-content/book) (make-test-projection/book->string)))

(def test test/projection/book->graphics ()
  (test/projection/apply-printer (make-test-document/book) (make-test-projection/book->graphics)))

(def test test/projection/xml->string ()
  (test/projection/apply-printer (make-test-content/xml) (make-test-projection/xml->string)))

(def test test/projection/xml->graphics ()
  (test/projection/apply-printer (make-test-document/xml) (make-test-projection/xml->graphics)))

(def test test/projection/json->string ()
  (test/projection/apply-printer (make-test-content/json) (make-test-projection/json->string)))

(def test test/projection/json->graphics ()
  (test/projection/apply-printer (make-test-document/json) (make-test-projection/json->graphics)))

(def test test/projection/lisp-form->tree ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->tree)))

(def test test/projection/lisp-form->string ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->string)))

(def test test/projection/lisp-form->graphics ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics)))

(def test test/projection/walked-lisp-form->lisp-form ()
  (test/projection/apply-printer (make-test-content/walked-lisp-form) (make-test-projection/walked-lisp-form->lisp-form)))

(def test test/projection/walked-lisp-form->string ()
  (test/projection/apply-printer (make-test-content/walked-lisp-form) (make-test-projection/walked-lisp-form->string)))

(def test test/projection/walked-lisp-form->graphics ()
  (test/projection/apply-printer (make-test-document/walked-lisp-form) (make-test-projection/walked-lisp-form->graphics)))

(def test test/projection/evaluator ()
  (test/projection/apply-printer (make-test-document/evaluator) (make-test-projection/evaluator)))

(def test test/projection/test->graphics ()
  (test/projection/apply-printer (make-test-document/test) (make-test-projection/test->graphics)))

(def test test/projection/t->string ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->string)))

(def test test/projection/t->graphics ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->graphics)))

(def test test/projection/nested->string ()
  (test/projection/apply-printer (make-test-content/nested) (make-test-projection/nested->string)))

(def test test/projection/nested->graphics ()
  (test/projection/apply-printer (make-test-document/nested) (make-test-projection/nested->graphics)))

(def test test/projection/complex->string ()
  (test/projection/apply-printer (make-test-content/complex) (make-test-projection/complex->string)))

(def test test/projection/complex->graphics ()
  (test/projection/apply-printer (make-test-document/complex) (make-test-projection/complex->graphics)))
