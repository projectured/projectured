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
        string->styled-string
        (sequential
          string->styled-string
          (styled-string->graphics)))))

(def function make-test-projection/styled-string->output ()
  ;; KLUDGE:
  (if (search "SLIME" (symbol-name (class-name (class-of (make-editor)))))
      (preserving)
      (styled-string->graphics)))

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
;;; Styled string

(def function make-test-projection/styled-string->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (styled-string->graphics))))))

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
          (make-test-projection/string->output))))))

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
          (make-test-projection/string->output))))))

;;;;;;
;;; Tree

(def function make-test-projection/tree->styled-string ()
  (recursive (tree->styled-string :delimiter-provider (make-alternative-function (list (make-delimiter-provider "(" ")")))
                                  :separator-provider (make-alternative-function (list (make-separator-provider " ")))
                                  :indentation-provider (make-alternative-function (list (make-indentation-provider :indentation-width 1)
                                                                                         (make-indentation-provider :indentation-width 0)
                                                                                         (constantly nil))))))

(def function make-test-projection/tree->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (make-test-projection/tree->styled-string))
          #+nil
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
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
            (make-test-projection/tree->styled-string))
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
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
            (make-test-projection/tree->styled-string))
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
        (nesting
          (document->graphics)
          (tree->graphics))))))

;;;;;;
;;; Graph

(def function make-test-projection/graph->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (graph->graphics))))))

;;;;;;
;;; State machine

(def function make-test-projection/state-machine->styled-string ()
  (sequential
    (recursive
      (state-machine->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/state-machine->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/state-machine->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Book

(def function make-test-projection/book->styled-string ()
  (sequential
    (recursive
      (book->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/book->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/book->styled-string))
        (nesting
          (document->document)
          (word-wrapping :wrap-width 1024))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; XML

(def function make-test-projection/xml->styled-string ()
  (sequential
    (recursive (xml->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/xml->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (recursive (xml->tree)))
          (nesting
            (document->document)
            (recursive (tree->styled-string)))
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
        (nesting
          (document->graphics)
          (sequential
            (recursive (xml->tree))
            (tree->graphics)))))))

;;;;;;
;;; JSON

(def function make-test-projection/json->styled-string ()
  (sequential
    (recursive (json->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/json->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (recursive (json->tree)))
          (nesting
            (document->document)
            (recursive (tree->styled-string)))
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
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
          (make-test-projection/json->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

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
          (make-test-projection/json->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

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
          (make-test-projection/json->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Java

(def function make-test-projection/java->tree ()
  (java->tree))

(def function make-test-projection/java->styled-string ()
  (sequential
    (recursive (make-test-projection/java->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/java->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/java->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Javascript

(def function make-test-projection/javascript->tree ()
  (javascript->tree))

(def function make-test-projection/javascript->styled-string ()
  (sequential
    (recursive (make-test-projection/javascript->tree))
    (recursive (tree->styled-string))))

(def function make-test-projection/javascript->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/javascript->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Lisp form

(def function make-test-projection/lisp-form->tree ()
  (recursive (lisp-form->tree)))

(def function make-test-projection/lisp-form->styled-string ()
  (sequential
    (make-test-projection/lisp-form->tree)
    (recursive (tree->styled-string))))

(def function make-test-projection/lisp-form->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/lisp-form->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Common lisp

(def function make-test-projection/common-lisp->lisp-form ()
  (recursive (common-lisp->lisp-form)))

(def function make-test-projection/common-lisp->styled-string ()
  (sequential
    (make-test-projection/common-lisp->lisp-form)
    (make-test-projection/lisp-form->tree)
    (recursive (tree->styled-string))))

(def function make-test-projection/common-lisp->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/common-lisp->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

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
          (make-test-projection/common-lisp->styled-string))
        (nesting
          (document->document)
          (list->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

;;;;;;
;;; Test

(def function make-test-projection/test->styled-string ()
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
        (make-test-projection/test->styled-string)
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; T

(def function make-test-projection/t->styled-string ()
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
          (make-test-projection/t->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Nested

(def function make-test-projection/nested->styled-string ()
  (sequential
    (recursive
      (type-dispatching
        (common-lisp/base (common-lisp->lisp-form))
        (t (preserving))))
    (recursive
      (type-dispatching
        (lisp-form/base (lisp-form->tree))
        (xml/base (xml->tree))
        (json/base (json->tree))))
    (recursive (tree->styled-string))))

(def function make-test-projection/nested->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/nested->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Complex

(def function make-test-projection/complex->styled-string ()
  (nesting
    (table->string)
    (type-dispatching
      (xml/base (sequential
                  (recursive (xml->tree))
                  (recursive (tree->styled-string))))
      (json/base (sequential
                   (recursive (json->tree))
                   (recursive (tree->styled-string))))
      (java/base (sequential
                   (recursive (java->tree))
                   (recursive (tree->styled-string))))
      (common-lisp/base (sequential
                          (recursive (common-lisp->lisp-form))
                          (recursive (lisp-form->tree))
                          (recursive (tree->styled-string)))))))

(def function make-test-projection/complex->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/complex->styled-string))
        (nesting
          (document->document)
          (styled-string->line-numbered-styled-string))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

;;;;;;
;;; Demo

(def function make-test-projection/demo->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (recursive
            (type-dispatching
              (book/base (book->tree))
              (text/base (text->tree))
              (image/image (make-projection/image/image->tree/leaf))
              (t
               (sequential
                 (recursive
                   (type-dispatching
                     (text/base (text->tree))
                     (book/base (book->tree))
                     (xml/base (xml->tree))
                     (json/base (json->tree))
                     (javascript/base (javascript->tree))
                     (table/base (sequential
                                   (recursive (table->string))
                                   (make-projection 't->tree/leaf)))
                     (common-lisp/base (sequential
                                         (common-lisp->lisp-form)
                                         (lisp-form->tree)))
                     (lisp-form/base (lisp-form->tree))
                     (image/image (make-projection/image/image->tree/leaf))
                     (t (preserving))))
                 (recursive (tree->styled-string))
                 ;; TODO: slow due to text/split
                 (styled-string->line-numbered-styled-string)
                 (make-projection 't->tree/leaf))))))
        (nesting
          (document->document)
          (recursive (tree->styled-string)))
        ;; TODO: this is slow due to text/find
        (nesting
          (document->document)
          (word-wrapping :wrap-width 1024))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

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
              (javascript/base (javascript->tree))
              (lisp-form/base (lisp-form->tree))
              (common-lisp/base
               (sequential
                 (recursive (common-lisp->lisp-form))
                 (recursive (lisp-form->tree))))
              (table/base (table->string))
              (text/base (text->string))
              (list/base (list->string))
              (string (make-projection/string->tree/leaf))
              (t (preserving)))))
        (nesting
          (document->document)
          (recursive (tree->styled-string)))
        (nesting
          (document->graphics)
          (make-test-projection/styled-string->output))))))

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

(def test test/projection/tree->styled-string ()
  (test/projection/apply-printer (make-test-content/tree) (make-test-projection/tree->styled-string)))

(def test test/projection/tree->graphics ()
  (test/projection/apply-printer (make-test-document/tree) (make-test-projection/tree->graphics)))

(def test test/projection/book->styled-string ()
  (test/projection/apply-printer (make-test-content/book) (make-test-projection/book->styled-string)))

(def test test/projection/book->graphics ()
  (test/projection/apply-printer (make-test-document/book) (make-test-projection/book->graphics)))

(def test test/projection/xml->styled-string ()
  (test/projection/apply-printer (make-test-content/xml) (make-test-projection/xml->styled-string)))

(def test test/projection/xml->graphics ()
  (test/projection/apply-printer (make-test-document/xml) (make-test-projection/xml->graphics)))

(def test test/projection/json->styled-string ()
  (test/projection/apply-printer (make-test-content/json) (make-test-projection/json->styled-string)))

(def test test/projection/json->graphics ()
  (test/projection/apply-printer (make-test-document/json) (make-test-projection/json->graphics)))

(def test test/projection/lisp-form->tree ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->tree)))

(def test test/projection/lisp-form->styled-string ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->styled-string)))

(def test test/projection/lisp-form->graphics ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics)))

(def test test/projection/common-lisp->lisp-form ()
  (test/projection/apply-printer (make-test-content/common-lisp) (make-test-projection/common-lisp->lisp-form)))

(def test test/projection/common-lisp->styled-string ()
  (test/projection/apply-printer (make-test-content/common-lisp) (make-test-projection/common-lisp->styled-string)))

(def test test/projection/common-lisp->graphics ()
  (test/projection/apply-printer (make-test-document/common-lisp) (make-test-projection/common-lisp->graphics)))

(def test test/projection/evaluator ()
  (test/projection/apply-printer (make-test-document/evaluator) (make-test-projection/evaluator)))

(def test test/projection/test->graphics ()
  (test/projection/apply-printer (make-test-document/test) (make-test-projection/test->graphics)))

(def test test/projection/t->string ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->styled-string)))

(def test test/projection/t->graphics ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->graphics)))

(def test test/projection/nested->styled-string ()
  (test/projection/apply-printer (make-test-content/nested) (make-test-projection/nested->styled-string)))

(def test test/projection/nested->graphics ()
  (test/projection/apply-printer (make-test-document/nested) (make-test-projection/nested->graphics)))

(def test test/projection/complex->styled-string ()
  (test/projection/apply-printer (make-test-content/complex) (make-test-projection/complex->styled-string)))

(def test test/projection/complex->graphics ()
  (test/projection/apply-printer (make-test-document/complex) (make-test-projection/complex->graphics)))
