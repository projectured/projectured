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
      ((elt (the list (elements-of (the widget/composite document))) 0)
       (nesting
         (widget->graphics)
         (reference-dispatching ()
           ;; TODO: this is very fragile, create a class for tabbed-pane selectors would probably help
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 0)) 0)
            (text->graphics))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 1)) 0)
            (text->graphics))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 2)) 0)
            (text->graphics))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 0)
            (text->graphics))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 0)) 1)
            (nesting
              (widget->graphics)
              projection))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 1)) 1)
            (nesting
              (widget->graphics)
              (sequential
                (nesting
                  (document->document)
                  (make-test-projection/t->text))
                (nesting
                  (document->graphics)
                  (make-test-projection/text->output)))))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 2)) 1)
            (nesting
              (widget->graphics)
              (sequential
                (nesting
                  (document->document)
                  (make-test-projection/t->text))
                (nesting
                  (document->graphics)
                  (make-test-projection/text->output)))))
           ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 1)
            (nesting
              (widget->graphics)
              ;; TODO: this is very fragile, create a class for tabbed-pane selectors would probably help
              ;; TODO: add more stages?
              (reference-dispatching ()
                ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 1)))) 0)) 0)
                 (text->graphics))
                ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 1)))) 1)) 0)
                 (text->graphics))
                ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 1)))) 0)) 1)
                 (nesting
                   (widget->graphics)
                   (sequential
                     (elt (elements-of projection) 0)
                     (nesting
                       (document->document)
                       (make-test-projection/t->text))
                     (nesting
                       (document->graphics)
                       (make-test-projection/text->output)))))
                ((elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elt (the list (selector-element-pairs-of (the widget/tabbed-pane (elt (the list (elements-of (the widget/composite document))) 0)))) 3)) 1)))) 1)) 1)
                 (nesting
                   (widget->graphics)
                   (sequential
                     (elt (elements-of projection) 0)
                     (elt (elements-of projection) 1)
                     (nesting
                       (document->document)
                       (make-test-projection/t->text))
                     (nesting
                       (document->graphics)
                       (make-test-projection/text->output)))))))))))
      ((elt (the list (elements-of (the widget/composite document))) 1)
       (nesting
         (widget->graphics)
         (sequential
           (type-dispatching
             (text/base (preserving))
             (t (reference->text)))
           (make-test-projection/text->output)))))))

(def macro test-projection (&body projection)
  `(make-test-projection ,(first projection)))

(def function make-test-projection/string->output (&key font-provider font-color-provider fill-color-provider line-color-provider)
  ;; KLUDGE:
  (bind ((string->text (string->text :font-provider font-provider
                                                       :font-color-provider font-color-provider
                                                       :fill-color-provider fill-color-provider
                                                       :line-color-provider line-color-provider)))
    (if (search "SLIME" (symbol-name (class-name (class-of (make-editor)))))
        string->text
        (sequential
          string->text
          (text->graphics)))))

(def function make-test-projection/text->output ()
  ;; KLUDGE:
  (if (search "SLIME" (symbol-name (class-name (class-of (make-editor)))))
      (preserving)
      (text->graphics)))

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

(def function make-test-projection/text->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        #+nil
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (text->graphics))))))

;;;;;;
;;; Text

(def function make-test-projection/text->string ()
  (text->string))

#+nil
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

(def function make-test-projection/table->text ()
  (recursive
    (type-dispatching
      (table/base (table->text))
      (t (preserving)))))

(def function make-test-projection/table->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/table->text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Tree

(def function make-test-projection/tree->text ()
  (recursive (tree->text)))

(def function make-test-projection/tree->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (make-test-projection/tree->text))
          #+nil
          (nesting
            (document->document)
            (text->line-numbered-text))
          (nesting
            (document->graphics)
            (make-test-projection/text->output)))
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
            (make-test-projection/tree->text))
          (nesting
            (document->document)
            (text->line-numbered-text))
          (nesting
            (document->graphics)
            (make-test-projection/text->output)))
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
            (make-test-projection/tree->text))
          (nesting
            (document->document)
            (text->line-numbered-text))
          (nesting
            (document->graphics)
            (make-test-projection/text->output)))
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

(def function make-test-projection/state-machine->text ()
  (sequential
    (recursive
      (state-machine->tree))
    (recursive (tree->text))))

(def function make-test-projection/state-machine->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/state-machine->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Book

(def function make-test-projection/book->text ()
  (sequential
    (recursive
      (book->tree))
    (recursive (tree->text))))

(def function make-test-projection/book->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/book->text))
        (nesting
          (document->document)
          (word-wrapping :wrap-width 1024))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; XML

(def function make-test-projection/xml->text ()
  (sequential
    (recursive (xml->tree))
    (recursive (tree->text))))

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
            (recursive (tree->text)))
          (nesting
            (document->document)
            (text->line-numbered-text))
          (nesting
            (document->graphics)
            (make-test-projection/text->output)))
        (nesting
          (document->graphics)
          (sequential
            (recursive (xml->tree))
            (tree->graphics)))))))

;;;;;;
;;; JSON

(def function make-test-projection/json->text ()
  (sequential
    (recursive (json->tree))
    (recursive (tree->text))))

(def function make-test-projection/json->graphics ()
  (sequential
    (nesting
      (document->document)
      (recursive (json->tree)))
    (nesting
      (document->document)
      (recursive (tree->text)))
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

(def function make-test-projection/json->graphics/focusing ()
  (sequential
    (nesting
      (document->document)
      (focusing '(the json/string (elt (the list (elements-of (the json/array document))) 4))))
    (nesting
      (document->document)
      (recursive (json->tree)))
    (nesting
      (document->document)
      (recursive (tree->text)))
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

(def function make-test-projection/json->graphics/removing ()
  (sequential
    (nesting
      (document->document)
      (copying)
      (removing 'json/boolean 'eq 'object-class-name)
      (preserving))
    (nesting
      (document->document)
      (recursive (json->tree)))
    (nesting
      (document->document)
      (recursive (tree->text)))
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

(def function make-test-projection/json->graphics/sorting ()
  (sequential
    (nesting
      (document->document)
      (copying)
      (sorting 'object-class-symbol-name 'string>)
      (preserving))
    (nesting
      (document->document)
      (recursive (json->tree)))
    (nesting
      (document->document)
      (recursive (tree->text)))
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

;;;;;;
;;; Java

(def function make-test-projection/java->tree ()
  (java->tree))

(def function make-test-projection/java->text ()
  (sequential
    (recursive (make-test-projection/java->tree))
    (recursive (tree->text))))

(def function make-test-projection/java->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/java->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Javascript

(def function make-test-projection/javascript->tree ()
  (javascript->tree))

(def function make-test-projection/javascript->text ()
  (sequential
    (recursive (make-test-projection/javascript->tree))
    (recursive (tree->text))))

(def function make-test-projection/javascript->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/javascript->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Lisp form

(def function make-test-projection/lisp-form->tree ()
  (recursive (lisp-form->tree)))

(def function make-test-projection/lisp-form->text ()
  (sequential
    (make-test-projection/lisp-form->tree)
    (recursive (tree->text))))

(def function make-test-projection/lisp-form->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/lisp-form->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Common lisp

(def function make-test-projection/common-lisp->lisp-form ()
  (recursive (common-lisp->lisp-form)))

(def function make-test-projection/common-lisp->text ()
  (sequential
    (make-test-projection/common-lisp->lisp-form)
    (make-test-projection/lisp-form->tree)
    (recursive (tree->text))))

(def function make-test-projection/common-lisp->graphics ()
  (sequential
    (nesting
      (document->document)
      (make-test-projection/common-lisp->text))
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

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
          (make-test-projection/common-lisp->text))
        (nesting
          (document->document)
          (list->string))
        (nesting
          (document->graphics)
          (make-test-projection/string->output))))))

;;;;;;
;;; Test

(def function make-test-projection/test->text ()
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
        (table->text)))))

(def function make-test-projection/test->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (make-test-projection/test->text)
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; T

(def function make-test-projection/t->text ()
  (sequential
    (recursive (t->table))
    (recursive
      (type-dispatching
        (table/base (table->text))
        (t (preserving))))))

(def function make-test-projection/t->graphics ()
  (sequential
    (nesting
      (document->document)
      (make-test-projection/t->text))
    #+nil
    (nesting
      (document->document)
      (text->line-numbered-text))
    (nesting
      (document->graphics)
      (make-test-projection/text->output))))

;;;;;;
;;; Nested

(def function make-test-projection/nested->text ()
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
    (recursive (tree->text))))

(def function make-test-projection/nested->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/nested->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

;;;;;;
;;; Complex

(def function make-test-projection/complex->text ()
  (nesting
    (table->text)
    (type-dispatching
      (xml/base (sequential
                  (recursive (xml->tree))
                  (recursive (tree->text))))
      (json/base (sequential
                   (recursive (json->tree))
                   (recursive (tree->text))))
      (java/base (sequential
                   (recursive (java->tree))
                   (recursive (tree->text))))
      (common-lisp/base (sequential
                          (recursive (common-lisp->lisp-form))
                          (recursive (lisp-form->tree))
                          (recursive (tree->text)))))))

(def function make-test-projection/complex->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (sequential
        (nesting
          (document->document)
          (make-test-projection/complex->text))
        (nesting
          (document->document)
          (text->line-numbered-text))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

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
                                   (recursive (table->text))
                                   (text->tree)))
                     (common-lisp/base (sequential
                                         (common-lisp->lisp-form)
                                         (lisp-form->tree)))
                     (lisp-form/base (lisp-form->tree))
                     (image/image (make-projection/image/image->tree/leaf))
                     (t (preserving))))
                 (recursive (tree->text))
                 ;; TODO: slow due to text/split
                 (text->line-numbered-text)
                 (text->tree))))))
        (nesting
          (document->document)
          (recursive (tree->text)))
        ;; TODO: this is slow due to text/find
        (nesting
          (document->document)
          (word-wrapping :wrap-width 1024))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

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
              (table/base (table->text))
              (text/base (text->string))
              (list/base (list->string))
              (string (make-projection/string->tree/leaf))
              (t (preserving)))))
        (nesting
          (document->document)
          (recursive (tree->text)))
        (nesting
          (document->graphics)
          (make-test-projection/text->output))))))

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

(def test test/projection/table->text ()
  (test/projection/apply-printer (make-test-content/table) (make-test-projection/table->text)))

(def test test/projection/table->graphics ()
  (test/projection/apply-printer (make-test-document/table) (make-test-projection/table->graphics)))

(def test test/projection/tree->text ()
  (test/projection/apply-printer (make-test-content/tree) (make-test-projection/tree->text)))

(def test test/projection/tree->graphics ()
  (test/projection/apply-printer (make-test-document/tree) (make-test-projection/tree->graphics)))

(def test test/projection/book->text ()
  (test/projection/apply-printer (make-test-content/book) (make-test-projection/book->text)))

(def test test/projection/book->graphics ()
  (test/projection/apply-printer (make-test-document/book) (make-test-projection/book->graphics)))

(def test test/projection/xml->text ()
  (test/projection/apply-printer (make-test-content/xml) (make-test-projection/xml->text)))

(def test test/projection/xml->graphics ()
  (test/projection/apply-printer (make-test-document/xml) (make-test-projection/xml->graphics)))

(def test test/projection/json->text ()
  (test/projection/apply-printer (make-test-content/json) (make-test-projection/json->text)))

(def test test/projection/json->graphics ()
  (test/projection/apply-printer (make-test-document/json) (make-test-projection/json->graphics)))

(def test test/projection/lisp-form->tree ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->tree)))

(def test test/projection/lisp-form->text ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->text)))

(def test test/projection/lisp-form->graphics ()
  (test/projection/apply-printer (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics)))

(def test test/projection/common-lisp->lisp-form ()
  (test/projection/apply-printer (make-test-content/common-lisp) (make-test-projection/common-lisp->lisp-form)))

(def test test/projection/common-lisp->text ()
  (test/projection/apply-printer (make-test-content/common-lisp) (make-test-projection/common-lisp->text)))

(def test test/projection/common-lisp->graphics ()
  (test/projection/apply-printer (make-test-document/common-lisp) (make-test-projection/common-lisp->graphics)))

(def test test/projection/evaluator ()
  (test/projection/apply-printer (make-test-document/evaluator) (make-test-projection/evaluator)))

(def test test/projection/test->graphics ()
  (test/projection/apply-printer (make-test-document/test) (make-test-projection/test->graphics)))

(def test test/projection/t->string ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->text)))

(def test test/projection/t->graphics ()
  (test/projection/apply-printer (make-test-document/t) (make-test-projection/t->graphics)))

(def test test/projection/nested->text ()
  (test/projection/apply-printer (make-test-content/nested) (make-test-projection/nested->text)))

(def test test/projection/nested->graphics ()
  (test/projection/apply-printer (make-test-document/nested) (make-test-projection/nested->graphics)))

(def test test/projection/complex->text ()
  (test/projection/apply-printer (make-test-content/complex) (make-test-projection/complex->text)))

(def test test/projection/complex->graphics ()
  (test/projection/apply-printer (make-test-document/complex) (make-test-projection/complex->graphics)))
