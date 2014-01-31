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

(def function make-test-projection/shell (projection)
  (nesting
    (widget->graphics)
    (type-dispatching
      (widget/tooltip (nesting
                        (widget->graphics)
                        (sequential
                          (type-dispatching
                            (text/base (preserving))
                            (t (reference->text)))
                          (make-test-projection/text->output))))
      (t projection))))

(def function make-test-projection/plain (projection)
  (nesting
    (widget->graphics)
    (type-dispatching
      (widget/tooltip (nesting
                        (widget->graphics)
                        (sequential
                          (type-dispatching
                            (text/base (preserving))
                            (t (reference->text)))
                          (make-test-projection/text->output))))
      (widget/scroll-pane (nesting
                            (widget->graphics)
                            projection)))))

(def function make-test-projection/selection (projection)
  (nesting
    (widget->graphics)
    (reference-dispatching ()
      (((the widget/scroll-pane (elt (the sequence document) 0)))
       (nesting
         (widget->graphics)
         projection))
      (((the widget/scroll-pane (elt (the sequence document) 1)))
       (nesting
         (widget->graphics)
         (sequential
           (make-instance 'projection
                          :reader (constantly nil)
                          :printer (lambda (projection recursion input input-reference)
                                     (make-iomap/object projection recursion input input-reference (selection-of input))))
           (reference->text)
           (text->graphics)))))))

(def function make-test-projection/generic (projection)
  (nesting
    (widget->graphics)
    (type-dispatching
      (widget/tooltip (nesting
                        (widget->graphics)
                        (sequential
                          (type-dispatching
                            (text/base (preserving))
                            (t (reference->text)))
                          (make-test-projection/text->output))))
      (widget/split-pane (nesting
                           (widget->graphics)
                           (reference-dispatching ()
                             (((the widget/scroll-pane (elt (the sequence document) 0)))
                              (nesting
                                (widget->graphics)
                                projection))
                             (((the widget/scroll-pane (elt (the sequence document) 1)))
                              (nesting
                                (widget->graphics)
                                (sequential
                                  (recursive (t->table))
                                  (recursive
                                    (type-dispatching
                                      (table/base (table->text))
                                      (t (preserving))))
                                  (make-test-projection/text->output))))))))))

(def function make-test-projection/reflection (projection)
  (nesting
    (widget->graphics)
    (type-dispatching
      (widget/tooltip (nesting
                        (widget->graphics)
                        (sequential
                          (type-dispatching
                            (text/base (preserving))
                            (t (reference->text)))
                          (make-test-projection/text->output))))
      (widget/tabbed-pane (nesting
                            (widget->graphics)
                            (reference-dispatching ()
                              ;; TODO: this is very fragile, create a class for tabbed-pane selectors would probably help
                              (((the widget/label (elt (the sequence document) 0))
                                (the sequence (elt (the sequence document) 0)))
                               (recursive
                                 (type-dispatching
                                   (widget/base (widget/label->graphics/canvas))
                                   (text/base (text->graphics)))))
                              (((the widget/label (elt (the sequence document) 0))
                                (the sequence (elt (the sequence document) 1)))
                               (recursive
                                 (type-dispatching
                                   (widget/base (widget/label->graphics/canvas))
                                   (text/base (text->graphics)))))
                              (((the widget/label (elt (the sequence document) 0))
                                (the sequence (elt (the sequence document) 2 )))
                               (recursive
                                 (type-dispatching
                                   (widget/base (widget/label->graphics/canvas))
                                   (text/base (text->graphics)))))
                              (((the widget/label (elt (the sequence document) 0))
                                (the sequence (elt (the sequence document) 3)))
                               (recursive
                                 (type-dispatching
                                   (widget/base (widget/label->graphics/canvas))
                                   (text/base (text->graphics)))))
                              (((the widget/scroll-pane (elt (the sequence document) 1))
                                (the sequence (elt (the sequence document) 0)))
                               (nesting
                                 (widget->graphics)
                                 projection))
                              (((the widget/scroll-pane (elt (the sequence document) 1))
                                (the sequence (elt (the sequence document) 1)))
                               (nesting
                                 (widget->graphics)
                                 (sequential
                                   (recursive (t->table))
                                   (recursive
                                     (type-dispatching
                                       (table/base (table->text))
                                       (t (preserving))))
                                   (make-test-projection/text->output))))
                              (((the widget/scroll-pane (elt (the sequence document) 1))
                                (the sequence (elt (the sequence document) 2)))
                               (nesting
                                 (widget->graphics)
                                 (sequential
                                   (recursive (t->table))
                                   (recursive
                                     (type-dispatching
                                       (table/base (table->text))
                                       (t (preserving))))
                                   (make-test-projection/text->output))))
                              (((the widget/tabbed-pane (elt (the sequence document) 1))
                                (the sequence (elt (the sequence document) 3)))
                               (nesting
                                 (widget->graphics)
                                 ;; TODO: this is very fragile, create a class for tabbed-pane selectors would probably help
                                 ;; TODO: add more stages?
                                 (reference-dispatching ()
                                   (((the widget/label (elt (the sequence document) 0))
                                     (the sequence (elt (the sequence document) 0)))
                                    (recursive
                                      (type-dispatching
                                        (widget/base (widget/label->graphics/canvas))
                                        (text/base (text->graphics)))))
                                   (((the widget/scroll-pane (elt (the sequence document) 1))
                                     (the sequence (elt (the sequence document) 0)))
                                    (nesting
                                      (widget->graphics)
                                      (sequential
                                        #+nil (elt (elements-of projection) 0)
                                        (recursive (t->table))
                                        (recursive
                                          (type-dispatching
                                            (table/base (table->text))
                                            (t (preserving))))
                                        (make-test-projection/text->output)))))))))))))

(def function make-test-projection/ide (projection)
  (nesting
    (widget->graphics)
    (type-dispatching
      (widget/tooltip (nesting
                        (widget->graphics)
                        (sequential
                          (type-dispatching
                            (text/base (preserving))
                            (t (reference->text)))
                          (make-test-projection/text->output))))
      (widget/split-pane (nesting
                           (widget->graphics)
                           (type-dispatching
                             (widget/base
                              (nesting
                                (widget->graphics)
                                (reference-dispatching ()
                                  ;; TODO: this is very fragile, create a class for tabbed-pane selectors would probably help
                                  (((the widget/label (elt (the sequence document) 0))
                                    (the sequence (elt (the sequence document) 0)))
                                   (recursive
                                     (type-dispatching
                                       (widget/base (widget/label->graphics/canvas))
                                       (text/base (text->graphics)))))
                                  (((the widget/scroll-pane (elt (the sequence document) 1))
                                    (the sequence (elt (the sequence document) 0)))
                                   (nesting
                                     (widget->graphics)
                                     projection)))))
                             (t
                              (sequential
                                (recursive (file-system->tree))
                                (recursive (type-dispatching
                                             (tree/base (tree->text))
                                             (t (preserving))))
                                (text->graphics)))))))))

(def macro test-projection (&body projection)
  `(make-test-projection/reflection ,(first projection)))

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
;;; Debug

(def projection test/debug->graphics/canvas ()
  ())

(def macro test/debug->graphics/canvas ()
  '(make-projection 'test/debug->graphics/canvas))

(def printer test/debug->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the test/debug document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (last-command-iomap (awhen (last-command-of input)
                               (recurse-printer recursion (last-command-of input) `((last-command-of (the test/debug document))
                                                                                    ,@(typed-reference (form-type input) input-reference)))))
         (output (make-graphics/canvas (list* (output-of content-iomap)
                                              (when last-command-iomap
                                                (list (make-graphics/rectangle (make-2d 0 700) (make-2d 1024 30)
                                                                               :stroke-color *color/black*
                                                                               :fill-color *color/pastel-yellow*)
                                                      (make-graphics/canvas (list (output-of last-command-iomap)) (make-2d 5 705)))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

(def reader test/debug->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((last-command (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0))))
    ;; TODO: this breaks context-sensitive-help
    (if (typep (operation-of last-command) 'operation/show-context-sensitive-help)
        last-command
        (make-command (gesture-of input)
                      (make-operation/compound (optional-list (operation-of last-command)
                                                              (make-operation/replace-target (input-of printer-iomap) '((the command (last-command-of (the test/debug document)))) last-command)))))))

(def function make-test-projection/debug (projection)
  (nesting
    (test/debug->graphics/canvas)
    (type-dispatching
      (command (sequential
                 (command->text)
                 (text->graphics)))
      (t projection))))

;;;;;;
;;; Graphics

(def function make-test-projection/graphics->graphics ()
  (preserving))

;;;;;;
;;; String

(def function make-test-projection/string->graphics ()
  (make-test-projection/string->output))

(def function make-test-projection/string->graphics/delimited ()
  (sequential
    (string->delimited-string "(" ")")
    (make-test-projection/string->output)))

(def function make-test-projection/string->graphics/removing ()
  (sequential
    (removing #\s 'char= 'identity)
    (make-test-projection/string->output)))

(def function make-test-projection/string->graphics/sorting ()
  (sequential
    (sorting 'identity 'char<)
    (make-test-projection/string->output)))

;;;;;;
;;; Styled string

(def function make-test-projection/text->graphics ()
  (sequential
    (line-numbering)
    (text->graphics)))

;;;;;;
;;; Text

(def function make-test-projection/text->string ()
  (text->string))

#+nil
(def function make-test-projection/text->graphics ()
  (sequential
    (text->string)
    (make-test-projection/string->output)))

;;;;;;
;;; List

(def function make-test-projection/widget->graphics ()
  (recursive
    (type-dispatching
      (widget/base (widget->graphics))
      (text/base (text->graphics)))))

;;;;;;
;;; List

(def function make-test-projection/list->text ()
  (type-dispatching
    (list/list (list->text))
    (text/text (preserving))))

(def function make-test-projection/list->graphics ()
  (sequential
    (make-test-projection/list->text)
    (make-test-projection/text->output)))

;;;;;;
;;; Table

(def function make-test-projection/table->text ()
  (recursive
    (type-dispatching
      (table/base (table->text))
      (t (preserving)))))

(def function make-test-projection/table->graphics ()
  (sequential
    (make-test-projection/table->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Tree

(def function make-test-projection/tree->text ()
  (recursive
    (type-dispatching
      (tree/base (tree->text))
      (t (preserving)))))

(def function make-test-projection/tree->graphics ()
  (sequential
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

;; TODO: factor
(def function make-test-projection/tree->graphics/removing ()
  (sequential
    ;; TODO: make really recursive
    (recursive
      (type-dispatching
        (list (removing #\s 'find (lambda (element) (when (typep element 'tree/leaf) (content-of element)))))
        (t (copying))))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/tree->graphics/sorting ()
  (sequential
    ;; TODO: make really recursive
    (recursive
      (type-dispatching
        (list (sorting (lambda (element) (when (typep element 'tree/leaf) (content-of element))) 'string<))
        (t (copying))))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Graph

(def function make-test-projection/graph->graphics ()
  (graph->graphics))

;;;;;;
;;; State machine

(def function make-test-projection/state-machine->text ()
  (sequential
    (recursive
      (state-machine->tree))
    (recursive (tree->text))))

(def function make-test-projection/state-machine->graphics ()
  (sequential
    (make-test-projection/state-machine->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Book

(def function make-test-projection/book->text ()
  (sequential
    (recursive
      (type-dispatching
        (book/base (book->tree))
        (text/text (text/text->tree/leaf))))
    (make-test-projection/tree->text)))

(def function make-test-projection/book->graphics ()
  (sequential
    (make-test-projection/book->text)
    (word-wrapping 1024)
    (make-test-projection/text->output)))

(def function make-test-projection/book->graphics/sorting ()
  (sequential
    (nesting
      (copying)
      (type-dispatching
        (sequence/sequence (sorting 'title-of 'string>))
        (t (preserving)))
      (preserving))
    (make-test-projection/book->text)
    (word-wrapping 1024)
    (make-test-projection/text->output)))

;;;;;;
;;; XML

(def function make-test-projection/xml->text ()
  (sequential
    (recursive (xml->tree))
    (make-test-projection/tree->text)))

(def function make-test-projection/xml->graphics ()
  (sequential
    (recursive (xml->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; JSON

(def function make-test-projection/json->text ()
  (sequential
    (recursive (json->tree))
    (make-test-projection/tree->text)))

(def function make-test-projection/json->graphics ()
  (sequential
    (recursive (json->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/json->graphics/focusing ()
  (sequential
    (focusing 'json/base '((the json/string (elt (the sequence document) 4))
                           (the sequence (elements-of (the json/array document)))))
    (recursive (json->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/json->graphics/removing ()
  (sequential
    (nesting
      (copying)
      (type-dispatching
        (sequence/sequence (removing 'json/boolean 'eq 'object-class-name))
        (t (preserving)))
      (preserving))
    #+nil
    (recursive
      (type-dispatching
        (list (removing 'json/boolean 'eq 'object-class-name))
        (t (copying))))
    (recursive (json->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/json->graphics/reversing ()
  (sequential
    (nesting
      (copying)
      (type-dispatching
        (sequence/sequence (reversing))
        (t (preserving)))
      (preserving))
    #+nil
    (recursive
      (type-dispatching
        (list (reversing))
        (t (copying))))
    (recursive (json->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/json->graphics/sorting ()
  (sequential
    (nesting
      ;; TODO: make list copying non-recursive
      (copying)
      (copying)
      (copying)
      (copying)
      (copying)
      (copying)
      (copying)
      (copying)
      (copying)
      (type-dispatching
        (sequence/sequence (sorting 'key-of 'string<))
        (t (preserving)))
      (preserving))
    #+nil
    (recursive
      (type-dispatching
        (list (sorting 'object-class-symbol-name 'string>))
        (t (copying))))
    (recursive (json->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; File system

(def function make-test-projection/file-system->graphics ()
  (sequential
    (recursive (file-system->tree))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Java

(def function make-test-projection/java->tree ()
  (java->tree))

(def function make-test-projection/java->text ()
  (sequential
    (recursive (make-test-projection/java->tree))
    (recursive (tree->text))))

(def function make-test-projection/java->graphics ()
  (sequential
    (make-test-projection/java->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Javascript

(def function make-test-projection/javascript->tree ()
  (javascript->tree))

(def function make-test-projection/javascript->text ()
  (sequential
    (recursive (make-test-projection/javascript->tree))
    (recursive (tree->text))))

(def function make-test-projection/javascript->graphics ()
  (sequential
    (make-test-projection/javascript->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Lisp form

(def function make-test-projection/lisp-form->tree ()
  (recursive (lisp-form->tree)))

(def function make-test-projection/lisp-form->text ()
  (sequential
    (make-test-projection/lisp-form->tree)
    (recursive (tree->text))))

(def function make-test-projection/lisp-form->graphics ()
  (sequential
    (make-test-projection/lisp-form->text)
    (line-numbering)
    (make-test-projection/text->output)))

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
    (make-test-projection/common-lisp->text)
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Evaluator

(def function make-test-projection/evaluator ()
  (sequential
    (evaluator)
    (nesting
      (copying)
      (make-test-projection/common-lisp->text))
    (list->text)
    (make-test-projection/text->output)))

;;;;;;
;;; Test

(def function make-test-projection/test->text ()
  (sequential
    (test->test-result)
    (test-result->table)
    (recursive
      (table->text))))

(def function make-test-projection/test->graphics ()
  (sequential
    (make-test-projection/test->text)
    (line-numbering)
    (make-test-projection/text->output)))

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
    (recursive (t->table))
    (recursive
      (type-dispatching
        (table/base (table->text))
        (t (preserving))))
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Inspector

(def function make-test-projection/inspector->text ()
  (sequential
    (recursive (inspector->table))
    (recursive
      (type-dispatching
        (table/base (table->text))
        (t (preserving))))))

(def function make-test-projection/inspector->graphics ()
  (sequential
    (recursive (inspector->table))
    (recursive
      (type-dispatching
        (table/base (table->text))
        (t (preserving))))
    (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; Demo

(def function make-test-projection/demo->graphics ()
  (sequential
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
           (line-numbering)
           (text->tree)))))
    (recursive (tree->text))
    ;; TODO: this is slow due to text/find
    (word-wrapping 1024)
    (make-test-projection/text->output)))

;;;;;;
;;; Documentation

(def function make-test-projection/documentation->graphics ()
  (sequential
    (focusing '(or json/base xml/base book/base text/text) nil)
    (recursive
      (type-dispatching
        (json/base (json->tree))
        (xml/base (xml->tree))
        #+nil
        ((or json/base xml/base) (sequential
                                   (recursive (type-dispatching
                                                (json/base (json->tree))
                                                (xml/base (xml->tree))))
                                   (make-test-projection/tree->text)
                                   #+nil (line-numbering)
                                   (text/text->tree/leaf)))
        (book/base (book->tree))
        (java/base (java->tree))
        (javascript/base (javascript->tree))
        (lisp-form/base (lisp-form->tree))
        (common-lisp/base
         (sequential
           (recursive (common-lisp->lisp-form))
           (recursive (lisp-form->tree))))
        (table/base
         (sequential
           (make-test-projection/table->text)
           (text/text->tree/leaf)))
        (list/base (make-test-projection/list->text))
        (text/text (sequential
                     (word-wrapping 1024)
                     (text/text->tree/leaf)))))
    (make-test-projection/tree->text)
    (make-test-projection/text->output)))
