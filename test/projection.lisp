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

(def function make-test-projection/document (projection)
  (nesting
    (document->t nil)
    (document/clipboard->t)
    projection))

(def function make-test-projection/shell (projection)
  (sequential
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
        (t projection)))
    (recursive
      (graphics->graphics))))

(def function make-test-projection/plain (projection)
  (sequential
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
                              projection))))
    (recursive
      (graphics->graphics))))

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

(def function make-test-projection/split (projection-1 projection-2)
  (nesting
    (widget->graphics)
    (widget->graphics)
    (reference-dispatching ()
      (((the widget/scroll-pane (elt (the sequence document) 0)))
       (nesting
         (widget->graphics)
         projection-1))
      (((the widget/scroll-pane (elt (the sequence document) 1)))
       (nesting
         (widget->graphics)
         projection-2)))))

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
                                (make-test-projection/t->graphics/tree)
                                #+nil(make-test-projection/t->graphics/table)))))))))

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

(def function test-factory (name)
  (completion-prefix-switch name
    ("book" (book/book (:selection '((the string (subseq (the string document) 0 0))
                                     (the string (title-of (the book/book document)))))))
    ("chapter" (book/chapter (:selection '((the string (subseq (the string document) 0 0))
                                           (the string (title-of (the book/chapter document)))))))
    ("paragraph" (book/paragraph (:selection '((the text/text (text/subseq (the text/text document) 0 0))
                                               (the text/text (content-of (the book/paragraph document)))))
                   (text/text () (text/string "" :font *font/liberation/serif/regular/24* :font-color (color/darken *color/solarized/blue* 0.5)))))
    ("picture" (book/picture (:selection '((the string (subseq (the string document) 0 0))
                                           (the string (filename-of (the image/file document)))
                                           (the image/file (content-of (the book/picture document)))))
                 (image/file () "")))
    ("text" (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0)))) (text/string "")))
    ;; TODO:
    ("image" (image/file () (resource-pathname "image/lisp-boxed-alien.jpg")))
    ("table" (table/table ()
               (table/row ()
                 (table/cell ()
                   (text/text () (text/string ""))
                   #+nil
                   (document/nothing)))))
    ("xml element" (xml/element ("" nil :selection '((the string (subseq (the string document) 0 0))
                                                     (the string (xml/start-tag (the xml/element document)))))))
    ("xml attribute" (xml/attribute (:selection '((the string (subseq (the string document) 0 0))
                                                  (the string (name-of (the xml/attribute document))))) "" ""))
    ("xml text" (xml/text (:selection '((the string (subseq (the string document) 0 0))
                                        (the string (value-of (the xml/text document))))) ""))
    ("css rule" (css/rule ("" :selection '((the string (subseq (the string document) 0 0))
                                           (the string (selector-of (the css/rule document)))))))
    ("css attribute" (css/attribute (:selection '((the string (subseq (the string document) 0 0))
                                                  (the string (name-of (the css/attribute document))))) "" ""))
    ("json null" (json/null ()))
    ("json false" (json/boolean () #f))
    ("json true" (json/boolean ()#t))
    ("json number" (json/number () 0))
    ("json string" (json/string () ""))
    ("json array" (json/array (:selection '((the string (subseq (the string document) 0 0))
                                            (the string (value-of (the json/nothing document)))
                                            (the json/nothing (elt (the sequence document) 0))
                                            (the sequence (elements-of (the json/array document)))))
                    (json/nothing (:selection '((the string (subseq (the string document) 0 0))
                                                (the string (value-of (the json/nothing document))))))))
    ("json object entry" (json/object-entry (:selection '((the string (subseq (the string document) 0 0))
                                                          (the string (key-of (the json/object-entry document))))) ""
                                                          (json/nothing ())))
    ("json object" (make-json/object (make-sequence/sequence (list (json/nothing (:selection '((the string (subseq (the string document) 0 0))
                                                                                               (the string (value-of (the json/nothing document)))))))
                                                             :selection '((the string (subseq (the string document) 0 0))
                                                                          (the string (value-of (the json/nothing document)))
                                                                          (the json/nothing (elt (the sequence document) 0))))
                                     :selection '((the string (subseq (the string document) 0 0))
                                                  (the string (value-of (the json/nothing document)))
                                                  (the json/nothing (elt (the sequence document) 0))
                                                  (the sequence (entries-of (the json/object document))))))
    ("common lisp comment" (make-instance 'common-lisp/comment
                                          :content (text/text () (text/string ""))
                                          :selection '((the string (subseq (the string document) 0 0))
                                                       (the string (content-of (the common-lisp/comment document))))))
    ("common lisp string" (make-instance 'common-lisp/constant
                                         :value ""
                                         :selection '((the string (subseq (the string document) 0 0))
                                                      (the string (value-of (the common-lisp/constant document))))))
    ("common lisp number" (make-instance 'common-lisp/constant
                                         :value 0
                                         :selection '((the string (subseq (the string document) 0 0))
                                                      (the string (write-to-string (the number document)))
                                                      (the number (value-of (the common-lisp/constant document))))))
    ("common lisp if" (make-instance 'common-lisp/if
                                     :condition (document/nothing)
                                     :then (document/nothing)
                                     :else (document/nothing)))
    ("common lisp progn" (make-instance 'common-lisp/progn :body nil))
    ("common lisp function application" (make-instance 'common-lisp/application
                                                       :operator (make-lisp-form/symbol "" "PROJECTURED.TEST")
                                                       :selection '((the string (subseq (the string document) 0 0))
                                                                    (the string (name-of (the lisp-form/symbol document)))
                                                                    (the lisp-form/symbol (operator-of (the common-lisp/application document))))
                                                       :arguments nil))
    ("common lisp function definition" (make-instance 'common-lisp/function-definition
                                                      :name (make-lisp-form/symbol "" "PROJECTURED.TEST") :documentation "" :bindings nil :allow-other-keys #f :body nil
                                                      :selection '((the string (subseq (the string document) 0 0))
                                                                   (the string (name-of (the lisp-form/symbol document)))
                                                                   (the lisp-form/symbol (name-of (the common-lisp/function-definition document))))))
    ("javascript block" (make-javascript/statement/block nil))
    ("javascript function definition" (make-javascript/definition/function "" nil nil))
    ("evaluator" (make-evaluator/evaluator (make-instance 'common-lisp/progn :body nil)))))

;;;;;;
;;; Debug

(def projection test/debug->graphics/canvas ()
  ())

(def macro test/debug->graphics/canvas ()
  '(make-projection 'test/debug->graphics/canvas))

(def printer test/debug->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the test/debug document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (last-commands-iomap (awhen (last-commands-of input)
                                (recurse-printer recursion (last-commands-of input) `((last-commands-of (the test/debug document))
                                                                                      ,@(typed-reference (form-type input) input-reference)))))
         (output (make-graphics/canvas (list* (output-of content-iomap)
                                              (when last-commands-iomap
                                                (list (make-graphics/rectangle (make-2d 0 650) (make-2d 1280 68)
                                                                               :stroke-color *color/black*
                                                                               :fill-color *color/pastel-yellow*)
                                                      (make-graphics/canvas (list (output-of last-commands-iomap)) (make-2d 5 655)))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

(def reader test/debug->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (last-command (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))
         (last-commands (last-commands-of printer-input))
         (new-last-commands (subseq (list* last-command last-commands) 0 (min 3 (1+ (length last-commands))))))
    ;; TODO: this breaks context-sensitive-help
    (if (typep (operation-of last-command) 'operation/show-context-sensitive-help)
        last-command
        (make-command (gesture-of input)
                      (make-operation/compound (optional-list (operation-of last-command)
                                                              (make-operation/replace-target (input-of printer-iomap)
                                                                                             '((the sequence (last-commands-of (the test/debug document))))
                                                                                             new-last-commands)))
                      :domain (domain-of last-command)
                      :description (description-of last-command)))))

(def function make-test-projection/debug (projection)
  (nesting
    (test/debug->graphics/canvas)
    (reference-dispatching ()
      (((the sequence (last-commands-of (the test/debug document))))
       (sequential
         (nesting
           (sequence->text :opening-delimiter nil :closing-delimiter nil :indentation nil :separator (text/text () (text/newline)))
           (command->text))
         (text->graphics)))
      (t
       projection))))

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
;;; Text

(def function make-test-projection/text->graphics ()
  (sequential
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/text->graphics/ll ()
  (make-test-projection/text->output))

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
      (text/text (preserving)))))

(def function make-test-projection/tree->graphics ()
  (sequential
    (make-test-projection/tree->text)
;; TODO:    (line-numbering)
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
    (recursive (state-machine->tree))
    (make-test-projection/tree->text)))

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
        (document/base (document->t 'test-factory))
        (text/text (preserving))))
    (make-test-projection/tree->text)))

(def function make-test-projection/book->graphics ()
  (sequential
    (make-test-projection/book->text)
    (word-wrapping 1280)
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
    (word-wrapping 1280)
    (make-test-projection/text->output)))

;;;;;;
;;; XML

(def function make-test-projection/xml->text ()
  (sequential
    (recursive
      (type-dispatching
        (document/base (document->t 'test-factory))
        (xml/base (xml->tree))))
    (make-test-projection/tree->text)))

(def function make-test-projection/xml->graphics ()
  (sequential
    (recursive
      (type-dispatching
        (document/base (document->t 'test-factory))
        (xml/base (xml->tree))))
    (make-test-projection/tree->text)
    ;; (line-numbering)
    (make-test-projection/text->output)))

;;;;;;
;;; JSON

(def function make-test-projection/json->text ()
  (sequential
    (recursive
      (type-dispatching
        (document/base (document->t 'test-factory))
        (json/base (json->tree))))
    (make-test-projection/tree->text)))

(def function make-test-projection/json->graphics ()
  (sequential
    (recursive
      (type-dispatching
        (document/base (document->t 'test-factory))
        (json/base (json->tree))))
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
    #+nil
    (recursive
      (type-dispatching
        (json/object
          (nesting
            (copying)
            (type-dispatching
              (sequence/sequence (sorting 'key-of 'string<))
              (t (preserving)))))
        (t (copying))))
    (nesting
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
    (make-test-projection/tree->text)))

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
    (make-test-projection/tree->text)))

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
    (make-test-projection/tree->text)))

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
    (make-test-projection/tree->text)))

(def function make-test-projection/common-lisp->graphics ()
  (sequential
    (make-test-projection/common-lisp->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/common-lisp/inliner->graphics ()
  (sequential
    (recursive
      (type-dispatching
        (common-lisp/application (inliner))
        (t (copying))))
    (make-test-projection/common-lisp->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/common-lisp->graphics/test ()
  (sequential
    (focusing '(or common-lisp/base book/base) nil)
    (recursive
      (type-dispatching
        (book/base (book->tree))
        (test/result (test/result->tree/leaf))
        (evaluator/evaluator (evaluator/evaluator->tree/node))
        (common-lisp/base (sequential
                            (recursive
                              (type-dispatching
                                (test/check (test/check->tree/node))
                                (common-lisp/variable-reference (preserving))
                                (common-lisp/function-reference (preserving))
                                (t (copying))))
                            (recursive (type-dispatching
                                         (common-lisp/base (common-lisp->lisp-form))
                                         (document/base (document->t 'test-factory))
                                         (t (copying))))
                            (recursive (type-dispatching
                                         (lisp-form/base (lisp-form->tree))
                                         (t (copying))))
                            (make-test-projection/tree->text)
                            (line-numbering)
                            (text/text->tree/leaf)))
        (document/base (document->t 'test-factory))
        (text/text (preserving))
        (t (sequential
             (t->tree)
             (text/text->tree/leaf)))))
    (make-test-projection/tree->text)
    (make-test-projection/text->output)))

(def function make-test-projection/common-lisp->graphics/search ()
  (sequential
    (document/search->tree/node (lambda (search instance)
                                  (search-parts instance (lambda (instance)
                                                           (typecase instance
                                                             (test/check
                                                              (string= search "is"))
                                                             (common-lisp/constant
                                                              (search-ignorecase search (write-to-string (value-of instance))))
                                                             (common-lisp/variable-reference
                                                              (search-ignorecase search (name-of (name-of (variable-of instance)))))
                                                             (common-lisp/application
                                                              (typecase (operator-of instance)
                                                                (lisp-form/symbol
                                                                 (search-ignorecase search (name-of (operator-of instance))))
                                                                (common-lisp/function-reference
                                                                 (search-ignorecase search (name-of (name-of (function-of (operator-of instance))))))))
                                                             (common-lisp/function-definition
                                                              (search-ignorecase search (name-of (name-of instance))))))
                                                :slot-provider 'test-slot-provider)))
    (recursive
      (type-dispatching
        (test/check (test/check->tree/node))
        (common-lisp/variable-reference (preserving))
        (common-lisp/function-reference (preserving))
        (t (copying))))
    (recursive (type-dispatching
                 (common-lisp/base (common-lisp->lisp-form))
                 (document/base (document->t 'test-factory))
                 (t (copying))))
    (recursive (type-dispatching
                 (lisp-form/base (lisp-form->tree))
                 (t (copying))))
    (make-test-projection/tree->text)
    (make-test-projection/text->output)))

(def function make-test-projection/common-lisp->graphics/split ()
  (make-test-projection/split (make-test-projection/common-lisp->graphics/search) (make-test-projection/common-lisp->graphics/test)))

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
;;; T

(def function test-slot-provider (instance)
  (remove-if (lambda (slot) (member (slot-definition-name slot) '(raw projection selection font font-color stroke-color fill-color line-color))) (class-slots (class-of instance))))

(def function make-test-projection/t->text/tree ()
  (sequential
    (recursive (t->tree :slot-provider 'test-slot-provider))
    (make-test-projection/tree->text)))

(def function make-test-projection/t->graphics/tree ()
  (sequential
    (recursive (t->tree :slot-provider 'test-slot-provider ))
    (make-test-projection/tree->text)
    (line-numbering)
    (make-test-projection/text->output)))

(def function make-test-projection/t->text/table ()
  (sequential
    (recursive (t->table :slot-provider 'test-slot-provider))
    (recursive
      (type-dispatching
        (table/base (table->text))
        (t (preserving))))))

(def function make-test-projection/t->graphics/table ()
  (sequential
    (recursive (t->table :slot-provider 'test-slot-provider))
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
        (document/base (document->t 'test-factory))
        (text/text (preserving))
        (xml/base (xml->tree))
        (json/base (json->tree))))
    (make-test-projection/tree->text)
    (word-wrapping 1280)
    (make-test-projection/text->output)))

#+nil
(def function make-test-projection/demo->graphics ()
  (sequential
    (focusing '(or json/base xml/base book/base) nil)
    (recursive
      (type-dispatching
        (book/base (book->tree))
        (document/base (document->t 'test-factory))
        (text/text (preserving))
        (table/base (sequential
                      (recursive
                        (type-dispatching
                          (table/base (table->text))
                          (text/base (preserving))))
                      (text/text->tree/leaf)))
        (t
         (sequential
           (recursive
             (type-dispatching
               (text/base (text/text->tree/leaf))
               (book/base (book->tree))
               (xml/base (xml->tree))
               (json/base (json->tree))
               (javascript/base (javascript->tree))
               (css/base (css->tree))
               (table/base (sequential
                             (recursive
                               (type-dispatching
                                 (table/base (table->text))
                                 (text/base (preserving))))
                             (text/text->tree/leaf)))
               (common-lisp/base (sequential
                                   (common-lisp->lisp-form)
                                   (lisp-form->tree)))
               (lisp-form/base (lisp-form->tree))
               (image/file (make-projection/image/file->tree/leaf))
               (document/base (document->t 'test-factory))
               (evaluator/evaluator (evaluator/evaluator->tree/node))
               (t (preserving))))
           (make-test-projection/tree->text)
           ;; TODO: slow due to text/split
           (line-numbering)
           (text/text->tree/leaf)))))
    (make-test-projection/tree->text)
    ;; TODO: this is slow due to text/find
    (word-wrapping 1280)
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
        (text/text (word-wrapping 1280))
        (document/base (document->t 'test-factory))))
    (make-test-projection/tree->text)
    (make-test-projection/text->output)))
