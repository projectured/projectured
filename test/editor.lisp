;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Test

(def suite* (test/editor :in test))

(def test test/editor/read-eval-print-loop (&key document projection wrap cache-graphics display-gestures display-selection mark-cached-graphics mark-graphics-changes profile (profile-reader profile) (profile-evaluator profile) (profile-printer profile))
  (bind ((editor (make-editor :filename "/tmp/projectured.bmp"))
         (document (ecase wrap
                     ((nil)
                      document)
                     (:plain
                      (make-test-document/plain (make-test-document/document document)))
                     (:search
                      (make-test-document/search document))
                     (:document
                      (make-test-document/document document))
                     (:debug
                      (make-test-document/shell (make-test-document/debug (make-test-document/document document))))
                     (:selection
                      (make-test-document/shell (make-test-document/debug (make-test-document/selection (make-test-document/document document)))))
                     (:generic
                      (make-test-document/shell (make-test-document/debug (make-test-document/generic (make-test-document/document document)))))
                     (:reflection
                      (make-test-document/reflection document projection))
                     (:ide
                      (make-test-document/ide document))))
         (projections (append (list (ecase wrap
                                      ((nil)
                                       projection)
                                      (:plain
                                       (make-test-projection/plain (make-test-projection/document projection)))
                                      (:search
                                       (make-test-projection/search projection))
                                      (:document
                                       (make-test-projection/document projection))
                                      (:debug
                                       (make-test-projection/shell (make-test-projection/debug (make-test-projection/document projection))))
                                      (:selection
                                       (make-test-projection/shell (make-test-projection/debug (make-test-projection/selection (make-test-projection/document projection)))))
                                      (:generic
                                       (make-test-projection/shell (make-test-projection/debug (make-test-projection/generic (make-test-projection/document projection)))))
                                      (:reflection
                                       (make-test-projection/reflection projection))
                                      (:ide
                                       (make-test-projection/ide projection))))
                              (when mark-graphics-changes
                                (list (recursive
                                        (graphics->graphics@mark-changes))))
                              (when cache-graphics
                                (list (recursive
                                        (graphics->graphics@cache-graphics :debug mark-cached-graphics))))))
         (projection (if (= 1 (length projections))
                         (first projections)
                         (make-projection/sequential projections))))
    (run-read-evaluate-print-loop editor document projection :profile-reader profile-reader :profile-evaluator profile-evaluator :profile-printer profile-printer)))

(def definer editor (name document projection)
  `(def test ,name (&rest args &key (document ,document) (projection ,projection) &allow-other-keys)
     (apply #'test/editor/read-eval-print-loop :document document :projection projection args)))

(def editor test/editor/graphics (make-test-document/graphics) (make-test-projection/graphics->graphics))

(def editor test/editor/graphics/ll (make-test-document/graphics/ll 1000 500) (make-test-projection/graphics->graphics))

(def editor test/editor/string (make-test-document/string) (make-test-projection/string->graphics))

(def editor test/editor/string/empty (make-test-document/string/empty) (make-test-projection/string->graphics))

(def editor test/editor/string/delimited (make-test-document/string) (make-test-projection/string->graphics/delimited))

(def editor test/editor/string/removing (make-test-document/string) (make-test-projection/string->graphics/removing))

(def editor test/editor/string/sorting (make-test-document/string) (make-test-projection/string->graphics/sorting))

(def editor test/editor/text (make-test-document/text) (make-test-projection/text->graphics))

(def editor test/editor/text/ll (make-test-document/text/ll 1000 500) (make-test-projection/text->output))

(def editor test/editor/text/searching/ll (document/search (:selection '((the text/text (text/subseq (the text/text document) 0 0))
                                                                         (the text/text (content-of (the document/search document))))
                                                                       :search "0")
                                            (make-test-document/text/ll 1000 500))
  (sequential
    (nesting
      (document/search->text/text)
      (preserving))
    (make-test-projection/text->output)))

(def editor test/editor/widget (make-test-document/widget/menu) (make-test-projection/widget->graphics))

(def editor test/editor/list (make-test-document/list) (make-test-projection/list->graphics))

(def editor test/editor/table (make-test-document/table) (make-test-projection/table->graphics))

(def editor test/editor/table/nested (make-test-document/table/nested) (make-test-projection/table->graphics))

(def editor test/editor/tree (make-test-document/tree) (make-test-projection/tree->graphics))

(def editor test/editor/tree/ll (make-test-document/tree/ll 10 5 2) (make-test-projection/tree->graphics))

(def editor test/editor/tree/leaf (make-test-document/tree/leaf) (make-test-projection/tree->graphics))

(def editor test/editor/tree/node (make-test-document/tree/node) (make-test-projection/tree->graphics))

(def editor test/editor/tree/empty (make-test-document/tree/empty) (make-test-projection/tree->graphics))

(def editor test/editor/tree/removing (make-test-document/tree) (make-test-projection/tree->graphics/removing))

(def editor test/editor/tree/sorting (make-test-document/tree) (make-test-projection/tree->graphics/sorting))

(def editor test/editor/graph (make-test-document/graph) (make-test-projection/graph->graphics))

(def editor test/editor/state-machine (make-test-document/state-machine) (make-test-projection/state-machine->graphics))

(def editor test/editor/book (make-test-document/book) (make-test-projection/book->graphics))

(def editor test/editor/book/sorting (make-test-document/book) (make-test-projection/book->graphics/sorting))

(def editor test/editor/book/focusing (make-test-document/book) (make-test-projection/book->graphics/focusing))

(def editor test/editor/xml (make-test-document/xml) (make-test-projection/xml->graphics))

(def editor test/editor/xml/empty (make-test-document/xml/empty) (make-test-projection/xml->graphics))

(def editor test/editor/xml/ll (make-test-document/xml/ll 200 100 3) (make-test-projection/xml->graphics))

(def editor test/editor/json (make-test-document/json) (make-test-projection/json->graphics))

(def editor test/editor/json/alternative (make-test-document/json) (make-test-projection/json->graphics/alternative))

(def editor test/editor/json/focusing (make-test-document/json) (make-test-projection/json->graphics/focusing))

(def editor test/editor/json/removing (make-test-document/json) (make-test-projection/json->graphics/removing))

(def editor test/editor/json/reversing (make-test-document/json) (make-test-projection/json->graphics/reversing))

(def editor test/editor/json/sorting (make-test-document/json) (make-test-projection/json->graphics/sorting))

(def editor test/editor/css (make-test-document/css) (make-test-projection/css->graphics))

(def editor test/editor/file-system (make-test-document/file-system) (make-test-projection/file-system->graphics))

(def editor test/editor/java (make-test-document/java) (make-test-projection/java->graphics))

(def editor test/editor/javascript (make-test-document/javascript) (make-test-projection/javascript->graphics))

(def editor test/editor/lisp-form (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics))

(def editor test/editor/common-lisp (make-test-document/common-lisp) (make-test-projection/common-lisp->graphics))

(def editor test/editor/common-lisp/inliner (make-test-document/common-lisp) (make-test-projection/common-lisp/inliner->graphics))

(def editor test/editor/common-lisp/test (make-test-document/common-lisp/test) (make-test-projection/common-lisp->graphics/test))

(def editor test/editor/common-lisp/search (make-test-document/common-lisp/search) (make-test-projection/common-lisp->graphics/search))

(def editor test/editor/common-lisp/split (make-test-document/common-lisp/split) (make-test-projection/common-lisp->graphics/split))

(def editor test/editor/evaluator (make-test-document/evaluator) (make-test-projection/evaluator))

(def editor test/editor/t/null (make-test-document/t/null) (make-test-projection/t->graphics/table))

(def editor test/editor/t/cons (make-test-document/t/cons) (make-test-projection/t->graphics/table))

(def editor test/editor/t/object (make-test-document/t/object) (make-test-projection/t->graphics/table))

(def editor test/editor/t/object/nested (make-test-document/t/object/nested) (make-test-projection/t->graphics/table))

(def editor test/editor/t/text (make-test-document/text) (make-test-projection/t->graphics/table))

(def editor test/editor/t/tree (make-test-document/tree) (make-test-projection/t->graphics/table))

(def editor test/editor/t/xml (make-test-document/xml) (make-test-projection/t->graphics/tree))

(def editor test/editor/t/json (make-test-document/json) (make-test-projection/t->graphics/tree))

(def editor test/editor/inspector/object (make-test-document/inspector/object) (make-test-projection/inspector->graphics))

(def editor test/editor/inspector/object/nested (make-test-document/inspector/object/nested) (make-test-projection/inspector->graphics))

(def editor test/editor/demo (make-test-document/demo) (make-test-projection/demo->graphics))

(def editor test/editor/documentation (make-test-document/documentation) (make-test-projection/documentation->graphics))

(def editor test/editor/workbench (make-test-document/workbench) (make-test-projection/workbench->graphics))
