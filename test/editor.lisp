;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Test

(def suite* (test/editor :in test))

(def test test/editor/read-eval-print-loop (wrap document selection projection)
  (bind ((editor (make-editor :filename "/tmp/projectured.bmp")))
    (ecase wrap
      ((nil)
       (run-read-evaluate-print-loop editor
                                     document
                                     projection))
      (:plain
       (run-read-evaluate-print-loop editor
                                     (make-test-document/plain (make-test-document/document document))
                                     (make-test-projection/plain (make-test-projection/document projection))))
      (:document
       (run-read-evaluate-print-loop editor
                                     (make-test-document/document document)
                                     (make-test-projection/document projection)))
      (:debug
       (run-read-evaluate-print-loop editor
                                     (make-test-document/shell (make-test-document/debug (make-test-document/document document)))
                                     (make-test-projection/shell (make-test-projection/debug (make-test-projection/document projection)))))
      (:selection
       (run-read-evaluate-print-loop editor
                                     (make-test-document/shell (make-test-document/debug (make-test-document/selection (make-test-document/document document))))
                                     (make-test-projection/shell (make-test-projection/debug (make-test-projection/selection (make-test-projection/document projection))))))
      (:generic
       (run-read-evaluate-print-loop editor
                                     (make-test-document/shell (make-test-document/debug (make-test-document/generic (make-test-document/document document))))
                                     (make-test-projection/shell (make-test-projection/debug (make-test-projection/generic (make-test-projection/document projection))))))
      (:reflection
       (run-read-evaluate-print-loop editor
                                     (make-test-document/reflection document projection)
                                     (make-test-projection/reflection projection)))
      (:ide
       (run-read-evaluate-print-loop editor
                                     (make-test-document/ide document)
                                     (make-test-projection/ide projection))))))

(def test test/editor/graphics (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/graphics) nil (make-test-projection/graphics->graphics)))

(def test test/editor/graphics/ll (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/graphics/ll 1000 500) nil (make-test-projection/graphics->graphics)))

(def test test/editor/string (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/string) nil (make-test-projection/string->graphics)))

(def test test/editor/string/empty (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/string/empty) nil (make-test-projection/string->graphics)))

(def test test/editor/string/delimited (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/string) nil (make-test-projection/string->graphics/delimited)))

(def test test/editor/string/removing (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/string) nil (make-test-projection/string->graphics/removing)))

(def test test/editor/string/sorting (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/string) nil (make-test-projection/string->graphics/sorting)))

(def test test/editor/text (&key wrap (content (make-test-document/text)) (selection (make-test-selection/text/character)))
  (test/editor/read-eval-print-loop wrap content selection (make-test-projection/text->graphics)))

(def test test/editor/text/ll (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text/ll 1000 500) nil (make-test-projection/text->output)))

(def test test/editor/text/pos (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) '((the text/text (text/subseq (the text/text document) 2 2)) (the text/text (content-of (the document document)))) (make-test-projection/text->graphics)))

(def test test/editor/text/elt (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) '((the text/text (text/subseq (the text/text document) 2 3)) (the text/text (content-of (the document document)))) (make-test-projection/text->graphics)))

(def test test/editor/text/box (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) '((the sequence-box (text/subbox (the text/text document) 5 18))) (make-test-projection/text->graphics)))

(def test test/editor/widget (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/widget/menu) nil (make-test-projection/widget->graphics)))

(def test test/editor/list (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/list) nil (make-test-projection/list->graphics)))

(def test test/editor/table (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/table) (make-test-selection/table/character) (make-test-projection/table->graphics)))

(def test test/editor/table/nested (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/table/nested) (make-test-selection/table/character/nested) (make-test-projection/table->graphics)))

(def test test/editor/tree (&key wrap (content (make-test-document/tree)))
  (test/editor/read-eval-print-loop wrap content (make-test-selection/tree/character) (make-test-projection/tree->graphics)))

(def test test/editor/tree/ll (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree/ll 10 5 2) nil (make-test-projection/tree->graphics)))

(def test test/editor/tree/leaf (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree/leaf) nil (make-test-projection/tree->graphics)))

(def test test/editor/tree/node (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree/node) nil (make-test-projection/tree->graphics)))

(def test test/editor/tree/empty (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree/empty) nil (make-test-projection/tree->graphics)))

(def test test/editor/tree/removing (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree) nil (make-test-projection/tree->graphics/removing)))

(def test test/editor/tree/sorting (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree) nil (make-test-projection/tree->graphics/sorting)))

(def test test/editor/graph (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/graph) nil (make-test-projection/graph->graphics)))

(def test test/editor/state-machine (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/state-machine) nil (make-test-projection/state-machine->graphics)))

(def test test/editor/book (&key wrap (content (make-test-document/book)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/book->graphics)))

(def test test/editor/book/sorting (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/book) nil (make-test-projection/book->graphics/sorting)))

(def test test/editor/xml (&key wrap (content (make-test-document/xml)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/xml->graphics)))

(def test test/editor/xml/empty (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/xml/empty) nil (make-test-projection/xml->graphics)))

(def test test/editor/json (&key wrap (content (make-test-document/json)) (selection (make-test-selection/json/character-position)))
  (test/editor/read-eval-print-loop wrap content selection (make-test-projection/json->graphics)))

(def test test/editor/json/focusing (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/focusing)))

(def test test/editor/json/removing (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/removing)))

(def test test/editor/json/reversing (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/reversing)))

(def test test/editor/json/sorting (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/sorting)))

(def test test/editor/file-system (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/file-system) nil (make-test-projection/file-system->graphics)))

(def test test/editor/java (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/java) nil (make-test-projection/java->graphics)))

(def test test/editor/javascript (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/javascript) nil (make-test-projection/javascript->graphics)))

(def test test/editor/lisp-form (&key wrap (content (make-test-document/lisp-form)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/lisp-form->graphics)))

(def test test/editor/common-lisp (&key wrap (content (make-test-document/common-lisp)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/common-lisp->graphics)))

(def test test/editor/common-lisp/inliner (&key wrap (content (make-test-document/common-lisp)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/common-lisp/inliner->graphics)))

(def test test/editor/common-lisp/test (&key wrap (content (make-test-document/common-lisp/test)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/common-lisp->graphics/test)))

(def test test/editor/common-lisp/search (&key wrap (content (make-test-document/common-lisp/search)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/common-lisp->graphics/search)))

(def test test/editor/common-lisp/split (&key wrap (content (make-test-document/common-lisp/split)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/common-lisp->graphics/split)))

(def test test/editor/evaluator (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/evaluator) nil (make-test-projection/evaluator)))

(def test test/editor/t (content &key wrap selection)
  (test/editor/read-eval-print-loop wrap content selection (make-test-projection/t->graphics/table)))

(def test test/editor/t/null (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/null) nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/cons (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/cons) nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/object (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/object) nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/object/nested (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/object/nested) nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/text (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/tree (&key wrap (content (make-test-document/tree)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/t->graphics/table)))

(def test test/editor/t/xml (&key wrap (content (make-test-document/xml)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/t->graphics/tree)))

(def test test/editor/t/json (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) nil (make-test-projection/t->graphics/tree)))

(def test test/editor/inspector/object (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/inspector/object) nil (make-test-projection/inspector->graphics)))

(def test test/editor/inspector/object/nested (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/inspector/object/nested) nil (make-test-projection/inspector->graphics)))

(def test test/editor/demo (&key wrap (content (make-test-document/demo)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/demo->graphics)))

(def test test/editor/documentation (&key wrap (content (make-test-document/documentation)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/documentation->graphics)))
