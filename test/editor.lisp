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
       (run-read-evaluate-print-loop editor document projection))
      (:plain
       (run-read-evaluate-print-loop editor (make-test-document/plain document :selection selection) (make-test-projection/plain projection)))
      (:debug
       (run-read-evaluate-print-loop editor (make-test-document/shell (make-test-document/debug document)) (make-test-projection/shell (make-test-projection/debug projection))))
      (:selection
       (run-read-evaluate-print-loop editor (make-test-document/shell (make-test-document/debug (make-test-document/selection document))) (make-test-projection/shell (make-test-projection/debug (make-test-projection/selection projection)))))
      (:generic
       (run-read-evaluate-print-loop editor (make-test-document/generic document :selection selection) (make-test-projection/generic projection)))
      (:reflection
       (run-read-evaluate-print-loop editor (make-test-document/reflection document :selection selection :projection projection) (make-test-projection/reflection projection)))
      (:ide
       (run-read-evaluate-print-loop editor (make-test-document/ide document :selection selection) (make-test-projection/ide projection))))))

(def test test/editor/graphics (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/graphics) nil (make-test-projection/graphics->graphics)))

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

(def test test/editor/text/pos (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) '(the sequence-position (text/pos (the text/text (content-of (the document document))) 2)) (make-test-projection/text->graphics)))

(def test test/editor/text/elt (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) '(the character (text/elt (the text/text (content-of (the document document))) 2)) (make-test-projection/text->graphics)))

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

(def test test/editor/lisp-form (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/lisp-form) nil (make-test-projection/lisp-form->graphics)))

(def test test/editor/common-lisp (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/common-lisp) nil (make-test-projection/common-lisp->graphics)))

(def test test/editor/evaluator (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/evaluator) nil (make-test-projection/evaluator)))

(def test test/editor/test (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/test) nil (make-test-projection/test->graphics)))

(def test test/editor/t (content &key wrap selection)
  (test/editor/read-eval-print-loop wrap content selection (make-test-projection/t->graphics)))

(def test test/editor/t/null (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/null) nil (make-test-projection/t->graphics)))

(def test test/editor/t/cons (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/cons)
                                    '((the sequence-position (pos (the string document) 1))
                                      (the string (elt (the sequence document) 0))
                                      (the sequence (content-of (the document document))))
                                    (make-test-projection/t->graphics)))

(def test test/editor/t/object (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/object)
                                    '((the sequence-position (pos (the string document) 1))
                                      (the string (slot-value (the test-object document) 'test-instance-slot)))
                                    (make-test-projection/t->graphics)))

(def test test/editor/t/object/nested (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/t/object/nested)
                                    '((the sequence-position (pos (the string document) 1))
                                      (the string (slot-value (the test-object document) 'test-instance-slot))
                                      (the test-object (slot-value (the test-object document) 'test-instance-slot)))
                                    (make-test-projection/t->graphics)))

(def test test/editor/t/text (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/text) nil (make-test-projection/t->graphics)))

(def test test/editor/t/tree (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/tree) nil (make-test-projection/t->graphics)))

(def test test/editor/t/xml (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/xml) nil (make-test-projection/t->graphics)))

(def test test/editor/t/json (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/json) nil (make-test-projection/t->graphics)))

(def test test/editor/inspector/object (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/inspector/object) nil (make-test-projection/inspector->graphics)))

(def test test/editor/inspector/object/nested (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/inspector/object/nested) nil (make-test-projection/inspector->graphics)))

(def test test/editor/demo (&key wrap)
  (test/editor/read-eval-print-loop wrap (make-test-document/demo) nil (make-test-projection/demo->graphics)))

(def test test/editor/documentation (&key wrap (content (make-test-document/documentation)))
  (test/editor/read-eval-print-loop wrap content nil (make-test-projection/documentation->graphics)))
