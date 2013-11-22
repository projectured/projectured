;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Test

(def suite* (test/editor :in test))

(def test test/editor/read-eval-print-loop (document projection)
  (finishes (run-read-evaluate-print-loop (make-editor :filename "/tmp/projectured.bmp") document projection)))

(def test test/editor/read-eval-print-loop* (content selection projection)
  (bind ((editor (make-editor :filename "/tmp/projectured.bmp"))
         (document (make-test-document content :selection selection :projection projection))
         (projection (make-test-projection projection)))
    (finishes (run-read-evaluate-print-loop editor document projection))))

(def test test/editor/graphics ()
  (test/editor/read-eval-print-loop (make-test-document/graphics) (make-test-projection/graphics->graphics)))

(def test test/editor/string ()
  (test/editor/read-eval-print-loop (make-test-document/string) (make-test-projection/string->graphics)))

(def test test/editor/string/empty ()
  (test/editor/read-eval-print-loop (make-test-document/string/empty) (make-test-projection/string->graphics)))

(def test test/editor/string/delimited ()
  (test/editor/read-eval-print-loop (make-test-document/string) (make-test-projection/string->graphics/delimited)))

(def test test/editor/string/removing ()
  (test/editor/read-eval-print-loop (make-test-document/string) (make-test-projection/string->graphics/removing)))

(def test test/editor/string/sorting ()
  (test/editor/read-eval-print-loop (make-test-document/string) (make-test-projection/string->graphics/sorting)))

(def test test/editor/text/pos ()
  (test/editor/read-eval-print-loop (make-test-document/text/pos) (make-test-projection/text->graphics)))

(def test test/editor/text/elt ()
  (test/editor/read-eval-print-loop (make-test-document/text/elt) (make-test-projection/text->graphics)))

(def test test/editor/text/box ()
  (test/editor/read-eval-print-loop (make-test-document/text/box) (make-test-projection/text->graphics)))

(def test test/editor/text ()
  (test/editor/read-eval-print-loop (make-test-document/text) (make-test-projection/text->graphics)))

(def test test/editor/list ()
  (test/editor/read-eval-print-loop (make-test-document/list) (make-test-projection/list->graphics)))

(def test test/editor/table ()
  (test/editor/read-eval-print-loop (make-test-document/table) (make-test-projection/table->graphics)))

(def test test/editor/tree ()
  (test/editor/read-eval-print-loop (make-test-document/tree) (make-test-projection/tree->graphics)))

(def test test/editor/tree/leaf ()
  (test/editor/read-eval-print-loop (make-test-document/tree/leaf) (make-test-projection/tree->graphics)))

(def test test/editor/tree/node ()
  (test/editor/read-eval-print-loop (make-test-document/tree/node) (make-test-projection/tree->graphics)))

(def test test/editor/tree/empty ()
  (test/editor/read-eval-print-loop (make-test-document/tree/empty) (make-test-projection/tree->graphics)))

(def test test/editor/tree/removing ()
  (test/editor/read-eval-print-loop (make-test-document/tree) (make-test-projection/tree->graphics/removing)))

(def test test/editor/tree/sorting ()
  (test/editor/read-eval-print-loop (make-test-document/tree) (make-test-projection/tree->graphics/sorting)))

(def test test/editor/graph ()
  (test/editor/read-eval-print-loop (make-test-document/graph) (make-test-projection/graph->graphics)))

(def test test/editor/state-machine ()
  (test/editor/read-eval-print-loop (make-test-document/state-machine) (make-test-projection/state-machine->graphics)))

(def test test/editor/book ()
  (test/editor/read-eval-print-loop (make-test-document/book) (make-test-projection/book->graphics)))

(def test test/editor/xml ()
  (test/editor/read-eval-print-loop (make-test-document/xml) (make-test-projection/xml->graphics)))

(def test test/editor/xml/empty ()
  (test/editor/read-eval-print-loop (make-test-document/xml/empty) (make-test-projection/xml->graphics)))

(def test test/editor/json ()
  (test/editor/read-eval-print-loop* (make-test-content/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics)))

(def test test/editor/json/focusing ()
  (test/editor/read-eval-print-loop* (make-test-content/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/focusing)))

(def test test/editor/json/removing ()
  (test/editor/read-eval-print-loop* (make-test-content/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/removing)))

(def test test/editor/json/sorting ()
  (test/editor/read-eval-print-loop* (make-test-content/json) (make-test-selection/json/character-position) (make-test-projection/json->graphics/sorting)))

(def test test/editor/json/common-lisp ()
  (test/editor/read-eval-print-loop* (bind ((h (make-adjustable-string "Hello World")))
                                       (json/array
                                         (make-instance 'common-lisp/application :operator 'make-instance
                                                        :arguments (list (make-instance 'common-lisp/constant :value 'json/number)
                                                                         (make-instance 'common-lisp/constant :value :value)
                                                                         (make-instance 'common-lisp/application :operator 'length :arguments (list (make-instance 'common-lisp/constant :value h)))))
                                         (json/string h)))
                                     (make-test-selection/json/character-position)
                                     (sequential
                                       (nesting
                                         (document->document)
                                         (recursive
                                           (type-dispatching
                                             (common-lisp/base (evaluator))
                                             (json/array (copying))
                                             (json/base (preserving))
                                             (list (copying)))))
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
                                         (make-test-projection/text->output)))))

(def test test/editor/java ()
  (test/editor/read-eval-print-loop (make-test-document/java) (make-test-projection/java->graphics)))

(def test test/editor/javascript ()
  (test/editor/read-eval-print-loop (make-test-document/javascript) (make-test-projection/javascript->graphics)))

(def test test/editor/lisp-form ()
  (test/editor/read-eval-print-loop (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics)))

(def test test/editor/common-lisp ()
  (test/editor/read-eval-print-loop* (make-test-content/common-lisp) nil (make-test-projection/common-lisp->graphics)))

(def test test/editor/evaluator ()
  (test/editor/read-eval-print-loop (make-test-document/evaluator) (make-test-projection/evaluator)))

(def test test/editor/test ()
  (test/editor/read-eval-print-loop (make-test-document/test) (make-test-projection/test->graphics)))

(def test test/editor/nested ()
  (test/editor/read-eval-print-loop (make-test-document/nested) (make-test-projection/nested->graphics)))

(def test test/editor/complex ()
  (test/editor/read-eval-print-loop (make-test-document/complex) (make-test-projection/complex->graphics)))

(def test test/editor/t/null ()
  (test/editor/read-eval-print-loop* (make-test-content/t/null) nil (make-test-projection/t->graphics)))

(def test test/editor/t/text ()
  (test/editor/read-eval-print-loop* (make-test-content/text) nil (make-test-projection/t->graphics)))

(def test test/editor/t/tree ()
  (test/editor/read-eval-print-loop* (make-test-content/tree) nil (make-test-projection/t->graphics)))

(def test test/editor/t/xml ()
  (test/editor/read-eval-print-loop* (make-test-content/xml) nil (make-test-projection/t->graphics)))

(def test test/editor/t/json ()
  (test/editor/read-eval-print-loop* (make-test-content/json) nil (make-test-projection/t->graphics)))

(def test test/editor/demo ()
  (test/editor/read-eval-print-loop (make-test-document/demo) (make-test-projection/demo->graphics)))

(def test test/editor/wow ()
  (test/editor/read-eval-print-loop (make-test-document/wow) (make-test-projection/wow->graphics)))
