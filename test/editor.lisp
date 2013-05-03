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

(def test test/editor/styled-string ()
  (test/editor/read-eval-print-loop (make-test-document/styled-string) (make-test-projection/styled-string->graphics)))

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

(def test test/editor/json ()
  (test/editor/read-eval-print-loop (make-test-document/json) (make-test-projection/json->graphics)))

(def test test/editor/json/focusing ()
  (test/editor/read-eval-print-loop (make-test-document/json) (make-test-projection/json->graphics/focusing)))

(def test test/editor/json/removing ()
  (test/editor/read-eval-print-loop (make-test-document/json) (make-test-projection/json->graphics/removing)))

(def test test/editor/json/sorting ()
  (test/editor/read-eval-print-loop (make-test-document/json) (make-test-projection/json->graphics/sorting)))

(def test test/editor/java ()
  (test/editor/read-eval-print-loop (make-test-document/java) (make-test-projection/java->graphics)))

(def test test/editor/javascript ()
  (test/editor/read-eval-print-loop (make-test-document/javascript) (make-test-projection/javascript->graphics)))

(def test test/editor/lisp-form ()
  (test/editor/read-eval-print-loop (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics)))

(def test test/editor/common-lisp ()
  (test/editor/read-eval-print-loop (make-test-document/common-lisp) (make-test-projection/common-lisp->graphics)))

(def test test/editor/evaluator ()
  (test/editor/read-eval-print-loop (make-test-document/evaluator) (make-test-projection/evaluator)))

(def test test/editor/test ()
  (test/editor/read-eval-print-loop (make-test-document/test) (make-test-projection/test->graphics)))

(def test test/editor/nested ()
  (test/editor/read-eval-print-loop (make-test-document/nested) (make-test-projection/nested->graphics)))

(def test test/editor/complex ()
  (test/editor/read-eval-print-loop (make-test-document/complex) (make-test-projection/complex->graphics)))

(def test test/editor/t ()
  (test/editor/read-eval-print-loop (make-test-document/t) (make-test-projection/t->graphics)))

(def test test/editor/demo ()
  (test/editor/read-eval-print-loop (make-test-document/demo) (make-test-projection/demo->graphics)))

(def test test/editor/wow ()
  (test/editor/read-eval-print-loop (make-test-document/wow) (make-test-projection/wow->graphics)))
