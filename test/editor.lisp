;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Test

(def suite* (test/editor :in test))

(def test test/editor/graphics ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/graphics) (make-test-projection/graphics->graphics))))

(def test test/editor/string ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/string) (make-test-projection/string->graphics))))

(def test test/editor/string/delimited ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/string) (make-test-projection/string->graphics/delimited))))

(def test test/editor/string/removing ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/string) (make-test-projection/string->graphics/removing))))

(def test test/editor/string/sorting ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/string) (make-test-projection/string->graphics/sorting))))

(def test test/editor/text ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/text) (make-test-projection/text->graphics))))

(def test test/editor/list ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/list) (make-test-projection/list->graphics))))

(def test test/editor/table ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/table) (make-test-projection/table->graphics))))

(def test test/editor/tree ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/tree) (make-test-projection/tree->graphics))))

(def test test/editor/book ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/book) (make-test-projection/book->graphics))))

(def test test/editor/xml ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/xml) (make-test-projection/xml->graphics))))

(def test test/editor/json ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/json) (make-test-projection/json->graphics))))

(def test test/editor/json/focusing ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/json) (make-test-projection/json->graphics/focusing))))

(def test test/editor/json/removing ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/json) (make-test-projection/json->graphics/removing))))

(def test test/editor/json/sorting ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/json) (make-test-projection/json->graphics/sorting))))

(def test test/editor/java ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/java) (make-test-projection/java->graphics))))

(def test test/editor/lisp-form ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics))))

(def test test/editor/walked-lisp-form ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/walked-lisp-form) (make-test-projection/walked-lisp-form->graphics))))

(def test test/editor/evaluator ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/evaluator) (make-test-projection/evaluator))))

(def test test/editor/test ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/test) (make-test-projection/test->graphics))))

(def test test/editor/nested ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/nested) (make-test-projection/nested->graphics))))

(def test test/editor/complex ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/complex) (make-test-projection/complex->graphics))))

(def test test/editor/t ()
  (finishes (run-read-evaluate-print-loop (make-editor) (make-test-document/t) (make-test-projection/t->graphics))))

(def test test/editor/wow ()
  (run-read-evaluate-print-loop (make-editor) (make-test-document/wow) (make-test-projection/wow->graphics)))
