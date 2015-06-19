;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Interactive editor test suite

(def suite* (test/editor :in test))

(def test test/editor/run-read-evaluate-print-loop (&key demo instance document projection filename search-string
                                                         provide-all (provide-document provide-all) (provide-clipboard provide-all) (provide-scroll provide-all) (provide-search provide-all) (provide-generic provide-all)
                                                         display-gestures display-selection
                                                         cache-graphics debug-cache-graphics clip-graphics-changes debug-clip-graphics-changes mark-graphics-changes
                                                         measure (measure-reader measure) (measure-evaluator measure) (measure-printer measure))
  (with-demo (:name demo :instance instance)
    (bind ((editor (make-editor :filename filename))
           (document (funcall (apply 'compose (optional-list
                                               (when display-gestures 'make-test-document/gesture)
                                               (when display-selection 'make-test-document/selection)
                                               (when provide-scroll 'make-test-document/scroll)
                                               (when provide-document 'make-test-document/document)
                                               (when provide-clipboard 'make-test-document/clipboard)
                                               (when provide-search (rcurry 'make-test-document/search search-string))
                                               'identity))
                              document))
           (projections (append (list (funcall (apply 'compose (optional-list
                                                                (when display-gestures 'make-test-projection/gesture)
                                                                (when display-selection 'make-test-projection/selection)
                                                                (when provide-scroll 'make-test-projection/scroll)
                                                                (when provide-document 'make-test-projection/document)
                                                                (when provide-clipboard 'make-test-projection/clipboard)
                                                                (when provide-search 'make-test-projection/search)
                                                                (when provide-generic 'make-test-projection/generic)
                                                                'identity))
                                               (if provide-search
                                                   projection
                                                   (sequential
                                                     projection
                                                     (make-test-projection/text->output)))))
                                (when cache-graphics
                                  (list (recursive
                                          (graphics->graphics@cache-graphics :debug debug-cache-graphics))))
                                (when mark-graphics-changes
                                  (list (recursive
                                          (graphics->graphics@mark-changes))))
                                (list (make-instance 'projection
                                                     :reader (lambda (projection recursion input printer-iomap)
                                                               (declare (ignore projection recursion printer-iomap))
                                                               input)
                                                     :printer (lambda (projection recursion input input-reference)
                                                                (make-iomap/object projection recursion input input-reference (as (make-graphics/canvas (list (make-graphics/rectangle 0 (make-2d 1280 720) :fill-color *color/white*) input)
                                                                                                                                                        0))))))
                                (when clip-graphics-changes
                                  (list (graphics/canvas->graphics/canvas@clip-changes :debug debug-clip-graphics-changes)))))
           (projection (if (= 1 (length projections))
                           (first projections)
                           (make-projection/sequential projections))))
      (run-read-evaluate-print-loop editor document projection :measure-reader measure-reader :measure-evaluator measure-evaluator :measure-printer measure-printer))))
