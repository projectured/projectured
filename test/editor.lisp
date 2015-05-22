;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Interactive editor test suite

(def suite* (test/editor :in test))

(def test test/editor/run-read-evaluate-print-loop (&key demo instance document projection filename wrap display-gestures display-selection cache-graphics debug-cache-graphics clip-graphics-changes debug-clip-graphics-changes mark-graphics-changes measure (measure-reader measure) (measure-evaluator measure) (measure-printer measure))
  (with-demo (:name demo :instance instance)
    (bind ((editor (make-editor :filename filename))
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
                        (make-test-document/debug (make-test-document/plain (make-test-document/document document))))
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
                                         (make-test-projection/debug (make-test-projection/plain (make-test-projection/document projection))))
                                        (:selection
                                         (make-test-projection/shell (make-test-projection/debug (make-test-projection/selection (make-test-projection/document projection)))))
                                        (:generic
                                         (make-test-projection/shell (make-test-projection/debug (make-test-projection/generic (make-test-projection/document projection)))))
                                        (:reflection
                                         (make-test-projection/reflection projection))
                                        (:ide
                                         (make-test-projection/ide projection))))
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
                                                                (make-iomap/object projection recursion input input-reference (as (make-graphics/canvas (list (make-graphics/rectangle (make-2d 0 0) (make-2d 1280 720) :fill-color *color/white*) input)
                                                                                                                                                        (make-2d 0 0)))))))
                                (when clip-graphics-changes
                                  (list (graphics/canvas->graphics/canvas@clip-changes :debug debug-clip-graphics-changes)))))
           (projection (if (= 1 (length projections))
                           (first projections)
                           (make-projection/sequential projections))))
      (run-read-evaluate-print-loop editor document projection :measure-reader measure-reader :measure-evaluator measure-evaluator :measure-printer measure-printer))))
