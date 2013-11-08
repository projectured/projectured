;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 1
;;;;
;;;; Goal:
;;;;  - allow creating empty jjson document
;;;;
;;;; Implementation:
;;;;  - define jjson/nothing document
;;;;  - define jjson/nothing->tree/leaf primitive projection
;;;;  - define jjson->tree compound projection
;;;;  - define test document and test projection

;;;;;;
;;; Document

(def document jjson/nothing ()
  ())

(def function make-document/jjson/nothing ()
  (make-instance 'jjson/nothing))

(def (macro e) jjson/nothing ()
  '(make-document/jjson/nothing))

;;;;;;
;;; Projection

(def projection jjson/nothing->tree/leaf ()
  ())

(def function make-projection/jjson/nothing->tree/leaf ()
  (make-projection 'jjson/nothing->tree/leaf))

(def (macro e) jjson/nothing->tree/leaf ()
  '(make-projection/jjson/nothing->tree/leaf))

(def function make-projection/jjson->tree ()
  (type-dispatching
    (jjson/nothing (jjson/nothing->tree/leaf))))

(def (macro e) jjson->tree ()
  '(make-projection/jjson->tree))

;;;;;;
;;; Printer

(def printer jjson/nothing->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content "")
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
         (name-reference `(value (the ,(form-type input) ,input-reference))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output-content name-reference 0
                                                   output-content `(content-of (the text/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length output-content))))))

;;;;;;
;;; Reader

(def reader jjson/nothing->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document-iomap))
  (when (typep operation 'operation/quit)
    operation))

;;;;;;
;;; Test

(in-package :projectured.test)

(def function make-test-document/jjson ()
  (bind ((selection '(the sequence-position (pos (the string (value (the jjson/nothing (content-of (the document document))))) 0))))
    (make-test-document (jjson/nothing) :selection selection)))

(def function make-test-projection/jjson ()
  (make-test-projection
   (nesting
     (widget->graphics)
     (sequential
       (nesting
         (document->document)
         (recursive (jjson->tree)))
       (nesting
         (document->document)
         (recursive (tree->styled-string)))
       (nesting
         (document->graphics)
         (styled-string->graphics))))))

(def function test/editor/jjson ()
  (bind ((editor (make-editor))
         (document (make-test-document/jjson))
         (projection (make-test-projection/jjson)))
    (run-read-evaluate-print-loop editor document projection)))
