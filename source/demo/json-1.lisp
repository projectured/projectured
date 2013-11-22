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
;;;;  - allow creating empty json document
;;;;
;;;; Implementation:
;;;;  - define json/nothing document
;;;;  - define json/nothing->tree/leaf primitive projection
;;;;  - define json->tree compound projection
;;;;  - define test document and test projection

;;;;;;
;;; Document

(def document json/nothing ()
  ())

(def function make-document/json/nothing ()
  (make-instance 'json/nothing))

(def (macro e) json/nothing ()
  '(make-document/json/nothing))

;;;;;;
;;; Projection

(def projection json/nothing->tree/leaf ()
  ())

(def function make-projection/json/nothing->tree/leaf ()
  (make-projection 'json/nothing->tree/leaf))

(def (macro e) json/nothing->tree/leaf ()
  '(make-projection/json/nothing->tree/leaf))

(def function make-projection/json->tree ()
  (type-dispatching
    (json/nothing (json/nothing->tree/leaf))))

(def (macro e) json->tree ()
  '(make-projection/json->tree))

;;;;;;
;;; Printer

(def printer json/nothing->tree/leaf (projection recursion iomap input input-reference output-reference)
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

(def reader json/nothing->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document-iomap))
  (when (typep operation 'operation/quit)
    operation))

;;;;;;
;;; Test

(in-package :projectured.test)

(def function make-test-document/json ()
  (bind ((selection '(the sequence-position (pos (the string (value (the json/nothing (content-of (the document document))))) 0))))
    (make-test-document (json/nothing) :selection selection)))

(def function make-test-projection/json ()
  (make-test-projection
   (nesting
     (widget->graphics)
     (sequential
       (nesting
         (document->document)
         (recursive (json->tree)))
       (nesting
         (document->document)
         (recursive (tree->text)))
       (nesting
         (document->graphics)
         (text->graphics))))))

(def function test/editor/json ()
  (bind ((editor (make-editor))
         (document (make-test-document/json))
         (projection (make-test-projection/json)))
    (run-read-evaluate-print-loop editor document projection)))
