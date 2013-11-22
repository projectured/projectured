;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 2
;;;;
;;;; Goal:
;;;;  - allow inserting json null into document
;;;;
;;;; Implementation:
;;;;  - define json/null document
;;;;  - define json/null->tree/leaf primitive projection
;;;;  - redefine json->tree compound projection

;;;;;;
;;; Document

(def document json/null ()
  ())

(def function make-document/json/null ()
  (make-instance 'json/null))

(def (macro e) json/null ()
  '(make-document/json/null))

;;;;;;
;;; Projection

(def projection json/null->tree/leaf ()
  ())

(def function make-projection/json/null->tree/leaf ()
  (make-projection 'json/null->tree/leaf))

(def (macro e) json/null->tree/leaf ()
  '(make-projection/json/null->tree/leaf))

(def function make-projection/json->tree ()
  (type-dispatching
    (json/nothing (json/nothing->tree/leaf))
    (json/null (json/null->tree/leaf))))

(def (macro e) json->tree ()
  '(make-projection/json->tree))

;;;;;;
;;; Printer

(def printer json/null->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content "null")
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
         (name-reference `(value (the ,(form-type input) ,input-reference))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               (make-iomap/string output-content name-reference 0
                                                  output-content `(content-of (the text/string (content-of (the tree/leaf ,output-reference)))) 0
                                                  (length output-content))))))

;;;;;;
;;; Reader

(def function json/read-opeartion (projection-iomap gesture-queue operation document-iomap)
  (bind ((latest-gesture (first-elt (gestures-of gesture-queue)))
         (document (input-of document-iomap)))
    (cond ((key-press? latest-gesture :key :sdl-key-n)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/null))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the json/null ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-delete)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/nothing))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the json/nothing ,target))) 0)))))))
          ((typep operation 'operation/replace-selection)
           ;; TODO: map backward
           operation)
          ((typep operation 'operation/quit)
           operation))))

(def reader json/nothing->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (json/read-opeartion projection-iomap gesture-queue operation document-iomap))

(def reader json/null->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (json/read-opeartion projection-iomap gesture-queue operation document-iomap))
