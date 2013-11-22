;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 3
;;;;
;;;; Goal:
;;;;  - allow inserting json boolean into document
;;;;
;;;; Implementation:
;;;;  - define json/boolean document
;;;;  - define json/boolean->tree/leaf primitive projection
;;;;  - redefine json->tree compound projection

;;;;;;
;;; Document

(def document json/boolean ()
  ((value :type boolean)))

(def function make-document/json/boolean (value)
  (make-instance 'json/boolean :value value))

(def (macro e) json/boolean (value)
  `(make-document/json/boolean ,value))

;;;;;;
;;; Projection

(def projection json/boolean->tree/leaf ()
  ())

(def function make-projection/json/boolean->tree/leaf ()
  (make-projection 'json/boolean->tree/leaf))

(def (macro e) json/boolean->tree/leaf ()
  '(make-projection/json/boolean->tree/leaf))

(def function make-projection/json->tree ()
  (type-dispatching
    (json/nothing (json/nothing->tree/leaf))
    (json/null (json/null->tree/leaf))
    (json/boolean (json/boolean->tree/leaf))))

;;;;;;
;;; Printer

(def printer json/boolean->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content (boolean-to-string (value-p input)))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
         (name-reference `(boolean-to-string (the boolean (value-p (the ,(form-type input) ,input-reference))))))
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
    (cond ((key-press? latest-gesture :key :sdl-key-delete)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/nothing))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the json/nothing ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-n)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/null))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the json/null ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-t)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/boolean #t))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the json/boolean ,target))))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-f)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (json/boolean #f))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the json/boolean ,target))))) 0)))))))
          ((typep operation 'operation/replace-selection)
           ;; TODO: map backward
           operation)
          ((typep operation 'operation/quit)
           operation))))

(def reader json/boolean->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (json/read-opeartion projection-iomap gesture-queue operation document-iomap))
