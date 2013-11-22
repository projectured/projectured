;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 4
;;;;
;;;; Goal:
;;;;  - allow inserting json string into document
;;;;
;;;; Implementation:
;;;;  - define json/string document
;;;;  - define json/string->tree/leaf primitive projection
;;;;  - redefine json->tree compound projection

;;;;;;
;;; Document

(def document json/string ()
  ((text :type string)))

(def function make-json/string (text)
  (make-instance 'json/string :text text))

(def (macro e) json/string (text)
  `(make-json/string ,text))

;;;;;;
;;; Projection

(def projection json/string->tree/leaf ()
  ())

(def (function e) make-projection/json/string->tree/leaf ()
  (make-projection 'json/string->tree/leaf))

(def (macro e) json/string->tree/leaf ()
  '(make-projection/json/string->tree/leaf))

(def function make-projection/json->tree ()
  (type-dispatching
    (json/nothing (json/nothing->tree/leaf))
    (json/null (json/null->tree/leaf))
    (json/boolean (json/boolean->tree/leaf))
    (json/string (json/string->tree/leaf))))

;;;;;;
;;; Printer

(def printer json/string->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content (text-of input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                 :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
         (text-reference `(text-of (the ,(form-type input) ,input-reference))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               (make-iomap/string output-content text-reference 0
                                                  output-content `(content-of (the text/string (content-of (the tree/leaf ,output-reference)))) 0
                                                  (length (text-of input)))))))

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
          ((key-press? latest-gesture :character #\")
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (json/string ""))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (text-of (the json/string ,target))) 0)))))))
          ((typep operation 'operation/replace-selection)
           ;; TODO: map backward
           operation)
          ((typep operation 'operation/quit)
           operation))))

(def reader json/string->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (json/read-opeartion projection-iomap gesture-queue operation document-iomap))