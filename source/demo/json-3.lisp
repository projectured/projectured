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
;;;;  - allow inserting jjson boolean into document
;;;;
;;;; Implementation:
;;;;  - define jjson/boolean document
;;;;  - define jjson/boolean->tree/leaf primitive projection
;;;;  - redefine jjson->tree compound projection

;;;;;;
;;; Document

(def document jjson/boolean ()
  ((value :type boolean)))

(def function make-document/jjson/boolean (value)
  (make-instance 'jjson/boolean :value value))

(def (macro e) jjson/boolean (value)
  `(make-document/jjson/boolean ,value))

;;;;;;
;;; Projection

(def projection jjson/boolean->tree/leaf ()
  ())

(def function make-projection/jjson/boolean->tree/leaf ()
  (make-projection 'jjson/boolean->tree/leaf))

(def (macro e) jjson/boolean->tree/leaf ()
  '(make-projection/jjson/boolean->tree/leaf))

(def function make-projection/jjson->tree ()
  (type-dispatching
    (jjson/nothing (jjson/nothing->tree/leaf))
    (jjson/null (jjson/null->tree/leaf))
    (jjson/boolean (jjson/boolean->tree/leaf))))

;;;;;;
;;; Printer

(def printer jjson/boolean->tree/leaf (projection recursion iomap input input-reference output-reference)
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

(def function jjson/read-opeartion (projection-iomap gesture-queue operation document-iomap)
  (bind ((latest-gesture (first-elt (gestures-of gesture-queue)))
         (document (input-of document-iomap)))
    (cond ((key-press? latest-gesture :key :sdl-key-delete)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (jjson/nothing))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/nothing ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-n)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (jjson/null))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/null ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-t)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (jjson/boolean #t))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the jjson/boolean ,target))))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-f)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace-content document (jjson/boolean #f))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the jjson/boolean ,target))))) 0)))))))
          ((typep operation 'operation/replace-selection)
           ;; TODO: map backward
           operation)
          ((typep operation 'operation/quit)
           operation))))

(def reader jjson/boolean->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (jjson/read-opeartion projection-iomap gesture-queue operation document-iomap))
