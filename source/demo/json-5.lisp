;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 5
;;;;
;;;; Goal:
;;;;  - allow inserting jjson array into document
;;;;
;;;; Implementation:
;;;;  - define jjson/array document
;;;;  - define jjson/array->tree/node primitive projection
;;;;  - redefine jjson->tree compound projection

;;;;;;
;;; Document

(def document jjson/array ()
  ((elements :type sequence)))

(def function make-document/jjson/array (elements)
  (make-instance 'jjson/array :elements elements))

(def (macro e) jjson/array (&body elements)
  `(make-document/jjson/array (list ,@elements)))

;;;;;;
;;; Projection

(def projection jjson/array->tree/node ()
  ())

(def function make-projection/jjson/array->tree/node ()
  (make-projection 'jjson/array->tree/node))

(def (macro e) jjson/array->tree/node ()
  '(make-projection/jjson/array->tree/node))

(def function make-projection/jjson->tree ()
  (type-dispatching
    (jjson/nothing (jjson/nothing->tree/leaf))
    (jjson/null (jjson/null->tree/leaf))
    (jjson/boolean (jjson/boolean->tree/leaf))
    (jjson/string (jjson/string->tree/leaf))
    (jjson/array (jjson/array->tree/node))))

;;;;;;
;;; Printer

(def printer jjson/array->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for element :in-sequence (elements-of input))
                                       (for index :from 0)
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push element-iomap child-iomaps)
                                       (unless (first-iteration-p)
                                         (setf (indentation-of (output-of element-iomap)) 1))
                                       (collect (output-of element-iomap)))
                                 :opening-delimiter (make-text/string "[" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "]" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :indentation nil)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def function make-operation/replace (document reference replacement &optional (list-replacement (list replacement)))
  (pattern-case reference
    ((content-of (the document ?a))
     (make-operation/replace-content document replacement))
    ((elt (the list ?a) ?b)
     (make-operation/sequence/replace-element-range document reference list-replacement))
    (?a
     (not-yet-implemented))))

(def function jjson/read-opeartion (projection-iomap gesture-queue operation document-iomap)
  (bind ((latest-gesture (first-elt (gestures-of gesture-queue)))
         (document (input-of document-iomap)))
    (cond ((key-press? latest-gesture :key :sdl-key-delete)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/nothing) nil)
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/nothing ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-n)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/null))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/null ,target))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-t)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/boolean #t))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the jjson/boolean ,target))))) 0)))))))
          ((key-press? latest-gesture :key :sdl-key-f)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/boolean #f))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the jjson/boolean ,target))))) 0)))))))
          ((key-press? latest-gesture :character #\")
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/string ""))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (text-of (the jjson/string ,target))) 0)))))))
          ((key-press? latest-gesture :character #\[)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (make-operation/compound (list (make-operation/replace document target (jjson/array (jjson/nothing)))
                                            (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/nothing (elt (the list (elements-of (the jjson/array ,target))) 0)))) 0)))))))
          ((key-press? latest-gesture :character #\,)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (pattern-case target
               ((elt (the list ?a) ?b)
                (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the list ,?a) ,?b ,?b)) (list (jjson/nothing)))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the jjson/nothing (elt (the list ,?a) ,?b)))) 0)))))))))
          ((typep operation 'operation/replace-selection)
           ;; TODO: map backward
           operation)
          ((typep operation 'operation/quit)
           operation))))

(def reader jjson/array->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (bind ((selection (tree-replace (selection-of (input-of document-iomap)) '(the document document) `(the document ,(input-reference-of document-iomap)))))
    (or (iter (for index :from 0 :below (length (elements-of (input-of projection-iomap))))
              (for child-iomap = (elt (child-iomaps-of projection-iomap) (1+ index)))
              (for child-operation =
                   (when (tree-search selection (input-reference-of child-iomap))
                     (recurse-reader recursion printer-iomap child-iomap gesture-queue operation document-iomap)))
              (when child-operation
                (return child-operation)))
        (jjson/read-opeartion projection-iomap gesture-queue operation document-iomap))))
