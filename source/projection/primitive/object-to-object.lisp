;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/object (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)))

;;;;;;
;;; Construction

(def (function e) make-iomap/object (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/object
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference `(the ,(form-type input) ,input-reference))
              :output output :output-reference (when output-reference `(the ,(form-type output) ,output-reference))))

(def (function e) make-iomap/object* (projection recursion input input-reference output output-reference)
  (make-iomap 'iomap/object
              :projection projection :recursion recursion
              :input input :input-reference input-reference
              :output output :output-reference output-reference))

;;;;;;
;;; Reference applier

(def reference-applier iomap/object (iomap reference function)
  (declare (ignore function))
  (when (tree-search reference (input-reference-of iomap))
    (bind ((candidate (tree-replace reference (input-reference-of iomap) (input-of iomap))))
      (unless (tree-search candidate 'printer-output)
        (eval candidate)))))

;;;;;;
;;; Forward mapper

(def forward-mapper iomap/object (iomap input-reference function)
  (if (equal input-reference (input-reference-of iomap))
      (funcall function iomap (output-reference-of iomap))
      ;; TODO: this is very inefficient
      ;; TODO: is this wrapping with printer-output correct?
      (when (tree-search input-reference `(the ,(form-type (output-of iomap))
                                            (printer-output ,(input-reference-of iomap) ,(projection-of iomap) ,(recursion-of iomap))))
        (funcall function iomap (tree-replace input-reference
                                              `(the ,(form-type (output-of iomap))
                                                 (printer-output ,(input-reference-of iomap) ,(projection-of iomap) ,(recursion-of iomap)))
                                              (output-reference-of iomap))))))

;;;;;;
;;; Backward mapper

(def backward-mapper iomap/object (iomap output-reference function)
  (if (equal output-reference (output-reference-of iomap))
      (funcall function iomap (input-reference-of iomap))
      ;; TODO: this is very inefficient
      ;; TODO: is this wrapping with printer-output correct?
      (when (tree-search output-reference (output-reference-of iomap))
        (funcall function iomap (tree-replace output-reference
                                              (output-reference-of iomap)
                                              `(the ,(form-type (output-of iomap))
                                                 (printer-output ,(input-reference-of iomap) ,(projection-of iomap) ,(recursion-of iomap))))))))

;;;;;;
;;; Projection

(def (projection e) object->object ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/object->object ()
  (make-projection 'object->object))

;;;;;;
;;; Construction

(def (macro e) object->object ()
  `(make-projection/object->object))

;;;;;;
;;; Printer

(def printer object->object (projection recursion iomap input input-reference output-reference)
  (make-iomap/object projection recursion input input-reference output-reference))

;;;;;;
;;; Reader

(def reader object->object (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
