;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) document->document ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/document->document ()
  (make-projection 'document->document))

;;;;;;
;;; Construction

(def (macro e) document->document ()
  '(make-projection/document->document))

;;;;;;
;;; Printer

(def printer document->document (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (typed-output-reference `(the ,(form-type input) ,output-reference))
         (iomap-cs (as (recurse-printer recursion iomap (content-of input) `(content-of ,typed-input-reference) `(content-of ,typed-output-reference))))
         (output-selection-cs (as (bind ((output-selection nil))
                                    (map-forward (computed-state-value iomap-cs) (tree-replace (selection-of input) '(the document document) typed-input-reference)
                                                 (lambda (iomap output-reference)
                                                   (declare (ignore iomap))
                                                   (setf output-selection output-reference)))
                                    (tree-replace output-selection output-reference 'document)))))
    (bind ((output (make-document (as (output-of (computed-state-value iomap-cs))) :selection output-selection-cs)))
      (make-iomap/recursive projection recursion input input-reference output output-reference
                            (list (make-iomap/object projection recursion input input-reference output output-reference) (computed-state-value iomap-cs))))))

;;;;;;
;;; Reader

(def reader document->document (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection document))
  (bind ((input (input-of projection-iomap))
         (child-operation (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation input)))
    (if (eq child-operation operation)
        ;; TODO: factor common parts
        (cond ((typep child-operation 'operation/replace-selection)
               (bind ((domain-reference nil))
                 (map-backward projection-iomap (tree-replace (selection-of child-operation) '(the document document) `(the document ,(output-reference-of projection-iomap)))
                               (lambda (iomap reference)
                                 (declare (ignore iomap))
                                 (setf domain-reference reference)))
                 (if domain-reference
                     (make-operation/replace-selection input (tree-replace domain-reference `(the document ,(input-reference-of projection-iomap)) '(the document document)))
                     child-operation)))
              ((typep child-operation 'operation/sequence/replace-element-range)
               (bind ((domain-reference nil))
                 (map-backward projection-iomap (tree-replace (target-of child-operation) '(the document document) `(the document ,(output-reference-of projection-iomap)))
                               (lambda (iomap reference)
                                 (declare (ignore iomap))
                                 (setf domain-reference reference)))
                 (if domain-reference
                     (make-operation/sequence/replace-element-range input (tree-replace domain-reference `(the document ,(input-reference-of projection-iomap)) '(the document document)) (replacement-of operation))
                     child-operation)))
              (t child-operation))
        child-operation)))
