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

(def printer document->document (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (typed-output-reference `(the ,(form-type input) ,output-reference))
         (input-content (content-of input))
         (iomap (recurse-printer  recursion input-content `(content-of ,typed-input-reference) `(content-of ,typed-output-reference)))
         (output-content (output-of iomap))
         (input-selection (selection-of input))
         (output-selection nil))
    (map-forward iomap (tree-replace input-selection '(the document document) typed-input-reference)
                 (lambda (iomap output-reference)
                   (declare (ignore iomap))
                   (setf output-selection output-reference)))
    (bind ((output (make-document output-content :selection (tree-replace output-selection output-reference 'document))))
      (make-iomap/recursive projection recursion input input-reference output output-reference
                            (list (make-iomap/object projection recursion input input-reference output output-reference) iomap)))))

;;;;;;
;;; Reader

(def reader document->document (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection))
  (bind ((input (input-of projection-iomap))
         (operation (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation document)))
    (when operation
      ;; TODO: factor common parts
      (cond ((typep operation 'operation/replace-selection)
             (bind ((domain-reference nil))
               (map-backward projection-iomap (tree-replace (selection-of operation) '(the document document) `(the document ,(output-reference-of projection-iomap)))
                             (lambda (iomap reference)
                               (declare (ignore iomap))
                               (setf domain-reference reference)))
               (if domain-reference
                   (make-operation/replace-selection input (tree-replace domain-reference `(the document ,(input-reference-of projection-iomap)) '(the document document)))
                   operation)))
            ((typep operation 'operation/sequence/replace-element-range)
             (bind ((domain-reference nil))
               (map-backward projection-iomap (tree-replace (target-of operation) '(the document document) `(the document ,(output-reference-of projection-iomap)))
                             (lambda (iomap reference)
                               (declare (ignore iomap))
                               (setf domain-reference reference)))
               (if domain-reference
                   (make-operation/sequence/replace-element-range input (tree-replace domain-reference `(the document ,(input-reference-of projection-iomap)) '(the document document)) (replacement-of operation))
                   operation)))
            (t operation)))))
