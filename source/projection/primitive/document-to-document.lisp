;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document->document ()
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
         (output-selection-cs (bind ((output-selection nil))
                                (map-forward (computed-state-value* iomap-cs) (tree-replace (selection-of input) '(the document document) typed-input-reference)
                                             (lambda (iomap output-reference)
                                               (declare (ignore iomap))
                                               (setf output-selection output-reference)))
                                (tree-replace output-selection output-reference 'document))))
    (bind ((output (make-document (as (output-of (computed-state-value* iomap-cs))) :selection output-selection-cs)))
      (make-iomap/compound projection recursion input input-reference output output-reference
                           (list (make-iomap/object projection recursion input input-reference output output-reference) (computed-state-value* iomap-cs))))))

;;;;;;
;;; Reader

(def reader document->document (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection document-iomap))
  (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation projection-iomap))

;; TODO: rename and move?
(def function operation/read-backward (operation projection-iomap document-iomap)
  (bind ((document (input-of document-iomap)))
    (labels ((recurse (child-operation)
               (cond ((typep child-operation 'operation/quit)
                      child-operation)
                     ((typep child-operation 'operation/save-document)
                      (make-instance 'operation/save-document :document document :filename (filename-of child-operation)))
                     ((typep child-operation 'operation/load-document)
                      (make-instance 'operation/load-document :document document :filename (filename-of child-operation)))
                     ((typep child-operation 'operation/replace-selection)
                      (awhen (map-backward/clever (selection-of child-operation) document-iomap)
                        (make-operation/replace-selection document (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)))))
                     ((typep child-operation 'operation/sequence/replace-element-range)
                      (awhen (map-backward/clever (target-of child-operation) document-iomap)
                        (make-operation/sequence/replace-element-range document (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation))))
                     ((typep child-operation 'operation/number/replace-range)
                      (awhen (map-backward/clever (target-of child-operation) document-iomap)
                        (make-operation/number/replace-range document (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation))))
                     ((typep child-operation 'operation/show-context-sensitive-help)
                      (make-instance 'operation/show-context-sensitive-help
                                     :operations (iter (for gesture->operation :in (operations-of child-operation))
                                                       (for operation = (recurse (operation-of gesture->operation)))
                                                       (when operation
                                                         (collect (make-instance 'gesture->operation
                                                                                 :gesture (gesture-of gesture->operation)
                                                                                 :domain (domain-of gesture->operation)
                                                                                 :description (description-of gesture->operation)
                                                                                 :operation operation))))))
                     ((typep child-operation 'operation/compound)
                      (bind ((child-operations (mapcar #'recurse (elements-of child-operation))))
                        (unless (some 'null child-operations)
                          (make-operation/compound child-operations)))))))
      (recurse operation))))

;; TODO: rename and move?
(def function map-backward/clever (reference document-iomap)
  (bind ((domain-reference nil))
    (map-backward document-iomap (tree-replace reference '(the document document) `(the document ,(output-reference-of document-iomap)))
                  (lambda (iomap input-reference)
                    (declare (ignore iomap))
                    (setf domain-reference input-reference)))
    domain-reference))
