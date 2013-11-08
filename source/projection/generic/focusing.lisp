;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) focusing ()
  ((part :type reference)
   (part-evaluator :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/focusing (part)
  (make-projection 'focusing
                   :part part
                   :part-evaluator (compile nil `(lambda (document) ,part))))

;;;;;;
;;; Construction

(def (macro e) focusing (part)
  `(make-projection/focusing ,part))

;;;;;;
;;; Printer

(def printer focusing (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((part (part-of projection))
         (output (funcall (part-evaluator-of projection) input)))
    (make-iomap/recursive projection recursion input (tree-replace part 'document input-reference) output output-reference)))

;;;;;;
;;; Reader

(def reader focusing (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap gesture-queue))
  (bind ((document (input-of document-iomap)))
    (labels ((recurse (child-operation)
               (cond ((typep child-operation 'operation/compound)
                      (bind ((child-operations (mapcar #'recurse (elements-of child-operation))))
                        (unless (some 'null child-operations)
                          (make-operation/compound child-operations))))
                     ((typep child-operation 'operation/replace-selection)
                      (awhen (map-backward/clever (selection-of child-operation) projection-iomap document-iomap)
                        (make-operation/replace-selection document (tree-replace it `(the document ,(input-reference-of projection-iomap)) '(the document document)))))
                     ((typep child-operation 'operation/sequence/replace-element-range)
                      (awhen (map-backward/clever (target-of child-operation) projection-iomap document-iomap)
                        (make-operation/sequence/replace-element-range document (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation))))
                     ((typep child-operation 'operation/number/replace-range)
                      (awhen (map-backward/clever (target-of child-operation) projection-iomap document-iomap)
                        (make-operation/number/replace-range document (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation))))
                     (t
                      (operation/read-backward operation projection-iomap document-iomap)))))
      (recurse operation))))
