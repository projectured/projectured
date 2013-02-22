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
    ;; TODO: how should we map and why?
    (make-iomap/object projection recursion input (tree-replace part 'document input-reference) output output-reference)
    #+nil
    (make-iomap 'iomap :projection projection :recursion recursion
                :input input :output output
                :forward-mapper (lambda (iomap reference function)
                                  nil)
                :backward-mapper (lambda (iomap reference function)
                                   nil
                                   #+nil
                                   (tree-replace reference output-reference (tree-replace part 'document input-reference)))
                :reference-applier (lambda (iomap reference function)
                                     (not-yet-implemented)))))

;;;;;;
;;; Reader

(def reader focusing (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
