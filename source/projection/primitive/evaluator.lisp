;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def class* environment ()
  ((bindings)))

(def projection evaluator ()
  ())

;;;;;;
;;; Construction

(def (function e) make-environment (bindings)
  (make-instance 'environment :bindings bindings))

(def (function e) make-projection/evaluator ()
  (make-projection 'evaluator))

;;;;;;
;;; Construction

(def (macro e) evaluator ()
  '(make-projection/evaluator))

;;;;;;
;;; Printer

(def printer evaluator (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (labels ((evaluate (environment form)
                            (etypecase form
                              (common-lisp/constant (value-of form))
                              (common-lisp/application
                               (bind ((operator (operator-of form))
                                      (arguments (iter (for argument :in (arguments-of form))
                                                       (collect (evaluate environment argument)))))
                                 (apply operator arguments))))))
                   (evaluate (make-environment nil) input))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

;;;;;;
;;; Reader

(def reader evaluator (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document-iomap))
  operation)
