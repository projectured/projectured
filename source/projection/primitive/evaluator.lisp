;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection evaluator ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/evaluator ()
  (make-projection 'evaluator))

;;;;;;
;;; Construction

(def (macro e) evaluator ()
  '(make-projection/evaluator))

;;;;;;
;;; Printer

(def printer evaluator (projection recursion input input-reference)
  (bind ((form (printer-output input (make-projection/t->form)))
         (output (block nil
                   (handler-bind ((condition (lambda (c)
                                               (unless (typep c 'warning)
                                                 (return (princ-to-string c))))))
                     (multiple-value-list (eval form))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader evaluator (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
