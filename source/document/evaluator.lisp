;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document evaluator/base ()
  ())

(def document evaluator/toplevel (evaluator/base)
  ((elements :type sequence)))

(def document evaluator/form (evaluator/base)
  ((form :type document)
   (result :type document)))

;;;;;;
;;; Construction

(def function make-evaluator/toplevel (elements &key selection)
  (make-instance 'evaluator/toplevel :elements elements :selection selection))

(def function make-evaluator/form (form &key result selection)
  (make-instance 'evaluator/form :form form :result result :selection selection))

;;;;;;
;;; Construction

(def macro evaluator/toplevel ((&key selection) &body elements)
  `(make-evaluator/toplevel (document/sequence (:selection (rest ,selection)) ,@elements) :selection ,selection))

(def macro evaluator/form ((&key selection) &body body)
  `(make-evaluator/form ,(first body) :result ,(second body) :selection ,selection))
