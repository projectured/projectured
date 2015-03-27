;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document template/base ()
  ())

(def document template/recurse-slot (template/base)
  ((name :type symbol)))

(def document template/evaluate-form (template/base)
  ((form :type t)))

;;;;;;
;;; Construction

(def function make-template/recurse-slot (name &key selection)
  (make-instance 'template/recurse-slot :name name :selection selection))

(def function make-template/evaluate-form (form &key selection)
  (make-instance 'template/evaluate-form :form form :selection selection))

;;;;;;
;;; Construction

(def macro template/recurse-slot ((&key selection) &body name)
  `(make-template/recurse-slot ,(first name) :selection ,selection))

(def macro template/evaluate-form ((&key selection) &body form)
  `(make-template/evaluate-form ,(first form) :selection ,selection))
