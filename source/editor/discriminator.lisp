;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Discriminator API

(def (generic e) discriminator? (object)
  (:documentation "Returns TRUE if OBJECT is a discriminator, otherwise returns FALSE. Purely functional."))

(def type discriminator ()
  '(or number symbol string cons))

;;;;;;
;;; Discriminator constructors

(def function make-discriminator (target &optional discriminator)
  (if discriminator
      `(discriminator ,target ,discriminator)
      target))

;;;;;;
;;; Discriminator API implementation

(def method discriminator? (object)
  (typep object 'discriminator))
