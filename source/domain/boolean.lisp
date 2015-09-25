;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/boolean ()
  ((value :type (or null boolean)))
  (:documentation "This class represents a domain independent plain old boolean which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-document/boolean (value &key selection)
  (make-instance 'document/boolean :value value :selection selection))

;;;;;;
;;; Construction

(def macro document/boolean ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-document/boolean ,(first value) :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/boolean/negate ()
  ((selection :type selection)))

;;;;;;
;;; API

(def method run-operation ((operation operation/boolean/negate))
  (not-yet-implemented))
