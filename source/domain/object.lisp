;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Object domain provides:
;;;;  - standard-object

;;;;;;
;;; Object document classes
;;;
;;; The object document classes are provided by the Common Lisp implementation or an external library.

;;;;;;
;;; Object document constructors
;;;
;;; The object document constructores are provided by the Common Lisp implementation or an external library.

;;;;;;
;;; Object operation classes

(def operation operation/object/replace-place-value ()
  ((selection :type reference)
   (replacement :type t)))

;;;;;;
;;; Object operation constructors

(def function make-operation/object/replace-place-value (selection replacement)
  (make-instance 'operation/object/replace-place-value :selection selection :replacement replacement))

;;;;;;
;;; Object operation API implementation

(def method run-operation ((operation operation/object/replace-place-value))
  (setf (eval-reference document (selection-of operation)) (replacement-of operation)))
