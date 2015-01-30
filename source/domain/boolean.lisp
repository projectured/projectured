;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Boolean domain provides:
;;;;  - boolean classes provided by the Common Lisp implementation

;;;;;;
;;; Boolean document classes
;;;
;;; The boolean document classes are provided by the Common Lisp implementation.

;;;;;;
;;; Boolean document constructors
;;;
;;; The boolean document constructores are provided by the Common Lisp implementation.

;;;;;;
;;; Boolean operation classes

(def operation operation/boolean/negate ()
  ((selection :type selection)))

;;;;;;
;;; Boolean operation API implementation

(def method run-operation ((operation operation/boolean/negate))
  (not-yet-implemented))
