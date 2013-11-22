;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Mouse API
;;;
;;; A mouse is an input hardware device.

(def (generic e) mouse? (object)
  (:documentation "Returns TRUE if OBJECT is a mouse device, otherwise returns FALSE."))

(def (generic e) mouse-position ()
  (:documentation "Returns the current mouse position."))

;;;;;;
;;; Mouse classes

(def class* device/mouse (device/input)
  ()
  (:documentation "A mouse device."))

;;;;;;
;;; Mouse API implementation

(def method mouse? (object)
  (typep object 'device/mouse))
