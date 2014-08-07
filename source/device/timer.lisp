;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Timer API
;;;
;;; A timer is an input hardware device.

(def generic timer? (object)
  (:documentation "Returns TRUE if OBJECT is a timer device, otherwise returns FALSE."))

;;;;;;
;;; Timer classes

(def class* device/timer (device/input)
  ()
  (:documentation "A timer device."))

;;;;;;
;;; Timer API implementation

(def method timer? (object)
  (typep object 'device/timer))
