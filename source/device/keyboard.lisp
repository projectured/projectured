;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Keyboard API
;;;
;;; A keyboard is an input hardware device.

(def generic keyboard? (object)
  (:documentation "Returns TRUE if OBJECT is a keyboard device, otherwise returns FALSE."))

;;;;;;
;;; Keyboard classes

(def class* device/keyboard (device/input)
  ()
  (:documentation "A keyboard device."))

;;;;;;
;;; Keyboard API implementation

(def method keyboard? (object)
  (typep object 'device/keyboard))
