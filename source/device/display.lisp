;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Display API
;;;
;;; A display is an output hardware device.

(def generic display? (object)
  (:documentation "Returns TRUE if OBJECT is a display device, otherwise returns FALSE."))

;;;;;;
;;; Display classes

(def class* device/display (device/output)
  ((width :type positive-integer)
   (height :type positive-integer))
  (:documentation "A display device."))

;;;;;;
;;; Display API implementation

(def method display? (object)
  (typep object 'device/display))
