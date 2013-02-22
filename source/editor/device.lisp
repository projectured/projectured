;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Device API
;;;
;;; A device is a hardware element.

(def (generic e) device? (object)
  (:documentation "Returns TRUE if OBJECT is a device, otherwise returns FALSE. Purely functional."))

(def (generic e) input-device? (device)
  (:documentation "Returns TRUE if OBJECT is an input device, otherwise returns FALSE. Purely functional."))

(def (generic e) output-device? (device)
  (:documentation "Returns TRUE if OBJECT is an output device, otherwise returns FALSE. Purely functional."))

;;;;;;
;;; Device classes

(def class* device ()
  ()
  (:documentation "Base class for devices."))

(def class* device/input (device)
  ()
  (:documentation "Base class for input devices."))

(def class* device/output (device)
  ()
  (:documentation "Base class for output devices."))

(def class* device/input-output (device/input device/output)
  ()
  (:documentation "Base class for input-output devices."))

;;;;;;
;;; Device API implementation

(def method device? (object)
  (typep object 'device))

(def method input-device? (object)
  (typep object 'device/input))

(def method output-device? (object)
  (typep object 'device/output))
