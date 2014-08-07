;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; File API
;;;
;;; A file is an output device.

(def generic file? (object)
  (:documentation "Returns TRUE if OBJECT is a file device, otherwise returns FALSE."))

;;;;;;
;;; File classes

(def class* device/file (device/output)
  ((filename :type pathname))
  (:documentation "A file device."))

;;;;;;
;;; File API implementation

(def method file? (object)
  (typep object 'device/file))
