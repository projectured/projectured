;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

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
