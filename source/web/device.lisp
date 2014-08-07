;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Device

(def class* device/display/web (device/display)
  ())

;;;;;;
;;; Construction

(def function make-device/display/web (width height)
  (make-instance 'device/display/web :width width :height height))
