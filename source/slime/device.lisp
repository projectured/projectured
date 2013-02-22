;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Device

(def class* device/display/slime (device/display)
  ())

;;;;;;
;;; Construction

(def (function e) make-device/display/slime ()
  (make-instance 'device/display/slime))
