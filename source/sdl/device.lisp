;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Device

(def class* device/display/sdl (device/display)
  ((surface nil :type (or null sdl:surface))))

;;;;;;
;;; Construction

(def (function e) make-device/display/sdl (width height)
  (make-instance 'device/display/sdl :width width :height height))
