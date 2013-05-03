;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Device

(def class* device/sdl ()
  ((surface nil :type (or null sdl:surface))))

(def class* device/display/sdl (device/display device/sdl)
  ())

(def class* device/file/sdl (device/file device/sdl)
  ())

;;;;;;
;;; Construction

(def (function e) make-device/display/sdl (width height)
  (make-instance 'device/display/sdl :width width :height height))

(def (function e) make-device/file/sdl (filename)
  (make-instance 'device/file/sdl :filename filename))
