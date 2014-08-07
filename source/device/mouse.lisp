;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def generic mouse-position ()
  (:documentation "Returns the current mouse position."))

;;;;;;
;;; Class

(def class* device/mouse (device/input)
  ()
  (:documentation "A mouse is an input device."))
