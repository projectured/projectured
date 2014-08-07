;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

(def class* device/display (device/output)
  ((width :type positive-integer)
   (height :type positive-integer))
  (:documentation "A display is an output device."))
