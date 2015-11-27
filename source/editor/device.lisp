;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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

(def class* device/keyboard (device/input)
  ()
  (:documentation "A keyboard is an input device."))

(def class* device/mouse (device/input)
  ()
  (:documentation "A mouse is an input device."))

(def class* device/timer (device/input)
  ()
  (:documentation "A timer is an input device."))

(def class* device/display (device/output)
  ((width :type positive-integer)
   (height :type positive-integer)
   (raw :type t))
  (:documentation "A display is an output device."))

(def class* device/file (device/output)
  ((filename :type pathname)
   (raw :type t))
  (:documentation "A file is an output device."))

;;;;;;
;;; Construction

(def function make-mouse-device ()
  (make-instance 'device/mouse))

(def function make-keyboard-device ()
  (make-instance 'device/keyboard))

(def function make-timer-device ()
  (make-instance 'device/timer))

(def function make-display-device (width height)
  (make-instance 'device/display :width width :height height :raw nil))

(def function make-file-device (filename)
  (make-instance 'device/file :filename filename :raw nil))

(def function make-default-devices (&key width height (filename nil))
  (append (list (make-mouse-device)
                (make-keyboard-device)
                (make-display-device width height))
          (when filename
            (list (make-file-device filename)))))
