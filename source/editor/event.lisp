;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

(def class* event ()
  ((timestamp :type integer)
   (modifiers :type list)
   (mouse-position :type 2d))
  (:documentation "Base class for events."))

(def class* event/window (event)
  ()
  (:documentation "Base class for window events."))

(def class* event/window/quit (event/window)
  ())

(def class* event/keyboard (event)
  ()
  (:documentation "Base class for keyboard events."))

(def class* event/keyboard/key-down (event/keyboard)
  ((key :type symbol))
  (:documentation "A key down event."))

(def class* event/keyboard/key-up (event/keyboard)
  ((key :type symbol))
  (:documentation "A key up event."))

(def class* event/keyboard/type-in (event/keyboard)
  ((character :type character))
  (:documentation "A type-in event."))

(def class* event/mouse (event)
  ()
  (:documentation "Base class for mouse events."))

(def class* event/mouse/button-press (event/mouse)
  ((button :type (member :button-left :button-middle :button-right)))
  (:documentation "A mouse button press event."))

(def class* event/mouse/button-release (event/mouse)
  ((button :type (member :button-left :button-middle :button-right)))
  (:documentation "A mouse button release event."))

(def class* event/mouse/scroll-wheel (event/mouse)
  ((scroll :type 2d))
  (:documentation "A mouse wheel event."))

(def class* event/mouse/move (event/mouse)
  ()
  (:documentation "A mouse move event."))

(def class* event-queue ()
  ((events :type sequence))
  (:documentation "A sequence of hardware events."))

;;;;;;
;;; Construction

(def function make-event-queue ()
  (make-instance 'event-queue :events nil))

;;;;;;
;;; API

(def method print-object ((instance event/keyboard/key-down) stream)
  (print-unreadable-object (instance stream :type #t :identity #f)
    (princ (key-of instance) stream)))

(def method print-object ((instance event/keyboard/key-up) stream)
  (print-unreadable-object (instance stream :type #t :identity #f)
    (princ (key-of instance) stream)))

(def method print-object ((instance event/keyboard/type-in) stream)
  (print-unreadable-object (instance stream :type #t :identity #f)
    (princ (character-of instance) stream)))
