;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Event API
;;;
;;; An event is a low level hardware signal provided by an input hardware device.

(def (generic e) event? (object)
  (:documentation "Returns TRUE if OBJECT is an event, otherwise returns FALSE. Purely functional."))

(def (generic e) read-event (devices)
  (:documentation "Reads a single event from the sequence of DEVICES at once. Does not return until is has successfully read an event. Has side effects on the state of devices."))

;;;;;;
;;; Event classes

(def class* event ()
  ((timestamp :type integer)
   (modifiers :type list))
  (:documentation "Base class for events."))

(def class* event/window (event)
  ()
  (:documentation "Base class for window events."))

(def class* event/window/hide (event/window)
  ())

(def class* event/window/show (event/window)
  ())

(def class* event/window/minimize (event/window)
  ())

(def class* event/window/maximize (event/window)
  ())

(def class* event/window/quit (event/window)
  ())

(def class* event/keyboard (event)
  ((key :type integer)
   (character :type character))
  (:documentation "Base class for keyboard events."))

(def class* event/keyboard/key-down (event/keyboard)
  ()
  (:documentation "A key down event."))

(def class* event/keyboard/key-up (event/keyboard)
  ()
  (:documentation "A key up event."))

(def class* event/mouse (event)
  ()
  (:documentation "Base class for mouse events."))

(def class* event/mouse/button (event/mouse)
  ((location :type 2d)
   (button :type (member :button-left :button-middle :button-right)))
  (:documentation "Base class for mouse button events."))

(def class* event/mouse/button/press (event/mouse/button)
  ()
  (:documentation "A mouse button press event."))

(def class* event/mouse/button/release (event/mouse/button)
  ()
  (:documentation "A mouse button release event."))

(def class* event/mouse/move (event/mouse)
  ((location :type 2d))
  (:documentation "A mouse move event."))

(def class* event-queue ()
  ((events :type sequence))
  (:documentation "A sequence of hardware events."))

;;;;;;
;;; Event API implementation

(def method event? (object)
  (typep object 'event))
