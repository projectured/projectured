;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def function input-from-devices (editor)
  (bind ((event-queue (event-queue-of editor))
         (gesture-queue (gesture-queue-of editor))
         (input-devices (remove-if-not (of-type 'device/input) (devices-of editor))))
    (if input-devices
        (iter (when-bind event (read-event)
                (push event (events-of event-queue))
                (when-bind gesture (read-gesture event-queue)
                  (push gesture (gestures-of gesture-queue))
                  (when-bind operation (operation-of (apply-reader (make-nothing-command gesture) (projection-of editor) (printer-iomap-of editor)))
                    (return operation)))))
        (make-operation/quit))))

(def function read-event ()
  (plus-c:c-with ((event sdl2-ffi:sdl-event))
    (setf (event :type) (autowrap:enum-value 'sdl2-ffi:sdl-event-type :firstevent))
    (sdl2-ffi.functions:sdl-wait-event event)
    (case (autowrap:enum-key 'sdl2-ffi:sdl-event-type (event :type))
      (:idle
       (values))
      (:keydown
       (bind ((keysym (plus-c:c-ref event sdl2-ffi:sdl-keyboard-event :keysym))
              (scancode (autowrap:enum-key 'sdl2-ffi:sdl-scancode (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :scancode))))
         (make-instance 'event/keyboard/key-down
                        :timestamp (get-internal-real-time)
                        :modifiers (modifier-keys (autowrap:mask-keywords 'sdl2::keymod (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :mod)))
                        :mouse-position (mouse-position)
                        :key scancode)))
      (:keyup
       (bind ((keysym (plus-c:c-ref event sdl2-ffi:sdl-keyboard-event :keysym))
              (scancode (autowrap:enum-key 'sdl2-ffi:sdl-scancode (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :scancode))))
         (make-instance 'event/keyboard/key-up
                        :timestamp (get-internal-real-time)
                        :modifiers (modifier-keys (autowrap:mask-keywords 'sdl2::keymod (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :mod)))
                        :mouse-position (mouse-position)
                        :key scancode)))
      (:textinput
       (make-instance 'event/keyboard/type-in
                      :timestamp (get-internal-real-time)
                      :modifiers nil
                      :mouse-position (mouse-position)
                      :character (first-elt (cffi:foreign-string-to-lisp (cffi-sys:inc-pointer (autowrap:ptr event) 12)))))
      (:mousemotion
       (make-instance 'event/mouse/move
                      :timestamp (get-internal-real-time)
                      :modifiers nil
                      :mouse-position (mouse-position)))
      (:mousebuttondown
       (bind ((button (plus-c:c-ref event sdl2-ffi:sdl-mouse-button-event :button)))
         (make-instance 'event/mouse/button-press
                        :timestamp (get-internal-real-time)
                        :modifiers nil
                        :button (mouse-button button)
                        :mouse-position (mouse-position))))
      (:mousebuttonup
       (bind ((button (plus-c:c-ref event sdl2-ffi:sdl-mouse-button-event :button)))
         (make-instance 'event/mouse/button-release
                        :timestamp (get-internal-real-time)
                        :modifiers nil
                        :button (mouse-button button)
                        :mouse-position (mouse-position))))
      (:mousewheel
       (make-instance 'event/mouse/scroll-wheel
                      :timestamp (get-internal-real-time)
                      :modifiers nil
                      :scroll (make-2d (plus-c:c-ref event sdl2-ffi:sdl-mouse-wheel-event :x)
                                       (plus-c:c-ref event sdl2-ffi:sdl-mouse-wheel-event :y))
                      :mouse-position (mouse-position)))
      (:quit
       (make-instance 'event/window/quit
                      :mouse-position (mouse-position))))))

(def function modifier-keys (keys)
  (remove nil (mapcar 'modifier-key keys)))

(def function modifier-key (key)
  (ecase key
    ((:lctrl :rctrl) :control)
    ((:lshift :rshift) :shift)
    ((:lalt :ralt) :alt)
    ((:lgui :num :mode) nil)))

(def function mouse-position ()
  (plus-c:c-with ((x :int) (y :int))
    (sdl2-ffi.functions:sdl-get-mouse-state (x plus-c:&) (y plus-c:&))
    (make-2d x y)))

(def function mouse-button (button)
  (iter (for mask :initially 1 :then (ash mask 1))
        (for name :in '(:button-left :button-middle :button-right :wheel-down :wheel-up))
        (unless (zerop (logand mask button))
          (return name))))
