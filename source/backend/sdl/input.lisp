;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.sdl)

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
  (cffi:with-foreign-object (event '#,SDL_Event)
    (setf (cffi:foreign-slot-value event '#,SDL_Event '#,type) #,SDL_FIRSTEVENT)
    (when (zerop (#,SDL_WaitEvent event))
      (error "Error in SDL_WaitEvent: ~S" (#,SDL_GetError)))
    (bind ((event-type (cffi:foreign-slot-value event '#,SDL_Event '#,type)))
      (case event-type
        ((#.#,SDL_KEYDOWN #.#,SDL_KEYUP)
         (bind ((keysym (cffi:foreign-slot-value event '#,SDL_KeyboardEvent '#,keysym))
                (scancode (translate-scancode (cffi:foreign-slot-value keysym '#,SDL_Keysym '#,scancode))))
           (make-instance (if (eql event-type #,SDL_KEYDOWN)
                              'event/keyboard/key-down
                              'event/keyboard/key-up)
                          :timestamp (get-internal-real-time)
                          :modifiers (translate-modifier-keys
                                      (cffi:foreign-slot-value keysym '(:struct #,SDL_Keysym) '#,mod))
                          :mouse-position (current-mouse-position)
                          :key scancode)))
        (#.#,SDL_TEXTINPUT
         (bind ((input (cffi:foreign-string-to-lisp
                        (cffi:foreign-slot-pointer event '#,SDL_TextInputEvent '#,text)
                        :max-chars (cffi:foreign-type-size (cffi:foreign-slot-type '#,SDL_TextInputEvent '#,text)))))
           (check-type input string)
           (make-instance 'event/keyboard/type-in
                          :timestamp (get-internal-real-time)
                          :modifiers nil
                          :mouse-position (current-mouse-position)
                          ;; TODO FIXME this assumption is wrong. see: https://wiki.libsdl.org/Tutorials/TextInput
                          :character (first-elt input))))
        (#.#,SDL_MOUSEMOTION
         (make-instance 'event/mouse/move
                        :timestamp (get-internal-real-time)
                        :modifiers nil
                        :mouse-position (current-mouse-position)))
        ((#.#,SDL_MOUSEBUTTONDOWN #.#,SDL_MOUSEBUTTONUP)
         (bind ((button (cffi:foreign-slot-value event '#,SDL_MouseButtonEvent '#,button)))
           (make-instance (if (= event-type #,SDL_MOUSEBUTTONDOWN)
                              'event/mouse/button-press
                              'event/mouse/button-release)
                          :timestamp (get-internal-real-time)
                          :modifiers nil
                          :button (translate-mouse-button button)
                          :mouse-position (make-2d (cffi:foreign-slot-value event '#,SDL_MouseButtonEvent '#,x)
                                                   (cffi:foreign-slot-value event '#,SDL_MouseButtonEvent '#,y)))))
        (#.#,SDL_MOUSEWHEEL
         (make-instance 'event/mouse/scroll-wheel
                        :timestamp (get-internal-real-time)
                        :modifiers nil
                        :scroll (make-2d (cffi:foreign-slot-value event '#,SDL_MouseWheelEvent '#,x)
                                         (cffi:foreign-slot-value event '#,SDL_MouseWheelEvent '#,y))
                        :mouse-position (current-mouse-position)))
        (#.#,SDL_QUIT
         (make-instance 'event/window/quit
                        :mouse-position (current-mouse-position)))))))

(def function translate-modifier-keys (keys)
  (labels
      ((modifier-key (key)
         (ecase key
           ((#,KMOD_LCTRL #,KMOD_RCTRL) :control)
           ((#,KMOD_LSHIFT #,KMOD_RSHIFT) :shift)
           ((#,KMOD_LALT #,KMOD_RALT) :alt)
           ((#,KMOD_LGUI #,KMOD_NUM #,KMOD_MODE) nil))))
    (remove nil (mapcar #'modifier-key (cffi:foreign-bitfield-symbols '#,SDL_Keymod keys)))))

;; turn an SDL_SCANCODE_A into a :SCANCODE_A.
(def function translate-scancode (scancode)
  (check-type scancode (and symbol (not null)))
  (bind ((symbol-name (symbol-name scancode)))
    (assert (starts-with-subseq "SDL_" symbol-name))
    (make-keyword (substitute #\- #\_ (subseq symbol-name 4)))))

(def function translate-mouse-button (button)
  (iter (for mask :initially 1 :then (ash mask 1))
        (for name :in '(:button-left :button-middle :button-right :wheel-down :wheel-up))
        (unless (zerop (logand mask button))
          (return name))))

(def function current-mouse-position ()
  (cffi:with-foreign-objects ((x :int) (y :int))
    (#,SDL_GetMouseState x y)
    (make-2d (cffi:mem-ref x :int) (cffi:mem-ref y :int))))
