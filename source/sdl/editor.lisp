;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; KLUDGE:
(sdl:init-sdl)

;; KLUDGE:
(sdl-image:init-image :jpg :png :tif)

;; KLUDGE:
(sdl:initialise-default-font (make-instance 'sdl:ttf-font-definition :size 18 :filename (system-relative-pathname :projectured "etc/UbuntuMono-R.ttf")))

;; KLUDGE:
(setf sdl:*default-color* sdl:*black*)

;;;;;;
;;; Editor

(def class* editor/sdl (editor)
  ())

;;;;;;
;;; Construction

(def method make-editor (&key (width 800) (height 600))
  (make-instance 'editor/sdl
                 :devices (list (make-instance 'device/mouse)
                                (make-instance 'device/keyboard)
                                (make-device/display/sdl width height))
                 :event-queue (make-instance 'event-queue :events nil)
                 :gesture-queue (make-instance 'gesture-queue :gestures nil)))

;;;;;;
;;; API

(def method run-read-evaluate-print-loop ((editor editor/sdl) document projection)
  (bind ((display (find-if (of-type 'device/display/sdl) (devices-of editor))))
    (unwind-protect
         (progn
           (sdl:init-video)
           (setf (surface-of display) (sdl:window (width-of display) (height-of display) :double-buffer #t))
           (call-next-method))
      (sdl:quit-video))))

(def method print-to-devices ((editor editor/sdl) document projection)
  (bind ((display (find-if (of-type 'device/display/sdl) (devices-of editor))))
    (sdl:with-surface ((surface-of display))
      (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)
      (call-next-method)
      (sdl:update-display))))

;; KLUDGE: what shall dispatch on here? this is obviously wrong...
(def method read-event ((devices sequence))
  (bind ((sdl:*sdl-event* (sdl:new-event)))
    (sdl:enable-unicode)
    (sdl-cffi::sdl-poll-event sdl:*sdl-event*)
    (prog1
        (case (sdl:event-type sdl:*sdl-event*)
          (:idle
           (values))
          (:quit-event
           (make-instance 'event/window/quit))
          (:key-down-event
           (sdl:with-key-down-event ((mod-key :mod-key) (key :key) (character :unicode)) sdl:*sdl-event*
             (make-instance 'event/keyboard/key-down
                            :timestamp (iolib.syscalls:get-monotonic-time)
                            :modifiers mod-key
                            :key key
                            :character (code-char character))))
          (:key-up-event
           (sdl:with-key-down-event ((mod-key :mod-key) (key :key) (character :unicode)) sdl:*sdl-event*
             (make-instance 'event/keyboard/key-up
                            :timestamp (iolib.syscalls:get-monotonic-time)
                            :modifiers mod-key
                            :key key
                            :character (code-char character))))
          (:mouse-motion-event
           (sdl:with-mouse-motion-event ((x :x) (y :y)) sdl:*sdl-event*
             (make-instance 'event/mouse/move
                            :timestamp (iolib.syscalls:get-monotonic-time)
                            :location (make-2d x y))))
          (:mouse-button-down-event
           (sdl:with-mouse-button-down-event ((x :x) (y :y) (button :button)) sdl:*sdl-event*
             (make-instance 'event/mouse/button/press
                            :timestamp (iolib.syscalls:get-monotonic-time)
                            ;; TODO:
                            :modifiers nil
                            :button (mouse-button button)
                            :location (make-2d x y))))
          (:mouse-button-up-event
           (sdl:with-mouse-button-up-event ((x :x) (y :y) (button :button)) sdl:*sdl-event*
             (make-instance 'event/mouse/button/release
                            :timestamp (iolib.syscalls:get-monotonic-time)
                            ;; TODO:
                            :modifiers nil
                            :button (mouse-button button)
                            :location (make-2d x y))))
          (:video-resize-event
           ;; TODO:
           (values))
          (:video-expose-event
           (sdl:update-display)))
      (sdl:free-event sdl:*sdl-event*))))

(def function mouse-button (button)
  (cond ((= sdl::mouse-left button)
         :button-left)
        ((= sdl::mouse-middle button)
         :button-middle)
        ((= sdl::mouse-right button)
         :button-right)
        ((= sdl::mouse-wheel-down button)
         :wheel-down)
        ((= sdl::mouse-wheel-up button)
         :wheel-up)))
