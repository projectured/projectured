;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Editor

(def class* editor/web (editor)
  ())

;;;;;;
;;; Construction

(def method make-editor (&key (width 1024) (height 768))
  (make-instance 'editor/web
                 :devices (list (make-instance 'device/mouse)
                                (make-instance 'device/keyboard)
                                (make-device/display/web width height))
                 :event-queue (make-instance 'event-queue :events nil)
                 :gesture-queue (make-instance 'gesture-queue :gestures nil)))

;;;;;;
;;; API

(def special-variable *kludge* nil)

(def method read-event ((devices sequence))
  ;; TODO: KLUDGE: multiple events
  (notf *kludge*)
  (if *kludge*
      (make-instance 'event/keyboard/key-down
                     :timestamp (get-internal-real-time)
                     :modifiers nil
                     :key :sdl-key-left
                     ;; TODO:
                     :character #\Space)
      (make-instance 'event/keyboard/key-up
                     :timestamp (get-internal-real-time)
                     :modifiers nil
                     :key :sdl-key-left
                     ;; TODO:
                     :character #\Space)))
