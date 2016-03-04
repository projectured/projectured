;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

(def class* gesture ()
  ((modifiers :type list)
   (mouse-position :type 2d))
  (:documentation "Base class for gestures."))

(def class* gesture/window (gesture)
  ()
  (:documentation "Base class for window gestures."))

(def class* gesture/window/quit (gesture/window)
  ())

(def class* gesture/keyboard (gesture)
  ()
  (:documentation "Base class for keyboard gestures."))

(def class* gesture/keyboard/key-press (gesture/keyboard)
  ((key :type symbol)))

(def class* gesture/keyboard/type-in (gesture/keyboard)
  ((character :type character)))

(def class* gesture/mouse (gesture)
  ()
  (:documentation "Base class for mouse gestures."))

(def class* gesture/mouse/move (gesture/mouse)
  ())

(def class* gesture/mouse/hover (gesture/mouse)
  ())

(def class* gesture/mouse/click (gesture/mouse)
  ((button :type (member :button-left :button-middle :button-right))))

(def class* gesture/mouse/scroll (gesture/mouse)
  ((scroll :type 2d)))

(def class* gesture-queue ()
  ((gestures :type sequence)))

;;;;;;
;;; Construction

(def function make-gesture-queue ()
  (make-instance 'gesture-queue :gestures nil))

(def function make-key-press-gesture (key &optional modifiers)
  (make-instance 'gesture/keyboard/key-press :key key :modifiers (ensure-list modifiers)))

(def function make-type-in-gesture (character)
  (make-instance 'gesture/keyboard/type-in :character character))

;;;;;;
;;; API

(def method print-object ((instance gesture/keyboard) stream)
  (print-unreadable-object (instance stream :type #t :identity #t)
    (princ (describe-gesture instance) stream)))

(def function read-gesture (event-queue)
  (bind ((events (events-of event-queue))
         (length (length events))
         (event0 (when (> length 0) (elt events 0))))
    (cond ((typep event0 'event/window/quit)
           (make-instance 'gesture/window/quit
                          :modifiers nil
                          :mouse-position (mouse-position-of event0)))
          ((typep event0 'event/keyboard/key-up)
           (make-instance 'gesture/keyboard/key-press
                          :modifiers (modifiers-of event0)
                          :mouse-position (mouse-position-of event0)
                          :key (key-of event0)))
          ((and (typep event0 'event/keyboard/type-in)
                (not (intersection '(:control :alt) (modifiers-of event0))))
           (make-instance 'gesture/keyboard/type-in
                          :modifiers (modifiers-of event0)
                          :mouse-position (mouse-position-of event0)
                          :character (character-of event0)))
          ((typep event0 'event/mouse/button-release)
           (make-instance 'gesture/mouse/click
                          :modifiers (modifiers-of event0)
                          :mouse-position (mouse-position-of event0)
                          :button (button-of event0)))
          ((typep event0 'event/mouse/scroll-wheel)
           (make-instance 'gesture/mouse/scroll
                          :modifiers (modifiers-of event0)
                          :mouse-position (mouse-position-of event0)
                          :scroll (scroll-of event0)))
          ((and (typep event0 'event/mouse/move)
                ;; TODO: need a timer that kicks in every now and them to make this work
                (> (- (get-internal-real-time) (timestamp-of event0)) 3))
           (make-instance 'gesture/mouse/hover
                          :mouse-position (mouse-position-of event0)
                          :modifiers (modifiers-of event0)))
          ((and (typep event0 'event/mouse/move))
           (make-instance 'gesture/mouse/move
                          :mouse-position (mouse-position-of event0)
                          :modifiers (modifiers-of event0)))
          (t nil))))

(def function key-press? (gesture &key (key nil key?) (modifier nil modifier?) (modifiers nil modifiers?))
  (and (typep gesture 'gesture/keyboard/key-press)
       (or (not key?) (eq key (key-of gesture)))
       (or (not modifier?) (equal (list modifier) (modifiers-of gesture)))
       (or (not modifiers?) (equal modifiers (modifiers-of gesture)))))

(def function type-in? (gesture &key (character nil character?) (modifier nil modifier?) (modifiers nil modifiers?))
  (and (typep gesture 'gesture/keyboard/key-press)
       (or (not character?) (char= character (character-of gesture)))
       (or (not modifier?) (equal (list modifier) (modifiers-of gesture)))
       (or (not modifiers?) (equal modifiers (modifiers-of gesture)))))

(def function gesture-slot= (g1 g2 slot test)
  (or (and (not (slot-boundp g1 slot))
           (not (slot-boundp g2 slot)))
      (if (and (slot-boundp g1 slot) (slot-boundp g2 slot))
          (funcall test (slot-value g1 slot) (slot-value g2 slot))
          #t)))

(def function gesture= (g1 g2)
  (typecase g1
    (gesture/keyboard/type-in
     (and (eq (class-of g1) (class-of g2))
          (gesture-slot= g1 g2 'character 'char=)))
    (gesture/keyboard/key-press
     (and (eq (class-of g1) (class-of g2))
          (gesture-slot= g1 g2 'key 'eq)
          (gesture-slot= g1 g2 'modifiers 'equal)))
    (gesture/mouse/click
     (and (eq (class-of g1) (class-of g2))
          (eq (button-of g1) (button-of g2))
          (equal (modifiers-of g1) (modifiers-of g2))))
    (gesture
     (and (eq (class-of g1) (class-of g2))
          (equal (modifiers-of g1) (modifiers-of g2))))))

(def function describe-gesture-modifiers (gesture)
  (when (and (slot-boundp gesture 'modifiers)
             (modifiers-of gesture))
    (with-output-to-string (string)
      (iter (for modifier :in (modifiers-of gesture))
            (write-string (symbol-name modifier) string)
            (write-string  " + " string)))))

(def function describe-gesture-keys (gesture)
  (etypecase gesture
    (gesture/keyboard/key-press
     (subseq (symbol-name (key-of gesture)) (length "SCANCODE-")))
    (gesture/keyboard/type-in
     (string (character-of gesture)))
    (gesture/mouse/click
     (ecase (button-of gesture)
       (:button-left
        "LEFT MOUSE BUTTON")
       (:button-middle
        "MIDDLE MOUSE BUTTON")
       (:button-right
        "RIGHT MOUSE BUTTON")
       (:wheel-down
        "MOUSE WHEEL DOWN")
       (:wheel-up
        "MOUSE WHEEL UP")))
    (gesture/mouse/scroll
     "MOUSE SCROLL")
    (gesture/mouse/move
     "MOUSE MOVE")
    (gesture/mouse/hover
     "MOUSE HOVER")
    (gesture/window/quit
     "CLOSE WINDOW")))

(def function describe-gesture (gesture)
  (string+ (describe-gesture-modifiers gesture) (describe-gesture-keys gesture)))
