;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Gesture API
;;;
;;; A gesture is a pattern of low level events satisfying certain constraints.

(def generic read-gesture (event-queue)
  (:documentation "Reads a single gesture from EVENT-QUEUE. Does not return until it has successfully read a gesture. Purely functional."))

;;;;;;
;;; Gesture classes

(def class* gesture ()
  ((modifiers :type list)
   (location :type 2d))
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
  ((key :type integer)
   (character :type character)))

(def class* gesture/mouse (gesture)
  ()
  (:documentation "Base class for mouse gestures."))

(def class* gesture/mouse/move (gesture/mouse)
  ())

(def class* gesture/mouse/hover (gesture/mouse)
  ())

(def class* gesture/mouse/button (gesture/mouse)
  ((button :type (member :button-left :button-middle :button-right))))

(def class* gesture/mouse/button/click (gesture/mouse/button)
  ())

(def class* gesture-queue ()
  ((gestures :type sequence)))

;;;;;;
;;; Construction

(def function make-gesture/keyboard/key-press (&key (key nil key?) (character nil character?) (modifiers nil modifiers?))
  (prog1-bind gesture (make-instance 'gesture/keyboard/key-press)
    (when key?
      (setf (key-of gesture) key))
    (when character?
      (setf (character-of gesture) character))
    (when modifiers?
      (setf (modifiers-of gesture) (ensure-list modifiers)))))

;;;;;;
;;; Construction

(def macro gesture/keyboard/key-press (&rest args &key key character modifiers)
  (declare (ignore key character modifiers))
  `(make-gesture/keyboard/key-press ,@args))

;;;;;;
;;; Gesture API implementation

(def method print-object ((instance gesture/keyboard) stream)
  (print-unreadable-object (instance stream :type #t :identity #t)
    (princ (gesture/describe instance) stream)))

(def method read-gesture ((event-queue event-queue))
  (bind ((events (events-of event-queue))
         (length (length events))
         (event0 (when (> length 0) (elt events 0))))
    (cond ((typep event0 'event/window/quit)
           (make-instance 'gesture/window/quit
                          :modifiers nil
                          :location (location-of event0)))
          ((typep event0 'event/keyboard/key-up)
           (make-instance 'gesture/keyboard/key-press
                          :modifiers (modifiers-of event0)
                          :location (location-of event0)
                          :key (key-of event0)
                          :character (or (iter (for event :in-sequence events)
                                               (when (and (typep event 'event/keyboard/key-down)
                                                          (eq (key-of event0) (key-of event)))
                                                 (return (fix-character (character-of event) (modifiers-of event0)))))
                                         #\nul)))
          ((typep event0 'event/mouse/button/release)
           (make-instance 'gesture/mouse/button/click
                          :modifiers (modifiers-of event0)
                          :location (location-of event0)
                          :button (button-of event0)))
          ((and (typep event0 'event/mouse/move)
                ;; TODO: need a timer that kicks in every now and them to make this work
                (> (- (get-internal-real-time) (timestamp-of event0)) 3))
           (make-instance 'gesture/mouse/hover
                          :location (location-of event0)
                          :modifiers (modifiers-of event0)))
          ((and (typep event0 'event/mouse/move))
           (make-instance 'gesture/mouse/move
                          :location (location-of event0)
                          :modifiers (modifiers-of event0)))
          (t nil))))

;; KLUDGE: workaround SDL unicode handling
(def function fix-character (character modifiers)
  (if (and character (member :shift modifiers))
      (if (alpha-char-p character)
          (char-upcase character)
          (or (cdr (assoc character '((#\' . #\"))))
              character))
      character))

(def function key-press? (gesture &key (key nil key?) (character nil character?) (modifier nil modifier?) (modifiers nil modifiers?))
  (and (typep gesture 'gesture/keyboard/key-press)
       (or (not key?) (eq key (key-of gesture)))
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
    (gesture/keyboard/key-press
     (and (eq (class-of g1) (class-of g2))
          (gesture-slot= g1 g2 'key 'eq)
          (gesture-slot= g1 g2 'character 'char=)
          (gesture-slot= g1 g2 'modifiers 'equal)))
    (gesture/mouse/button/click
     (and (eq (class-of g1) (class-of g2))
          (eq (button-of g1) (button-of g2))
          (equal (modifiers-of g1) (modifiers-of g2))))
    (gesture
     (and (eq (class-of g1) (class-of g2))
          (equal (modifiers-of g1) (modifiers-of g2))))))

(def function gesture/describe-modifiers (gesture)
  (when (and (slot-boundp gesture 'modifiers)
             (modifiers-of gesture))
    (with-output-to-string (string)
      (iter (for modifier :in (modifiers-of gesture))
            (write-string (symbol-name modifier) string)
            (write-string  " + " string)))))

(def function gesture/describe-key (gesture)
  (etypecase gesture
    (gesture/keyboard/key-press
     (when (slot-boundp gesture 'character)
       (bind ((character (character-of gesture)))
         (if (and character
                  (not (whitespace? character))
                  (or (alphanumericp character)
                      (graphic-char-p character)))
             (string (character-of gesture))
             (subseq (symbol-name (key-of gesture)) (length "SDL-KEY-"))))))
    (gesture/mouse/button/click
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
    (gesture/mouse/move
     "MOUSE MOVE")
    (gesture/mouse/hover
     "MOUSE HOVER")
    (gesture/window/quit
     "CLOSE WINDOW")))

(def function gesture/describe (gesture)
  (string+ (gesture/describe-modifiers gesture) (gesture/describe-key gesture)))
