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

(def (generic e) gesture? (object)
  (:documentation "Returns TRUE if OBJECT is a gesture, otherwise returns FALSE. Purely functional."))

(def (generic e) read-gesture (event-queue)
  (:documentation "Reads a single gesture from EVENT-QUEUE. Does not return until it has successfully read a gesture. Purely functional."))

(def (generic e) last-gesture (gesture-queue)
  (:documentation "Returns the last gesture that has been read. Purely functional."))

;;;;;;
;;; Gesture classes

(def class* gesture ()
  ((modifiers :type list))
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
  ((location :type 2d)))

(def class* gesture/mouse/hover (gesture/mouse)
  ((location :type 2d)))

(def class* gesture/mouse/button (gesture/mouse)
  ((button :type (member :button-left :button-middle :button-right))))

(def class* gesture/mouse/button/click (gesture/mouse/button)
  ((location :type 2d)))

(def class* gesture-queue ()
  ((gestures :type sequence)))

;;;;;;
;;; Construction

(def function make-gesture/keyboard/key-press (key-or-character &optional modifier-or-modifiers)
  (make-instance 'gesture/keyboard/key-press
                 :key (when (symbolp key-or-character) key-or-character)
                 :character (when (characterp key-or-character) key-or-character)
                 :modifiers (if (listp modifier-or-modifiers)
                                modifier-or-modifiers
                                (list modifier-or-modifiers))))

;;;;;;
;;; Construction

(def macro gesture/keyboard/key-press (key-or-character &optional modifier-or-modifiers)
  `(make-gesture/keyboard/key-press ,key-or-character ,modifier-or-modifiers))

;;;;;;
;;; Gesture API implementation

(def method read-gesture ((event-queue event-queue))
  (bind ((events (events-of event-queue))
         (length (length events))
         (event0 (when (> length 0) (elt events 0)))
         (event1 (when (> length 1) (elt events 1))))
    (cond ((typep event0 'event/window/quit)
           (make-instance 'gesture/window/quit))
          ((and (typep event0 'event/keyboard/key-up)
                (typep event1 'event/keyboard/key-down)
                (eq (key-of event0) (key-of event1)))
           (make-instance 'gesture/keyboard/key-press
                          :modifiers (modifiers-of event0)
                          :key (key-of event1)
                          :character (character-of event1)))
          ((and (typep event0 'event/mouse/button/release)
                (typep event1 'event/mouse/button/press))
           (make-instance 'gesture/mouse/button/click
                          :modifiers (modifiers-of event0)
                          :button (button-of event0)
                          :location (location-of event0)))
          ((and (typep event0 'event/mouse/move)
                ;; TODO: need a timer that kicks in every now and them to make this work
                (> (- (iolib.syscalls:get-monotonic-time) (timestamp-of event0)) 3))
           (make-instance 'gesture/mouse/hover :location (location-of event0)))
          ((and (typep event0 'event/mouse/move))
           (make-instance 'gesture/mouse/move :location (location-of event0)))
          (t nil))))

(def method last-gesture ((gesture-queue gesture-queue))
  (bind ((gestures (gestures-of gesture-queue)))
    (when (> (length gestures) 0)
      (elt gestures 0))))

(def function key-press? (gesture &key (key nil key?) (character nil character?) (modifier nil modifier?) (modifiers nil modifiers?))
  (and (typep gesture 'gesture/keyboard/key-press)
       (or (not key?) (eq key (key-of gesture)))
       (or (not character?) (char= character (character-of gesture)))
       (or (not modifier?) (equal (list modifier) (modifiers-of gesture)))
       (or (not modifiers?) (equal modifiers (modifiers-of gesture)))))

(def function gesture= (g1 g2)
  (typecase g1
    (gesture/keyboard/key-press
     (and (eq (class-of g1) (class-of g2))
          (or (eq (key-of g1) (key-of g2))
              (equal (character-of g1) (character-of g2)))
          (equal (modifiers-of g1) (modifiers-of g2))))
    (gesture/mouse/button/click
     (and (eq (class-of g1) (class-of g2))
          (eq (button-of g1) (button-of g2))
          (equal (modifiers-of g1) (modifiers-of g2))))))

;; TODO: move and rename
(def class* gesture->operation ()
  ((gesture :type gesture)
   (domain :type string)
   (description :type string)
   (operation :type operation)))

(def macro gesture-case (gesture &body cases)
  (with-unique-names (gesture-variable)
    `(bind ((,gesture-variable ,gesture))
       (or (and (key-press? ,gesture-variable :key :sdl-key-h :modifier :control)
                (make-instance 'operation/show-context-sensitive-help
                               :operations (optional-list ,@(iter (for case :in cases)
                                                                  (for operation = (getf (rest case) :operation))
                                                                  (collect `(bind ((operation ,(getf (rest case) :operation)))
                                                                              (when operation
                                                                                (make-instance 'gesture->operation
                                                                                               :gesture ,(first case)
                                                                                               :domain ,(getf (rest case) :domain)
                                                                                               ;; TODO: make consistent
                                                                                               :description ,(getf (rest case) :help)
                                                                                               :operation ,operation))))))))
           ,@(iter (for case :in cases)
                   (collect `(and (gesture= ,gesture-variable ,(first case))
                                  ,(getf (rest case) :operation))))))))

(def function merge-operations (operation-1 operation-2)
  (cond ((and (typep operation-1 'operation/show-context-sensitive-help)
              (typep operation-2 'operation/show-context-sensitive-help))
         (make-instance 'operation/show-context-sensitive-help
                        :operations (append (operations-of operation-1)
                                            (operations-of operation-2)
                                            #+nil
                                            (set-difference (operations-of operation-2) (operations-of operation-1) :key 'gesture-of :test 'gesture=))))
        (t
         (or operation-1 operation-2))))
