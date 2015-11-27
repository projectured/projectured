;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; 2d

(def type 2d ()
  'complex)

(def function make-2d (x y)
  (complex x y))

(def function 2d-x (coordinate)
  (realpart coordinate))

(def function 2d-y (coordinate)
  (imagpart coordinate))

;;;;;;
;;; Rectangle

(def class* rectangle ()
  ((position :type 2d)
   (size :type 2d)))

(def function make-rectangle (position size)
  (make-instance 'rectangle :position position :size size))

(def function rectangle-contains-point? (rectangle point)
  (bind ((position (position-of rectangle))
         (position-x (2d-x position))
         (position-y (2d-y position))
         (size (size-of rectangle))
         (size-x (2d-x size))
         (size-y (2d-y size)))
    (and (<= position-x (2d-x point) (+ position-x size-x))
         (<= position-y (2d-y point) (+ position-y size-y)))))

(def function rectangle-union (rectangle-1 rectangle-2)
  (bind ((position-1 (position-of rectangle-1))
         (position-2 (position-of rectangle-2))
         (size-1 (size-of rectangle-1))
         (size-2 (size-of rectangle-2))
         (position (make-2d (min (2d-x position-1) (2d-x position-2))
                            (min (2d-y position-1) (2d-y position-2)))))
    (make-rectangle position
                    (- (make-2d (max (+ (2d-x position-1) (2d-x size-1)) (+ (2d-x position-2) (2d-x size-2)))
                                (max (+ (2d-y position-1) (2d-y size-1)) (+ (2d-y position-2) (2d-y size-2))))
                       position))))

;;;;;;
;;; Document

(def document graphics/base ()
  ((bounds (as (make-bounding-rectangle -self-)) :type rectangle))
  (:documentation "Base class for graphics elements."))

(def document graphics/point (graphics/base)
  ((position :type 2d)
   (stroke-color :type style/color)))

(def document graphics/line (graphics/base)
  ((begin :type 2d)
   (end :type 2d)
   (stroke-color :type style/color)))

(def document graphics/rectangle (graphics/base)
  ((position :type 2d)
   (size :type 2d)
   (stroke-color :type style/color)
   (fill-color :type style/color)))

(def document graphics/rounded-rectangle (graphics/rectangle)
  ((radius :type number)
   (corners :type list)))

(def document graphics/polygon (graphics/base)
  ((points :type sequence)
   (stroke-color :type style/color)
   (fill-color :type style/color)))

(def document graphics/circle (graphics/base)
  ((position :type 2d)
   (radius :type number)
   (stroke-color :type style/color)
   (fill-color :type style/color)))

(def document graphics/ellipse (graphics/base)
  ((position :type 2d)
   (radius :type 2d)
   (stroke-color :type style/color)
   (fill-color :type style/color)))

(def document graphics/text (graphics/base)
  ((position :type 2d)
   (text :type string)
   (font :type t)
   (font-color :type style/color)
   (fill-color :type style/color)))

(def document graphics/image (graphics/base)
  ((position :type 2d)
   (image :type image/file)))

(def document graphics/viewport (graphics/base)
  ((position :type 2d)
   (size :type 2d)
   (content :type t)))

(def document graphics/canvas (graphics/base)
  ((position :type 2d)
   (orientation :type (member nil :horizontal :vertical))
   (elements :type sequence)))

(def document graphics/window (graphics/base)
  ((position :type 2d)
   (size :type 2d)
   (content :type t)
   (title :type string)))

;;;;;;
;;; Construction

(def function make-graphics/point (position &key stroke-color)
  (make-instance 'graphics/point :position position :stroke-color stroke-color))

(def function make-graphics/line (begin end &key stroke-color)
  (make-instance 'graphics/line :begin begin :end end :stroke-color stroke-color))

(def function make-graphics/rectangle (position size &key stroke-color fill-color)
  (make-instance 'graphics/rectangle :position position :size size :stroke-color stroke-color :fill-color fill-color))

(def function make-graphics/rounded-rectangle (position size radius &key corners stroke-color fill-color)
  (make-instance 'graphics/rounded-rectangle :position position :size size :radius radius :corners corners :stroke-color stroke-color :fill-color fill-color))

(def function make-graphics/polygon (points &key stroke-color fill-color)
  (make-instance 'graphics/polygon :points points :stroke-color stroke-color :fill-color fill-color))

(def function make-graphics/circle (position radius &key stroke-color fill-color)
  (make-instance 'graphics/circle :position position :radius radius :stroke-color stroke-color :fill-color fill-color))

(def function make-graphics/ellipse (position radius &key stroke-color fill-color)
  (make-instance 'graphics/ellipse :position position :radius radius :stroke-color stroke-color :fill-color fill-color))

(def function make-graphics/text (position text &key selection font font-color fill-color)
  (make-instance 'graphics/text :position position :text text :selection selection :font font :font-color font-color :fill-color fill-color))

(def function make-graphics/image (position image)
  (make-instance 'graphics/image :position position :image image))

(def function make-graphics/viewport (content position size &key selection)
  (make-instance 'graphics/viewport :position position :content content :size size :selection selection))

(def function make-graphics/canvas (elements position &key selection)
  (make-instance 'graphics/canvas :position position :elements elements :selection selection))

(def function make-graphics/window (position size content &key title selection)
  (make-instance 'graphics/window :position position :size size :content content :title title :selection selection))

;;;;;;
;;; API

(def function make-bounding-rectangle (instance)
  (typecase instance
    (graphics/text
     (make-rectangle (position-of instance) (measure-text (text-of instance) (font-of instance))))

    (graphics/image
     (make-rectangle (position-of instance) (measure-image (image-of instance))))

    (graphics/canvas
     (iter (with canvas-rectangle = nil)
           (for element :in-sequence (elements-of instance))
           (for element-rectangle = (bounds-of element))
           (setf canvas-rectangle (if canvas-rectangle
                                      (rectangle-union canvas-rectangle element-rectangle)
                                      element-rectangle))
           (finally
            (unless canvas-rectangle
              (setf canvas-rectangle (make-rectangle 0 0)))
            (incf (position-of canvas-rectangle) (position-of instance))
            (return canvas-rectangle))))

    (graphics/viewport
     (make-rectangle (position-of instance) (size-of instance)))

    (graphics/rectangle
     (make-rectangle (position-of instance) (size-of instance)))

    (graphics/polygon
     (iter (with points = (points-of instance))
           (for point :in-sequence points)
           (minimizing (2d-x point) :into min-x)
           (minimizing (2d-y point) :into min-y)
           (maximizing (2d-x point) :into max-x)
           (maximizing (2d-y point) :into max-y)
           (finally
            (return (bind ((top-left (make-2d min-x min-y))
                           (bottom-right (make-2d max-x max-y)))
                      (make-rectangle top-left (- bottom-right top-left)))))))

    (graphics/line
     (bind ((begin (begin-of instance))
            (begin-x (2d-x begin))
            (begin-y (2d-y begin))
            (end (end-of instance))
            (end-x (2d-x end))
            (end-y (2d-y end))
            (top-left (make-2d (min begin-x end-x)
                               (min begin-y end-y)))
            (bottom-right (make-2d (max begin-x end-x)
                                   (max begin-y end-y))))
       (make-rectangle top-left (+ (- bottom-right top-left) (make-2d 1 1)))))

    (graphics/circle
     (bind ((radius (radius-of instance))
            (size (* 2 radius)))
       (make-rectangle (- (position-of instance) (make-2d radius radius)) (make-2d size size))))

    (graphics/ellipse
     (make-rectangle (- (position-of instance) (radius-of instance)) (* 2 (radius-of instance))))

    (graphics/point
     (make-rectangle (position-of instance) (make-2d 1 1)))

    (graphics/window
     (make-rectangle (content-of instance) (size-of instance)))))

(def function make-reference (instance position reference)
  (typecase instance
    (graphics/point
     nil)
    (graphics/line
     nil)
    (graphics/rectangle
     (when (rectangle-contains-point? instance position)
       reference))
    (graphics/polygon
     (not-yet-implemented))
    (graphics/circle
     (not-yet-implemented))
    (graphics/ellipse
     (not-yet-implemented))
    (graphics/text
     ;; TODO: figure out which
     (when (rectangle-contains-point? (bounds-of instance) position)
       (iter (with text = (text-of instance))
             (with font = (font-of instance))
             (for index :from 0 :to (length text))
             (for size = (measure-text (subseq text 0 index) font))
             (when (rectangle-contains-point? (make-rectangle (position-of instance) size) position)
               (return
                 (append reference
                         `((the string (text-of (the graphics/text document)))
                           (the string (subseq (the string document) ,(1- index) ,(1- index))))))))))
    (graphics/image
     (when (rectangle-contains-point? (bounds-of instance) position)
       reference))
    (graphics/viewport
     (when (rectangle-contains-point? instance position)
       (make-reference (content-of instance) (- position (position-of instance))
                       (append reference
                               `((the ,(document-type (content-of instance)) (content-of (the graphics/viewport document))))))))
    (graphics/canvas
     (bind ((elements (elements-of instance)))
       (if (typep elements 'computed-ll)
           (iter (for index :from 0)
                 ;; TODO: previous-element-of
                 (for element-ll :initially elements :then (next-element-of element-ll))
                 (while element-ll)
                 (for element = (value-of element-ll))
                 (thereis (make-reference element (- position (position-of instance))
                                          (append reference
                                                  `((the sequence (elements-of (the graphics/canvas document)))
                                                    (the ,(document-type element) (elt (the sequence document) ,index)))))))
           (iter (for index :from (1- (length elements)) :downto 0)
                 (for element = (elt elements index))
                 (thereis (make-reference element (- position (position-of instance))
                                          (append reference
                                                  `((the sequence (elements-of (the graphics/canvas document)))
                                                    (the ,(document-type element) (elt (the sequence document) ,index))))))))))))

(def function graphics/read-operation (graphics gesture)
  (gesture-case gesture
    ((make-instance 'gesture/mouse/click :mouse-position 0 :button :button-left :modifiers nil)
     :domain "Graphics" :description "Moves the selection to where the mouse is pointing at"
     :operation (make-operation/replace-selection graphics (make-reference graphics (mouse-position-of gesture) nil)))
    ;; KLUDGE: for toggle collapsed tree node operation
    ((make-instance 'gesture/mouse/click :mouse-position 0 :button :button-left :modifiers '(:control))
     :domain "Graphics" :description "Moves the selection to where the mouse is pointing at"
     :operation (make-operation/replace-selection graphics (make-reference graphics (mouse-position-of gesture) nil)))
    ((make-instance 'gesture/keyboard/key-press :mouse-position 0 :key :scancode-i :modifiers '(:control))
     :domain "Graphics" :description "Describes what the mouse is pointing at"
     :operation (make-operation/describe (make-reference graphics (mouse-position-of gesture) nil)))
    ((make-instance 'gesture/keyboard/key-press :mouse-position 0 :key :scancode-a :modifiers '(:control))
     :domain "Graphics" :description "TODO"
     :operation (make-instance 'operation/show-annotation :document graphics :selection (make-reference graphics (mouse-position-of gesture) nil)))))
