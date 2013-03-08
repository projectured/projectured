;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Printer

(def special-variable *translation* (make-2d 0 0))

(def method print-to-device ((instance graphics/canvas) (display device/display/sdl))
  (bind ((*translation* (+ *translation* (location-of instance))))
    (iter (for element :in-sequence (elements-of instance))
          (print-to-device element display))))

(def method print-to-device ((instance graphics/viewport) (display device/display/sdl))
  (bind ((location (+ *translation* (location-of instance)))
         (size (size-of instance))
         (clipping (sdl:get-clip-rect)))
    (sdl:set-clip-rect (sdl:rectangle :x (2d-x location) :y (2d-y location) :w (2d-x size) :h (2d-y size)))
    (print-to-device (content-of instance) display)
    (sdl:set-clip-rect clipping)))

(def method print-to-device ((instance graphics/point) (display device/display/sdl))
  (bind ((location (+ *translation* (location-of instance))))
    (sdl:draw-pixel-* (round (2d-x location))
                      (round (2d-y location))
                      :color (raw-of (stroke-color-of instance)))))

(def method print-to-device ((instance graphics/line) (display device/display/sdl))
  (bind ((begin (+ *translation* (begin-of instance)))
         (end (+ *translation* (end-of instance))))
    (sdl:draw-aa-line-* (round (2d-x begin))
                        (round (2d-y begin))
                        (round (2d-x end))
                        (round (2d-y end))
                        :color (raw-of (stroke-color-of instance)))))

(def method print-to-device ((instance graphics/rectangle) (display device/display/sdl))
  (bind ((location (+ *translation* (location-of instance)))
         (size (size-of instance)))
    (when (fill-color-of instance)
      (sdl:draw-box-* (round (2d-x location))
                      (round (2d-y location))
                      ;; TODO: validate
                      (1- (round (2d-x size)))
                      (1- (round (2d-y size)))
                      :color (raw-of (fill-color-of instance))))
    (when (stroke-color-of instance)
      (sdl:draw-rectangle-* (round (2d-x location))
                            (round (2d-y location))
                            (round (2d-x size))
                            (round (2d-y size))
                            :color (raw-of (stroke-color-of instance))))))

(def method print-to-device ((instance graphics/polygon) (display device/display/sdl))
  (sdl:draw-aa-polygon (iter (for point :in-sequence (points-of instance))
                             (incf point *translation*)
                             (collect (sdl:point :x (2d-x point) :y (2d-y point))))
                       :color (raw-of (stroke-color-of instance))))

(def method print-to-device ((instance graphics/circle) (display device/display/sdl))
  (bind ((center (+ *translation* (center-of instance))))
    (sdl::draw-aa-circle-* (round (2d-x center))
                           (round (2d-y center))
                           (round (radius-of instance))
                           :color (raw-of (stroke-color-of instance)))))

(def method print-to-device ((instance graphics/ellipse) (display device/display/sdl))
  (bind ((center (+ *translation* (center-of instance)))
         (radius (radius-of instance)))
    (sdl::draw-aa-ellipse-* (round (2d-x center))
                            (round (2d-y center))
                            (round (2d-x radius))
                            (round (2d-y radius))
                            :color (raw-of (stroke-color-of instance)))))

(def method print-to-device ((instance graphics/text) (display device/display/sdl))
  (unless (string= "" (text-of instance))
    (bind ((text (text-of instance))
           (font (font-of instance))
           (fill-color (fill-color-of instance))
           (location (+ *translation* (location-of instance))))
      (when fill-color
        (bind ((size (measure-text text font)))
          (sdl:draw-box-* (round (2d-x location))
                          (round (2d-y location))
                          (round (2d-x size))
                          (round (2d-y size))
                          :color (raw-of fill-color))))
      (sdl:draw-string-blended-* text
                                 (round (2d-x location))
                                 (round (2d-y location))
                                 :font (raw-of font)
                                 :color (raw-of (font-color-of instance))))))

(def method print-to-device ((instance graphics/image) (display device/display/sdl))
  ;; TODO: do not load every time
  (bind ((location (+ *translation* (location-of instance))))
    (sdl:draw-surface-at-* (sdl-image:load-image (source-of instance) :color-key-at #(0 0))
                           (round (2d-x location))
                           (round (2d-y location)))))
