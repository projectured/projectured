;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def method raw-of :around ((font style/font))
  (or (call-next-method)
      (va (setf (raw-of font)
                (as (sdl2-ffi.functions:ttf-open-font (namestring (resource-pathname (filename-of font))) (size-of font)))))))

(def method raw-of :around ((color style/color))
  (or (call-next-method)
      (va (setf (raw-of color)
                (as (sdl2-ttf::create-sdl-color-list (round (* 255 (red-of color)))
                                                     (round (* 255 (green-of color)))
                                                     (round (* 255 (blue-of color)))
                                                     (round (* 255 (alpha-of color)))))))))

(def method raw-of :around ((image image/file))
  (or (call-next-method)
      (va (setf (raw-of image)
                (as (sdl2-image:load-image (namestring (resource-pathname (filename-of image)))))))))

(def function measure-text (text font)
  (plus-c:c-with ((w :int) (h :int))
    (sdl2-ttf::ttf-size-utf8 (raw-of font) text (w plus-c:&) (h plus-c:&))
    (make-2d w h)))

(def function measure-image (image)
  (make-2d 100 100)
  #+nil ; TODO: old code
  (bind ((rectangle (sdl:get-surface-rect :surface (raw-of image))))
    (make-2d (sdl:width rectangle) (sdl:height rectangle))))

(def function set-render-draw-color (renderer color)
  (sdl2-ffi.functions:sdl-set-render-draw-color renderer
                                                (round (* 255 (red-of color)))
                                                (round (* 255 (green-of color)))
                                                (round (* 255 (blue-of color)))
                                                (round (* 255 (alpha-of color)))))

(def function render-draw-rect (renderer x y width height)
  (plus-c:c-with ((rectangle sdl2-ffi:sdl-rect))
    (setf (rectangle :x) x
          (rectangle :y) y
          (rectangle :w) width
          (rectangle :h) height)
    (sdl2-ffi.functions:sdl-render-draw-rect renderer rectangle)))

(def function render-fill-rect (renderer x y width height)
  (plus-c:c-with ((rectangle sdl2-ffi:sdl-rect))
    (setf (rectangle :x) x
          (rectangle :y) y
          (rectangle :w) width
          (rectangle :h) height)
    (sdl2-ffi.functions:sdl-render-fill-rect renderer rectangle)))
