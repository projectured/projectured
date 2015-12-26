;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured/sdl)

;;;;;;
;;; API

(def method raw-of :around ((font style/font))
  (or (call-next-method)
      (va (setf (raw-of font)
                (as (#,TTF_OpenFont (namestring (resource-pathname (filename-of font))) (size-of font)))))))

(def method raw-of :around ((color style/color))
  (or (call-next-method)
      (va (setf (raw-of color)
                (as (make-sdl-color (round (* 255 (red-of color)))
                                    (round (* 255 (green-of color)))
                                    (round (* 255 (blue-of color)))
                                    (round (* 255 (alpha-of color)))))))))

(def method raw-of :around ((image image/file))
  (or (call-next-method)
      (va (setf (raw-of image)
                (as (sdl/load-image (namestring (resource-pathname (filename-of image)))))))))

(def function measure-text (text font)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (#,TTF_SizeUTF8 (raw-of font) text w h)
    (make-2d (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(def function measure-image (image)
  (make-2d 100 100)
  #+nil ; TODO: old code
  (bind ((rectangle (sdl:get-surface-rect :surface (raw-of image))))
    (make-2d (sdl:width rectangle) (sdl:height rectangle))))

(def function set-render-draw-color (renderer color)
  (c-fun/rc #,SDL_SetRenderDrawColor renderer
            (round (* 255 (red-of color)))
            (round (* 255 (green-of color)))
            (round (* 255 (blue-of color)))
            (round (* 255 (alpha-of color)))))

(def function render-draw-rect (renderer x y width height)
  (cffi:with-foreign-objects ((rectangle '#,SDL_Rect))
    (macrolet ((copy (slot value)
                 `(setf (cffi:foreign-slot-value rectangle '#,SDL_Rect ',slot) ,value)))
      (copy #,x x)
      (copy #,y y)
      (copy #,w width)
      (copy #,h height))
    (c-fun/rc #,SDL_RenderDrawRect renderer rectangle)))

(def function render-fill-rect (renderer x y width height)
  (cffi:with-foreign-objects ((rectangle '#,SDL_Rect))
    (macrolet ((copy (slot value)
                 `(setf (cffi:foreign-slot-value rectangle '#,SDL_Rect ',slot) ,value)))
      (copy #,x x)
      (copy #,y y)
      (copy #,w width)
      (copy #,h height))
    (c-fun/rc #,SDL_RenderFillRect renderer rectangle)))
