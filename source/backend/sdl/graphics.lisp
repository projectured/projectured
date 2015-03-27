;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def method raw-of :around ((font style/font))
  (or (call-next-method)
      (va (setf (raw-of font) (as (or (ignore-errors (sdl:initialise-font (make-instance 'sdl:ttf-font-definition :size (size-of font) :filename (resource-pathname (filename-of font)))))
                                      (raw-of *font/default*)))))))

(def method raw-of :around ((color style/color))
  (or (call-next-method)
      (va (setf (raw-of color) (as (sdl:color :r (red-of color) :g (green-of color) :b (blue-of color) :a (alpha-of color)))))))

(def method raw-of :around ((image image/file))
  (or (call-next-method)
      (bind ((filename (filename-of image)))
        (va (setf (raw-of image) (as (sdl-image:load-image (if (starts-with #\/ (namestring filename))
                                                               filename
                                                               (merge-pathnames filename (hu.dwim.asdf:system-pathname :projectured))) :alpha 255)))))))

(def method measure-text (text font)
  (make-2d (sdl:get-font-size text :size :w :font (raw-of font))
           (sdl:get-font-size text :size :h :font (raw-of font))))

(def method make-bounding-rectangle ((instance graphics/image))
  (bind ((image (image-of instance))
         (rectangle (sdl:get-surface-rect :surface (raw-of image))))
    (make-rectangle (location-of instance) (make-2d (sdl:width rectangle) (sdl:height rectangle)))))
