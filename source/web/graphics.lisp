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
      (setf (raw-of font) (sdl:initialise-font (make-instance 'sdl:ttf-font-definition :size (size-of font) :filename (filename-of font))))))

(def method raw-of :around ((color style/color))
  (or (call-next-method)
      (setf (raw-of color) (sdl:color :r (red-of color) :g (green-of color) :b (blue-of color) :a (alpha-of color)))))

(def method measure-text (text font)
  (make-2d (sdl:get-font-size text :size :w :font (raw-of font))
           (sdl:get-font-size text :size :h :font (raw-of font))))
