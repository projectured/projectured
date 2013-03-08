;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Printer

(def special-variable *translation* (make-2d 0 0))

(def method print-to-device ((instance graphics/canvas) (display device/display/web))
  (iter (for element :in-sequence (elements-of instance))
        (bind ((*translation* (+ *translation* (location-of instance))))
          (print-to-device element display))))

(def method print-to-device ((instance graphics/viewport) (display device/display/web))
  (print-to-device (content-of instance) display))

(def method print-to-device ((instance graphics/text) (display device/display/web))
  (bind ((color (font-color-of instance))
         (location (location-of instance)))
    ;; TODO: factor, use qq string
    `str(,(format nil "~%context.fillStyle = 'rgb(~A, ~A, ~A)'; context.fillText(~S, ~A, ~A);"
                  (red-of color) (green-of color) (blue-of color)
                  (text-of instance)
                  (+ (2d-x *translation*) (2d-x location))
                  (+ (2d-y *translation*) (2d-y location))))))

(def method print-to-device ((instance graphics/point) (display device/display/web))
  ;; TODO:
  )

(def method print-to-device ((instance graphics/line) (display device/display/web))
  ;; TODO: factor, use qq string
  (bind ((begin (begin-of instance))
         (end (end-of instance))
         (color (stroke-color-of instance)))
    `str(,(format nil "~%context.color = 'rgb(~A, ~A, ~A)'; context.moveTo(~A, ~A); context.lineTo(~A, ~A); context.stroke();"
                  (red-of color) (green-of color) (blue-of color)
                  (2d-x begin) (2d-y begin) (2d-x end) (2d-y end)))))

(def method print-to-device ((instance graphics/rectangle) (display device/display/web))
  ;; TODO:
  )

(def method print-to-device ((instance graphics/polygon) (display device/display/web))
  ;; TODO:
  )

(def method print-to-device ((instance graphics/circle) (display device/display/web))
  ;; TODO:
  )

(def method print-to-device ((instance graphics/ellipse) (display device/display/web))
  ;; TODO:
  )

(def method print-to-device ((instance graphics/image) (display device/display/web))
  ;; TODO:
  )
