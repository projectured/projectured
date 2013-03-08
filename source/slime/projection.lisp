;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Printer

(def method print-to-device ((instance graphics/canvas) (display device/display/slime))
  (iter (for element :in (elements-of instance))
        (print-to-device element display)))

(def method print-to-device ((instance graphics/viewport) (display device/display/slime))
  (print-to-device (content-of instance) display))

(def method print-to-device ((instance graphics/point) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/line) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/rectangle) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/circle) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/ellipse) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/polygon) (display device/display/slime))
  (values))

(def method print-to-device ((instance graphics/image) (display device/display/slime))
  (values))

(def method print-to-device ((instance styled-string/document) (display device/display/slime))
  (iter (for element :in (elements-of instance))
        (print-to-device element display)))

(def method print-to-device ((instance styled-string/string) (display device/display/slime))
  (bind ((fill-color (fill-color-of instance)))
    (swank::eval-in-emacs
     `(with-current-buffer (slime-repl-buffer)
        (insert (propertize ,(content-of instance) 'font-lock-face
                            '(:foreground ,(raw-of (font-color-of instance))
                              :background ,(when fill-color (raw-of fill-color)))))))))
