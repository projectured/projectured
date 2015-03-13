;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Graphics test suite

(def suite* (test/graphics :in test))

(def test test/graphics/simple ()
  (unwind-protect
       (progn
         (sdl:init-sdl)
         (sdl-image:init-image :jpg :png :tif)
         (sdl:initialise-default-font (make-instance 'sdl:ttf-font-definition :size 18 :filename (resource-pathname "font/UbuntuMono-R.ttf")))
         (sdl:init-video)
         (bind ((surface (sdl:window 1000 1000 :flags sdl-cffi::sdl-no-frame :double-buffer #t :title-caption "Projectional Editor")))
           (time
            (sb-sprof:with-profiling (:loop #f)
              (sdl:with-surface (surface)
                (sdl:fill-surface sdl:*white*)
                (iter (for i :from 0 :to 1000)
                      (sdl:draw-string-blended-* "Hello"
                                                 (random 1000)
                                                 (random 1000)
                                                 :color sdl:*black*))
                (sdl:update-display))))))
    (sdl:quit-video)))
