;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Backend

(def class* backend/sdl (backend)
  ((windows (make-hash-table) :type hash-table)))

(def function make-sdl-backend ()
  (make-instance 'backend/sdl
                 :read-evaluate-print-loop 'sdl/read-evaluate-print-loop
                 :input-from-devices 'input-from-devices
                 :output-to-devices 'output-to-devices))

;;;;;;
;;; API

(def function sdl/read-evaluate-print-loop (editor &rest args &key &allow-other-keys)
  (unwind-protect
       (progn
         (sdl2-ffi.functions:sdl-init (autowrap:mask-apply 'sdl2::sdl-init-flags '(:everything)))
         (sdl2-ffi.functions:ttf-init)
         (sdl2-ffi.functions:img-init (autowrap:mask-apply 'sdl2-image::img-initflags nil))
         (apply 'read-evaluate-print-loop editor args))
    (progn
      (iter (for (key value) :in-hashtable (windows-of (backend-of editor)))
            (sdl2-ffi.functions:sdl-destroy-window value))
      (sdl2-ffi.functions:img-quit)
      ;; KLUDGE: TODO: this would free the fonts loaded in the raw slot of fonts and lead to a crash
      #+nil (sdl2-ffi.functions:ttf-quit)
      (sdl2-ffi.functions:sdl-quit))))

(def function ensure-window (backend window)
  (bind ((windows (windows-of backend)))
    (or (gethash (title-of window) windows)
        (setf (gethash (title-of window) windows)
              (bind ((bounds (bounds-of window))
                     (size (size-of bounds))
                     (title (title-of window)))
                (bind ((position (position-of window))
                       (x (if position
                              (2d-x position)
                              (logior sdl2-ffi:+sdl-windowpos-centered-mask+ 0)))
                       (y (if position
                              (2d-y position)
                              (logior sdl2-ffi:+sdl-windowpos-centered-mask+ 0))))
                  (if title
                      (sdl2-ffi.functions:sdl-create-window title x y (2d-x size) (2d-y size) (autowrap:mask-apply 'sdl2::sdl-window-flags nil))
                      (sdl2-ffi.functions:sdl-create-window "" x y (2d-x size) (2d-y size) (autowrap:mask-apply 'sdl2::sdl-window-flags '(:borderless))))))))))
