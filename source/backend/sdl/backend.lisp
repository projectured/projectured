;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured/sdl)

;;;;;;
;;; Backend

(def class* backend/sdl (backend)
  ((windows (make-hash-table) :type hash-table)))

(def (function e) make-sdl-backend ()
  (make-instance 'backend/sdl
                 :read-evaluate-print-loop 'sdl/read-evaluate-print-loop
                 :input-from-devices 'input-from-devices
                 :output-to-devices 'output-to-devices))

;;;;;;
;;; Utils

;; TODO move some of these into hu.dwim.sdl, but on which name?
(defun sdl/load-image (filename)
  (bind ((img (c-fun/not-null #,IMG_Load filename)))
    (trivial-garbage:finalize img (lambda ()
                                    (#,SDL_FreeSurface img)))
    img))

(defun make-sdl-color (red green blue alpha)
  ;; TODO maybe get rid of the struct-by-value translation?
  `(#,r ,red
    #,g ,green
    #,b ,blue
    #,a ,alpha))

;;;;;;
;;; API

(def function sdl/read-evaluate-print-loop (editor &rest args &key &allow-other-keys)
  (unwind-protect
       (progn
         (#,SDL_Init #,SDL_INIT_EVERYTHING)
         (#,TTF_Init)
         (#,IMG_Init 0)
         (apply 'read-evaluate-print-loop editor args))
    (iter (for (key value) :in-hashtable (windows-of (backend-of editor)))
          (#,SDL_DestroyWindow value))
    (#,IMG_Quit)
    ;; KLUDGE: TODO: this would free the fonts loaded in the raw slot of fonts and lead to a crash
    #+nil (#,TTF_Quit)
    (#,SDL_Quit)))

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
                              (logior #,SDL_WINDOWPOS_CENTERED_MASK 0)))
                       (y (if position
                              (2d-y position)
                              (logior #,SDL_WINDOWPOS_CENTERED_MASK 0))))
                  (if title
                      (#,SDL_CreateWindow title x y (2d-x size) (2d-y size) 0)
                      (#,SDL_CreateWindow "" x y (2d-x size) (2d-y size) #,SDL_WINDOW_BORDERLESS))))))))
