;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.sdl)

;;;;;;
;;; Backend

(def class* backend/sdl (backend)
  ((sdl-windows (make-hash-table) :type hash-table)))

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
         (#,SDL_Init #,SDL_INIT_EVERYTHING)
         (#,TTF_Init)
         (#,IMG_Init 0)
         (apply 'read-evaluate-print-loop editor args))
    (progn
      (iter (for (key value) :in-hashtable (sdl-windows-of (backend-of editor)))
            (#,SDL_DestroyWindow value))
      (#,IMG_Quit)
      ;; KLUDGE: TODO: this would free the fonts loaded in the raw slot of fonts and lead to a crash
      #+nil (#,TTF_Quit)
      (#,SDL_Quit))))

(def function find-sdl-window (backend title)
  (gethash title (sdl-windows-of backend)))

(def function (setf find-sdl-window) (window backend title)
  (if window
      (setf (gethash title (sdl-windows-of backend)) window)
      (remhash title (sdl-windows-of backend))))

;; TODO move some of these into hu.dwim.sdl, but on which name?
(def function load-image (filename)
  (bind ((img (the (not null)
                (#,IMG_Load filename))))
    (trivial-garbage:finalize img (lambda () (#,SDL_FreeSurface img)))
    img))

(def function make-sdl-color (red green blue alpha)
  ;; TODO maybe get rid of the struct-by-value translation?
  `(#,r ,red #,g ,green #,b ,blue #,a ,alpha))
