;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection graphics/canvas->graphics/image ()
  ((threshold 16 :type number)))

(def projection graphics/viewport->graphics/image ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/graphics/canvas->graphics/image ()
  (make-projection 'graphics/canvas->graphics/image))

(def (function e) make-projection/graphics/viewport->graphics/image ()
  (make-projection 'graphics/viewport->graphics/image))

;;;;;;
;;; Construction

(def (macro e) graphics/canvas->graphics/image ()
  '(make-projection/graphics/canvas->graphics/image))

(def (macro e) graphics/viewport->graphics/image ()
  '(make-projection/graphics/viewport->graphics/image))

;;;;;;
;;; Printer

(def printer graphics/canvas->graphics/image (projection recursion input input-reference)
  (bind ((element-iomaps (as (iter (for element :in-sequence (elements-of input))
                                   (collect (recurse-printer recursion element nil)))))
         (output (as (if (> (length (elements-of input)) (threshold-of projection))
                         (make-graphics/image (as (location-of input)) (make-image/memory (as (bind ((size (size-of (make-bounding-rectangle input)))
                                                                                                     (*translation* (make-2d 0 0)))
                                                                                                (declare (special *translation*))
                                                                                                (printer.debug "Creating image of ~A canvas elements" (length (elements-of input)))
                                                                                                ;; TODO: KLUDGE: when do we destroy this surface?
                                                                                                (sdl:with-surface (surface (sdl:create-surface (2d-x size) (2d-y size) :color-key sdl:*white* :type :hw) #f)
                                                                                                  (sdl:fill-surface sdl:*white*)
                                                                                                  (print-to-device (make-graphics/canvas (mapcar 'output-of (va element-iomaps)) (make-2d 0 0))
                                                                                                                   (make-device/display/sdl (2d-x size) (2d-y size)))
                                                                                                  sdl:*default-surface*)))))
                         (make-graphics/canvas (as (mapcar 'output-of (va element-iomaps))) (as (location-of input)))))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer graphics/viewport->graphics/image (projection recursion input input-reference)
  (bind ((contents-iomap (as (list (recurse-printer recursion (content-of input) nil))))
         (output (as (make-graphics/image (as (location-of input)) (make-image/memory (as (bind ((size (size-of input))
                                                                                                 (*translation* (make-2d 0 0)))
                                                                                            (declare (special *translation*))
                                                                                            (printer.debug "Creating image of viewport")
                                                                                            ;; TODO: KLUDGE: when do we destroy this surface?
                                                                                            (sdl:with-surface (surface (sdl:create-surface (2d-x size) (2d-y size) :color-key sdl:*white* :type :hw) #f)
                                                                                              (sdl:fill-surface sdl:*white*)
                                                                                              (print-to-device (make-graphics/viewport (output-of (first (va contents-iomap))) (make-2d 0 0) (size-of input))
                                                                                                               (make-device/display/sdl (2d-x size) (2d-y size)))
                                                                                              sdl:*default-surface*))))))))
    (make-iomap/compound projection recursion input input-reference output contents-iomap)))

;;;;;;
;;; Reader

(def reader graphics/canvas->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader graphics/viewport->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
