;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection graphics/canvas->graphics/image ()
  ((threshold 2 :type number)))

(def projection graphics/viewport->graphics/image ()
  ())

;;;;;;
;;; Construction

(def function make-projection/graphics/canvas->graphics/image ()
  (make-projection 'graphics/canvas->graphics/image))

(def function make-projection/graphics/viewport->graphics/image ()
  (make-projection 'graphics/viewport->graphics/image))

;;;;;;
;;; Construction

(def macro graphics/canvas->graphics/image ()
  '(make-projection/graphics/canvas->graphics/image))

(def macro graphics/viewport->graphics/image ()
  '(make-projection/graphics/viewport->graphics/image))

;;;;;;
;;; Printer

(def printer graphics/canvas->graphics/image (projection recursion input input-reference)
  (bind ((elements (elements-of input)))
    (if (typep elements 'computed-ll)
        (bind ((output (labels ((recurse (element)
                                  (make-computed-ll (as (output-of (recurse-printer recursion (hu.dwim.computed-class::value-of element) nil)))
                                                    (as nil) (as nil)
                                                    (as (awhen (previous-element-of element) (recurse it)))
                                                    (as (awhen (next-element-of element) (recurse it))))))
                         (make-graphics/canvas (as (recurse elements)) (location-of input)))))
          (make-iomap/object projection recursion input input-reference output))
        (bind ((element-iomaps (as (iter (for element :in-sequence (elements-of input))
                                         (collect (recurse-printer recursion element nil)))))
               (output (as (if (> (length (elements-of input)) (threshold-of projection))
                               (make-graphics/image (as (location-of input)) (make-image/memory (as (bind ((size (size-of (make-bounding-rectangle input)))
                                                                                                           (*translation* (make-2d 0 0)))
                                                                                                      (declare (special *translation*))
                                                                                                      (printer.debug "Creating image of ~A canvas" (length (elements-of input)))
                                                                                                      ;; TODO: KLUDGE: when do we destroy this surface?
                                                                                                      (sdl:with-surface (surface (sdl:create-surface (2d-x size) (2d-y size) :color-key sdl:*white* :type :hw) #f)
                                                                                                        (sdl:fill-surface sdl:*white*)
                                                                                                        (print-to-device (make-graphics/canvas (mapcar 'output-of (va element-iomaps)) (make-2d 0 0))
                                                                                                                         (make-device/display/sdl (2d-x size) (2d-y size)))
                                                                                                        sdl:*default-surface*)))))
                               (make-graphics/canvas (as (mapcar 'output-of (va element-iomaps))) (as (location-of input)))))))
          (make-iomap/compound projection recursion input input-reference output element-iomaps)))))

(def printer graphics/viewport->graphics/image (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) nil)))
         (output (as (if t ;; TODO: conditional
                         (make-graphics/viewport (output-of (va content-iomap)) (location-of input) (size-of input))
                         (make-graphics/image (as (location-of input)) (make-image/memory (as (bind ((size (size-of input))
                                                                                                     (*translation* (make-2d 0 0)))
                                                                                                (declare (special *translation*))
                                                                                                (printer.debug "Creating image of viewport")
                                                                                                ;; TODO: KLUDGE: when do we destroy this surface?
                                                                                                (sdl:with-surface (surface (sdl:create-surface (2d-x size) (2d-y size) :color-key sdl:*white* :type :hw) #f)
                                                                                                  (sdl:fill-surface sdl:*white*)
                                                                                                  (print-to-device (make-graphics/viewport (output-of (va content-iomap)) (make-2d 0 0) (size-of input))
                                                                                                                   (make-device/display/sdl (2d-x size) (2d-y size)))
                                                                                                  sdl:*default-surface*)))))))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader graphics/canvas->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader graphics/viewport->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
