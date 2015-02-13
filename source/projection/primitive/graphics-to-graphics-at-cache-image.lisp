;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection graphics/canvas->graphics/image@cache-graphics ()
  ((debug :type boolean)
   (threshold 2 :type number)))

(def projection graphics/viewport->graphics/image@cache-graphics ()
  ((debug :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/graphics/canvas->graphics/image@cache-graphics (&key debug)
  (make-projection 'graphics/canvas->graphics/image@cache-graphics :debug debug))

(def function make-projection/graphics/viewport->graphics/image@cache-graphics (&key debug)
  (make-projection 'graphics/viewport->graphics/image@cache-graphics :debug debug))

;;;;;;
;;; Construction

(def macro graphics/canvas->graphics/image@cache-graphics (&key debug)
  `(make-projection/graphics/canvas->graphics/image@cache-graphics :debug ,debug))

(def macro graphics/viewport->graphics/image@cache-graphics (&key debug)
  `(make-projection/graphics/viewport->graphics/image@cache-graphics :debug ,debug))

;;;;;;
;;; Printer

(def printer graphics/canvas->graphics/image@cache-graphics (projection recursion input input-reference)
  (bind ((elements (elements-of input)))
    (if (typep elements 'computed-ll)
        (bind ((output (labels ((recurse (element)
                                  (make-computed-ll (as (output-of (recurse-printer recursion (hu.dwim.computed-class::value-of element) nil)))
                                                    (as (awhen (previous-element-of element) (recurse it)))
                                                    (as (awhen (next-element-of element) (recurse it))))))
                         (make-graphics/canvas (as (recurse elements)) (location-of input)))))
          (make-iomap/object projection recursion input input-reference output))
        (bind ((element-iomaps (as (make-instance 'computed-sequence :elements (iter (for element :in-sequence (elements-of input))
                                                                                     (rebind (element)
                                                                                       (collect (as (recurse-printer recursion element nil))))))))
               (elements (as (iter (for element-iomap :in-sequence (va element-iomaps))
                                   (collect (output-of element-iomap)))))
               (output (as (if (and (> (length (elements-of input)) (threshold-of projection))
                                    (notany (of-type 'graphics/canvas) (elements-of input)))
                               (bind ((bounding-rectangle (make-bounding-rectangle input)))
                                 (make-graphics/image (as (location-of bounding-rectangle))
                                                      (make-image/memory (as (bind ((size (size-of bounding-rectangle))
                                                                                    (*translation* (- (location-of bounding-rectangle))))
                                                                               (declare (special *translation*))
                                                                               (printer.debug "Creating image of ~A canvas elements" (length (elements-of input)))
                                                                               ;; TODO: KLUDGE: when do we destroy this surface?
                                                                               (sdl:with-surface (surface (sdl:create-surface (2d-x size) (2d-y size) :color-key sdl:*white* :type :hw) #f)
                                                                                 (sdl:fill-surface sdl:*white*)
                                                                                 (print-to-device (make-graphics/canvas elements (location-of input))
                                                                                                  (make-device/display/sdl (2d-x size) (2d-y size)))
                                                                                 (when (debug-p projection)
                                                                                   (sdl:draw-aa-line-* 0 0 (2d-x size) (1-(2d-y size)) :color (raw-of *color/solarized/gray*))
                                                                                   (sdl:draw-aa-line-* 0 (1- (2d-y size)) (2d-x size) 0 :color (raw-of *color/solarized/gray*)))
                                                                                 sdl:*default-surface*))))))
                               (make-graphics/canvas elements (as (location-of input)))))))
          (make-iomap/compound projection recursion input input-reference output element-iomaps)))))

(def printer graphics/viewport->graphics/image@cache-graphics (projection recursion input input-reference)
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
                                                                                                  (when (debug-p projection)
                                                                                                    (sdl:draw-aa-line-* 0 0 (2d-x size) (1-(2d-y size)) :color (raw-of *color/solarized/gray*))
                                                                                                    (sdl:draw-aa-line-* 0 (1- (2d-y size)) (2d-x size) 0 :color (raw-of *color/solarized/gray*)))
                                                                                                  sdl:*default-surface*)))))))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader graphics/canvas->graphics/image@cache-graphics (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader graphics/viewport->graphics/image@cache-graphics (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
