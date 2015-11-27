;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def function output-to-devices (editor)
  (bind ((document (document-of editor))
         (projection (projection-of editor))
         (output-devices (remove-if-not (of-type 'device/output) (devices-of editor)))
         (printer-iomap (if (slot-boundp editor 'printer-iomap)
                            (printer-iomap-of editor)
                            (setf (printer-iomap-of editor) (apply-printer document projection)))))
    (iter (for device :in-sequence output-devices)
          (output-to-device (backend-of editor) (output-of printer-iomap) device))))

(def function output-to-device (backend instance device)
  (etypecase device
    (device/file
     (output-to-file backend instance device))
    (device/display
     (output-to-display backend instance device))))

(def function output-to-file (backend instance device)
  (bind ((rectangle (bounds-of instance))
         (surface-size (- (size-of rectangle) (position-of rectangle)))
         (surface (sdl2-ffi.functions:sdl-create-rgb-surface 0 (2d-x surface-size) (2d-y surface-size) 24 0 0 0 0))
         (translation (- (position-of rectangle))))
    (setf (raw-of device) surface)
    #+nil ; old code
    (sdl:with-surface (surface)
      (sdl:fill-surface sdl:*white*)
      (output-to-surface backend instance translation)
      (sdl:save-image surface (filename-of device)))))

(def function output-to-display (backend instance device)
  (declare (ignore device))
  (output-to-renderer backend nil instance 0))

(def function output-to-renderer (backend renderer instance translation)
  (typecase instance
    (graphics/text
     (unless (zerop (length (text-of instance)))
       (bind ((text (coerce (text-of instance) 'string))
              (font (font-of instance))
              (fill-color (fill-color-of instance))
              (font-color (font-color-of instance))
              (position (+ translation (position-of instance))))
         #+nil ; old code
         (when fill-color
           (bind ((size (measure-text text font)))
             (sdl:draw-box-* (round (2d-x position))
                             (round (2d-y position))
                             (round (2d-x size))
                             (round (2d-y size))
                             :color (raw-of fill-color))))
         (unless (or (every 'whitespace? text)
                     #+nil (< (2d-y position) (- (sdl:get-font-size text :size :h :font (raw-of font))))
                     #+nil (> (2d-y position) (sdl:height sdl:*default-surface*)))
           (bind ((surface (sdl2-ttf::%sdl-render-utf8-blended (autowrap:ptr (raw-of font)) text (raw-of font-color)))
                  (texture (sdl2-ffi.functions::sdl-create-texture-from-surface renderer surface)))
             (plus-c:c-with ((rectangle sdl2-ffi:sdl-rect))
               (setf (rectangle :x) (round (2d-x position))
                     (rectangle :y) (round (2d-y position))
                     (rectangle :w) (plus-c:c-ref surface sdl2-ffi:sdl-surface :w)
                     (rectangle :h) (plus-c:c-ref surface sdl2-ffi:sdl-surface :h))
               (sdl2-ffi.functions:sdl-render-copy renderer texture nil rectangle)
               (sdl2-ffi.functions:sdl-destroy-texture texture)
               (sdl2-ffi.functions:sdl-free-surface surface)))))))

    #+nil
    (graphics/image
     (bind ((position (+ translation (position-of instance))))
       (sdl:draw-surface-at-* (raw-of (image-of instance))
                              (round (2d-x position))
                              (round (2d-y position)))))

    (graphics/canvas
     (bind ((translation (+ translation (position-of instance)))
            (elements (elements-of instance)))
       (iter (for element :in-sequence elements)
             (output-to-renderer backend renderer element translation))))

    (graphics/rounded-rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (radius (radius-of instance))
            (corners (corners-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (set-render-draw-color renderer fill-color)
         (render-fill-rect renderer
                           (round (2d-x position))
                           (round (+ (2d-y position) radius))
                           (round (2d-x size))
                           (round (- (2d-y size) (* 2 radius))))
         (render-fill-rect renderer
                           (round (+ (2d-x position) radius))
                           (round (2d-y position))
                           (round (- (2d-x size) (* 2 radius)))
                           (round (2d-y size)))
         (if (member :bottom-left corners)
             (render-fill-rect renderer
                               (round (2d-x position))
                               (round (- (+ (2d-y position) (2d-y size)) radius))
                               (round radius)
                               (round radius))
             #+nil
             (progn
               (sdl:draw-aa-circle-* (round (+ (2d-x position) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (+ (2d-x position) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :bottom-right corners)
             (render-fill-rect renderer
                               (round (- (+ (2d-x position) (2d-x size)) radius))
                               (round (- (+ (2d-y position) (2d-y size)) radius))
                               (round radius)
                               (round radius))
             #+nil
             (progn
               (sdl:draw-aa-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :top-left corners)
             (render-fill-rect renderer
                               (round (2d-x position))
                               (round (2d-y position))
                               (round radius)
                               (round radius))
             #+nil
             (progn
               (sdl:draw-aa-circle-* (round (+ (2d-x position) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (+ (2d-x position) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :top-right corners)
             (render-fill-rect renderer
                               (round (- (+ (2d-x position) (2d-x size)) radius))
                               (round (2d-y position))
                               (round radius)
                               (round radius))
             #+nil
             (progn
               (sdl:draw-aa-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         :color (raw-of fill-color)))))
       (when stroke-color
         (set-render-draw-color renderer stroke-color)
         (render-draw-rect renderer
                           (round (2d-x position))
                           (round (2d-y position))
                           (round (2d-x size))
                           (round (2d-y size))))))

    (graphics/rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (fill-color (fill-color-of instance))
            (stroke-color (stroke-color-of instance)))
       (when fill-color
         (set-render-draw-color renderer fill-color)
         (render-draw-rect renderer
                           (round (2d-x position))
                           (round (2d-y position))
                           ;; TODO: validate
                           (1- (round (2d-x size)))
                           (1- (round (2d-y size)))))
       (when stroke-color
         (set-render-draw-color renderer stroke-color)
         (render-draw-rect renderer
                           (round (2d-x position))
                           (round (2d-y position))
                           (round (2d-x size))
                           (round (2d-y size))))))

    (graphics/viewport
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance)))
       (output-to-renderer backend renderer (content-of instance) translation)))

    (graphics/line
     (bind ((begin (+ translation (begin-of instance)))
            (end (+ translation (end-of instance)))
            (stroke-color (stroke-color-of instance)))
       (set-render-draw-color renderer stroke-color)
       (sdl2-ffi.functions:sdl-render-draw-line renderer
                                                (round (2d-x begin))
                                                (round (2d-y begin))
                                                (round (2d-x end))
                                                (round (2d-y end)))))
    (graphics/window
     (bind ((window (ensure-window backend instance))
            (renderer (or (bind ((renderer (sdl2-ffi.functions:sdl-get-renderer window)))
                            (if (cffi:null-pointer-p (autowrap:ptr renderer))
                                nil
                                renderer))
                          (sdl2-ffi.functions:sdl-create-renderer window -1 (autowrap:mask-apply 'sdl2::sdl-renderer-flags nil)))))
       (set-render-draw-color renderer *color/white*)
       (sdl2-ffi.functions:sdl-render-clear renderer)
       (output-to-renderer backend renderer (content-of instance) 0)
       (sdl2-ffi.functions:sdl-render-present renderer)))

    #+nil
    (graphics/polygon
     (sdl:draw-aa-polygon (iter (for point :in-sequence (points-of instance))
                                (incf point translation)
                                (collect (sdl:point :x (2d-x point) :y (2d-y point))))
                          :color (raw-of (stroke-color-of instance))))

    #+nil
    (graphics/circle
     (bind ((center (+ translation (position-of instance)))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-filled-circle-* (round (2d-x center))
                                   (round (2d-y center))
                                   (round (radius-of instance))
                                   :color (raw-of fill-color)))
       (when stroke-color
         (sdl::draw-aa-circle-* (round (2d-x center))
                                (round (2d-y center))
                                (round (radius-of instance))
                                :color (raw-of stroke-color)))))

    #+nil
    (graphics/ellipse
     (bind ((center (+ translation (position-of instance)))
            (radius (radius-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-filled-ellipse-* (round (2d-x center))
                                    (round (2d-y center))
                                    (round (2d-x radius))
                                    (round (2d-y radius))
                                    :color (raw-of fill-color)))
       (when stroke-color
         (sdl::draw-aa-ellipse-* (round (2d-x center))
                                 (round (2d-y center))
                                 (round (2d-x radius))
                                 (round (2d-y radius))
                                 :color (raw-of stroke-color)))))

    (graphics/point
     (bind ((position (+ translation (position-of instance)))
            (stroke-color (raw-of (stroke-color-of instance))))
       (sdl2-ffi.functions:sdl-render-draw-point renderer
                                                 (round (2d-x position))
                                                 (round (2d-y position)))))))

#+nil ; old code
(def function output-to-surface (backend instance translation)
  ;; types are in the order of frequency
  (etypecase instance
    (graphics/text
     (unless (zerop (length (text-of instance)))
       (bind ((text (coerce (text-of instance) 'string))
              (font (font-of instance))
              (fill-color (fill-color-of instance))
              (position (+ translation (position-of instance))))
         (when fill-color
           (bind ((size (measure-text text font)))
             (sdl:draw-box-* (round (2d-x position))
                             (round (2d-y position))
                             (round (2d-x size))
                             (round (2d-y size))
                             :color (raw-of fill-color))))
         (unless (or (every 'whitespace? text)
                     (< (2d-y position) (- (sdl:get-font-size text :size :h :font (raw-of font))))
                     (> (2d-y position) (sdl:height sdl:*default-surface*)))
           (sdl:draw-string-blended-* text
                                      (round (2d-x position))
                                      (round (2d-y position))
                                      :font (raw-of font)
                                      :color (raw-of (font-color-of instance)))))))

    (graphics/image
     (bind ((position (+ translation (position-of instance))))
       (sdl:draw-surface-at-* (raw-of (image-of instance))
                              (round (2d-x position))
                              (round (2d-y position)))))

    (graphics/canvas
     (bind ((translation (+ translation (position-of instance)))
            (elements (elements-of instance)))
       (if (typep elements 'computed-ll)
           (bind ((clipping (sdl:get-clip-rect)))
             (output-to-surface backend (value-of elements) translation)
             (rebind (translation)
               (iter (for element :initially (previous-element-of elements) :then (previous-element-of element))
                     (while element)
                     (for value = (value-of element))
                     (while (> (2d-y (+ translation (position-of value) (size-of (bounds-of value)))) (sdl:y clipping)))
                     (when (< (2d-y (+ translation (position-of value))) (sdl:y2 clipping))
                       (output-to-surface backend value translation))))
             (rebind (translation)
               (iter (for element :initially (next-element-of elements) :then (next-element-of element))
                     (while element)
                     (for value = (value-of element))
                     (while (< (2d-y (+ translation (position-of value))) (sdl:y2 clipping)))
                     (when (> (2d-y (+ translation (position-of value) (size-of (bounds-of value)))) (sdl:y clipping))
                       (output-to-surface backend value translation)))))
           (iter (for element :in-sequence elements)
                 (output-to-surface backend element translation)))))

    (graphics/viewport
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (clipping (sdl:get-clip-rect)))
       (sdl:set-clip-rect (sdl:rectangle :x (2d-x position) :y (2d-y position) :w (2d-x size) :h (2d-y size)))
       (output-to-surface backend (content-of instance) translation)
       (sdl:set-clip-rect clipping)))

    (graphics/rounded-rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (radius (radius-of instance))
            (corners (corners-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-box-* (round (2d-x position))
                         (round (+ (2d-y position) radius))
                         (round (2d-x size))
                         (round (- (2d-y size) (* 2 radius)))
                         :color (raw-of fill-color))
         (sdl:draw-box-* (round (+ (2d-x position) radius))
                         (round (2d-y position))
                         (round (- (2d-x size) (* 2 radius)))
                         (round (2d-y size))
                         :color (raw-of fill-color))
         (if (member :bottom-left corners)
             (sdl:draw-box-* (round (2d-x position))
                             (round (- (+ (2d-y position) (2d-y size)) radius))
                             (round radius)
                             (round radius)
                             :color (raw-of fill-color))
             (progn
               (sdl:draw-aa-circle-* (round (+ (2d-x position) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (+ (2d-x position) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :bottom-right corners)
             (sdl:draw-box-* (round (- (+ (2d-x position) (2d-x size)) radius))
                             (round (- (+ (2d-y position) (2d-y size)) radius))
                             (round radius)
                             (round radius)
                             :color (raw-of fill-color))
             (progn
               (sdl:draw-aa-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :top-left corners)
             (sdl:draw-box-* (round (2d-x position))
                             (round (2d-y position))
                             (round radius)
                             (round radius)
                             :color (raw-of fill-color))
             (progn
               (sdl:draw-aa-circle-* (round (+ (2d-x position) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (+ (2d-x position) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         :color (raw-of fill-color))))
         (if (member :top-right corners)
             (sdl:draw-box-* (round (- (+ (2d-x position) (2d-x size)) radius))
                             (round (2d-y position))
                             (round radius)
                             (round radius)
                             :color (raw-of fill-color))
             (progn
               (sdl:draw-aa-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     :color (raw-of fill-color))
               (sdl:draw-filled-circle-* (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         :color (raw-of fill-color)))))
       (when stroke-color
         (sdl:draw-rectangle-* (2d-x position)
                               (2d-y position)
                               (2d-x size)
                               (2d-y size)
                               :color (raw-of stroke-color)))))

    (graphics/rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-box-* (round (2d-x position))
                         (round (2d-y position))
                         ;; TODO: validate
                         (1- (round (2d-x size)))
                         (1- (round (2d-y size)))
                         :color (raw-of fill-color)))
       (when (stroke-color-of instance)
         (sdl:draw-rectangle-* (round (2d-x position))
                               (round (2d-y position))
                               (round (2d-x size))
                               (round (2d-y size))
                               :color (raw-of (stroke-color-of instance))))))

    (graphics/line
     (bind ((begin (+ translation (begin-of instance)))
            (end (+ translation (end-of instance))))
       ;; TODO: draw-aa-line-* is broken
       (sdl:draw-line-* (round (2d-x begin))
                        (round (2d-y begin))
                        (round (2d-x end))
                        (round (2d-y end))
                        :color (raw-of (stroke-color-of instance)))))

    (graphics/polygon
     (sdl:draw-aa-polygon (iter (for point :in-sequence (points-of instance))
                                (incf point translation)
                                (collect (sdl:point :x (2d-x point) :y (2d-y point))))
                          :color (raw-of (stroke-color-of instance))))

    (graphics/circle
     (bind ((center (+ translation (position-of instance)))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-filled-circle-* (round (2d-x center))
                                   (round (2d-y center))
                                   (round (radius-of instance))
                                   :color (raw-of fill-color)))
       (when stroke-color
         (sdl::draw-aa-circle-* (round (2d-x center))
                                (round (2d-y center))
                                (round (radius-of instance))
                                :color (raw-of stroke-color)))))

    (graphics/ellipse
     (bind ((center (+ translation (position-of instance)))
            (radius (radius-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (sdl:draw-filled-ellipse-* (round (2d-x center))
                                    (round (2d-y center))
                                    (round (2d-x radius))
                                    (round (2d-y radius))
                                    :color (raw-of fill-color)))
       (when stroke-color
         (sdl::draw-aa-ellipse-* (round (2d-x center))
                                 (round (2d-y center))
                                 (round (2d-x radius))
                                 (round (2d-y radius))
                                 :color (raw-of stroke-color)))))

    (graphics/point
     (bind ((position (+ translation (position-of instance))))
       (sdl:draw-pixel-* (round (2d-x position))
                         (round (2d-y position))
                         :color (raw-of (stroke-color-of instance)))))

    (graphics/screen
     (bind ((surface (ensure-surface backend instance)))
       (sdl:fill-surface sdl:*white* :surface surface)
       (sdl:with-surface (surface)
         (output-to-surface backend (content-of instance) 0)
         (sdl:update-display))))))
