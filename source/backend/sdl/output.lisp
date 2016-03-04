;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.sdl)

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
         (surface (#,SDL_CreateRGBSurface 0 (2d-x surface-size) (2d-y surface-size) 24 0 0 0 0))
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

(def (with-macro* :macro-only-arguments (x-name y-name)) with-sdl-point-vectors (x-name y-name points translation)
  (bind ((count (length points)))
    (cffi:with-foreign-objects ((xs '#,Sint16 count)
                                (ys '#,Sint16 count))
      (iter (for offset :upfrom 0 :by (load-time-value (cffi:foreign-type-size '#,Sint16)))
            (for point :in-sequence points)
            (incf point translation)
            (setf (cffi:mem-ref xs '#,Sint16 offset) (round (2d-x point)))
            (setf (cffi:mem-ref ys '#,Sint16 offset) (round (2d-y point))))
      (-with-macro/body- (xs x-name) (ys y-name)))))

(def function output-to-renderer (backend renderer instance translation)
  (typecase instance
    (graphics/text
     (unless (zerop (length (text-of instance)))
       (bind ((text (coerce (text-of instance) 'string))
              (font (font-of instance))
              (fill-color (fill-color-of instance))
              (font-color (font-color-of instance))
              (position (+ translation (position-of instance))))
         (when fill-color
           (bind ((size (measure-text text font)))
             (set-render-draw-color renderer fill-color)
             (render-fill-rect renderer
                               (round (2d-x position))
                               (round (2d-y position))
                               (round (2d-x size))
                               (round (2d-y size)))))
         (unless (every 'whitespace? text)
           (bind ((surface (#,TTF_RenderUTF8_Blended (raw-of font) text (raw-of font-color)))
                  (texture (#,SDL_CreateTextureFromSurface renderer surface)))
             (cffi:with-foreign-object (rectangle '#,SDL_Rect)
               (macrolet ((set-rect (slot value)
                            `(setf (cffi:foreign-slot-value rectangle '#,SDL_Rect ,slot) ,value)))
                 (set-rect '#,x (round (2d-x position)))
                 (set-rect '#,y (round (2d-y position)))
                 (set-rect '#,w (cffi:foreign-slot-value surface '#,SDL_Surface '#,w))
                 (set-rect '#,h (cffi:foreign-slot-value surface '#,SDL_Surface '#,h)))
               (#,SDL_RenderCopy renderer texture (cffi:null-pointer) rectangle)
               (#,SDL_DestroyTexture texture)
               (#,SDL_FreeSurface surface)))))))

    (graphics/image
     (bind ((position (+ translation (position-of instance)))
            (surface (raw-of (image-of instance)))
            (texture (#,SDL_CreateTextureFromSurface renderer surface)))
       (cffi:with-foreign-object (rectangle '#,SDL_Rect)
         (macrolet ((set-rect (slot value)
                      `(setf (cffi:foreign-slot-value rectangle '#,SDL_Rect ,slot) ,value)))
           (set-rect '#,x (round (2d-x position)))
           (set-rect '#,y (round (2d-y position)))
           (set-rect '#,w (cffi:foreign-slot-value surface '#,SDL_Surface '#,w))
           (set-rect '#,h (cffi:foreign-slot-value surface '#,SDL_Surface '#,h)))
         (#,SDL_RenderCopy renderer texture (cffi:null-pointer) rectangle)
         (#,SDL_DestroyTexture texture)
         (#,SDL_FreeSurface surface))))

    (graphics/canvas
     (bind ((translation (+ translation (position-of instance)))
            (elements (elements-of instance)))
       (if (typep elements 'computed-ll)
           (cffi:with-foreign-object (rectangle '#,SDL_Rect)
             (#,SDL_RenderGetClipRect renderer rectangle)
             (bind ((y (cffi:foreign-slot-value rectangle '#,SDL_Rect '#,y))
                    (h (cffi:foreign-slot-value rectangle '#,SDL_Rect '#,h)))
               (output-to-renderer backend renderer (value-of elements) translation)
               (rebind (translation)
                 (iter (for element :initially (previous-element-of elements) :then (previous-element-of element))
                       (while element)
                       (for value = (value-of element))
                       (while (> (2d-y (+ translation (position-of value) (size-of (bounds-of value)))) y))
                       (when (< (2d-y (+ translation (position-of value))) (+ y h))
                         (output-to-renderer backend renderer value translation))))
               (rebind (translation)
                 (iter (for element :initially (next-element-of elements) :then (next-element-of element))
                       (while element)
                       (for value = (value-of element))
                       (while (< (2d-y (+ translation (position-of value))) (+ y h)))
                       (when (> (2d-y (+ translation (position-of value) (size-of (bounds-of value)))) y)
                         (output-to-renderer backend renderer value translation))))))
           (iter (for element :in-sequence elements)
                 (output-to-renderer backend renderer element translation)))))

    (graphics/rounded-rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (radius (radius-of instance))
            (corners (corners-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (if (equal corners 'nil)
           (progn
             (when fill-color
               (#,roundedBoxRGBA renderer
                                 (round (2d-x position)) (round (2d-y position))
                                 (round (+ (2d-x position) (2d-x size))) (round (+ (2d-y position) (2d-y size)))
                                 radius
                                 (round (* 255 (red-of fill-color)))
                                 (round (* 255 (green-of fill-color)))
                                 (round (* 255 (blue-of fill-color)))
                                 (round (* 255 (alpha-of fill-color)))))
             (when stroke-color
               (#,roundedRectangleRGBA renderer
                                       (round (2d-x position)) (round (2d-y position))
                                       (round (+ (2d-x position) (2d-x size))) (round (+ (2d-y position) (2d-y size)))
                                       radius
                                       (round (* 255 (red-of stroke-color)))
                                       (round (* 255 (green-of stroke-color)))
                                       (round (* 255 (blue-of stroke-color)))
                                       (round (* 255 (alpha-of stroke-color))))))
           (progn
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
                   (progn
                     (#,aacircleRGBA renderer
                                     (round (+ (2d-x position) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     (round (* 255 (red-of fill-color)))
                                     (round (* 255 (green-of fill-color)))
                                     (round (* 255 (blue-of fill-color)))
                                     (round (* 255 (alpha-of fill-color))))
                     (#,filledCircleRGBA renderer
                                         (round (+ (2d-x position) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         (round (* 255 (red-of fill-color)))
                                         (round (* 255 (green-of fill-color)))
                                         (round (* 255 (blue-of fill-color)))
                                         (round (* 255 (alpha-of fill-color))))))
               (if (member :bottom-right corners)
                   (render-fill-rect renderer
                                     (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     (round radius))
                   (progn
                     (#,aacircleRGBA renderer
                                     (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (- (+ (2d-y position) (2d-y size)) radius))
                                     (round radius)
                                     (round (* 255 (red-of fill-color)))
                                     (round (* 255 (green-of fill-color)))
                                     (round (* 255 (blue-of fill-color)))
                                     (round (* 255 (alpha-of fill-color))))
                     (#,filledCircleRGBA renderer
                                         (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (- (+ (2d-y position) (2d-y size)) radius))
                                         (round radius)
                                         (round (* 255 (red-of fill-color)))
                                         (round (* 255 (green-of fill-color)))
                                         (round (* 255 (blue-of fill-color)))
                                         (round (* 255 (alpha-of fill-color))))))
               (if (member :top-left corners)
                   (render-fill-rect renderer
                                     (round (2d-x position))
                                     (round (2d-y position))
                                     (round radius)
                                     (round radius))
                   (progn
                     (#,aacircleRGBA renderer
                                     (round (+ (2d-x position) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     (round (* 255 (red-of fill-color)))
                                     (round (* 255 (green-of fill-color)))
                                     (round (* 255 (blue-of fill-color)))
                                     (round (* 255 (alpha-of fill-color))))
                     (#,filledCircleRGBA renderer
                                         (round (+ (2d-x position) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         (round (* 255 (red-of fill-color)))
                                         (round (* 255 (green-of fill-color)))
                                         (round (* 255 (blue-of fill-color)))
                                         (round (* 255 (alpha-of fill-color))))))
               (if (member :top-right corners)
                   (render-fill-rect renderer
                                     (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (2d-y position))
                                     (round radius)
                                     (round radius))
                   (progn
                     (#,aacircleRGBA renderer
                                     (round (- (+ (2d-x position) (2d-x size)) radius))
                                     (round (+ (2d-y position) radius))
                                     (round radius)
                                     (round (* 255 (red-of fill-color)))
                                     (round (* 255 (green-of fill-color)))
                                     (round (* 255 (blue-of fill-color)))
                                     (round (* 255 (alpha-of fill-color))))
                     (#,filledCircleRGBA renderer
                                         (round (- (+ (2d-x position) (2d-x size)) radius))
                                         (round (+ (2d-y position) radius))
                                         (round radius)
                                         (round (* 255 (red-of fill-color)))
                                         (round (* 255 (green-of fill-color)))
                                         (round (* 255 (blue-of fill-color)))
                                         (round (* 255 (alpha-of fill-color)))))))
             (when stroke-color
               (set-render-draw-color renderer stroke-color)
               (render-draw-rect renderer
                                 (round (2d-x position))
                                 (round (2d-y position))
                                 (round (2d-x size))
                                 (round (2d-y size))))))))

    (graphics/rectangle
     (bind ((position (+ translation (position-of instance)))
            (size (size-of instance))
            (fill-color (fill-color-of instance))
            (stroke-color (stroke-color-of instance)))
       (when fill-color
         (set-render-draw-color renderer fill-color)
         (render-fill-rect renderer
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
       (cffi:with-foreign-objects ((old-clipping '#,SDL_Rect)
                                   (new-clipping '#,SDL_Rect))
         (#,SDL_RenderGetClipRect renderer old-clipping)
         (macrolet ((set-rect (slot value)
                      `(setf (cffi:foreign-slot-value new-clipping '#,SDL_Rect ,slot) ,value)))
           (set-rect '#,x (round (2d-x position)))
           (set-rect '#,y (round (2d-y position)))
           (set-rect '#,w (round (2d-x size)))
           (set-rect '#,h (round (2d-y size))))
         (#,SDL_RenderSetClipRect renderer new-clipping)
         (output-to-renderer backend renderer (content-of instance) translation)
         (#,SDL_RenderSetClipRect renderer old-clipping))))

    (graphics/line
     (bind ((begin (+ translation (begin-of instance)))
            (end (+ translation (end-of instance)))
            (stroke-color (stroke-color-of instance)))
       (set-render-draw-color renderer stroke-color)
       (#,SDL_RenderDrawLine renderer
                             (round (2d-x begin))
                             (round (2d-y begin))
                             (round (2d-x end))
                             (round (2d-y end)))))

    (graphics/polygon
     (with-sdl-point-vectors (xs ys (points-of instance) translation)
       (bind ((stroke-color (stroke-color-of instance))
              (fill-color (fill-color-of instance)))
         (when fill-color
           (#,filledPolygonRGBA renderer xs ys
                                (length (points-of instance))
                                (round (* 255 (red-of fill-color)))
                                (round (* 255 (green-of fill-color)))
                                (round (* 255 (blue-of fill-color)))
                                (round (* 255 (alpha-of fill-color)))))
         (when stroke-color
           (#,aapolygonRGBA renderer xs ys
                            (length (points-of instance))
                            (round (* 255 (red-of stroke-color)))
                            (round (* 255 (green-of stroke-color)))
                            (round (* 255 (blue-of stroke-color)))
                            (round (* 255 (alpha-of stroke-color))))))))

    (graphics/bezier
     (with-sdl-point-vectors (xs ys (points-of instance) translation)
       (bind ((stroke-color (stroke-color-of instance)))
         (#,bezierRGBA renderer xs ys
                       (length (points-of instance))
                       10
                       (round (* 255 (red-of stroke-color)))
                       (round (* 255 (green-of stroke-color)))
                       (round (* 255 (blue-of stroke-color)))
                       (round (* 255 (alpha-of stroke-color)))))))

    (graphics/circle
     (bind ((center (+ translation (position-of instance)))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (#,filledCircleRGBA renderer
                             (round (2d-x center))
                             (round (2d-y center))
                             (round (radius-of instance))
                             (round (* 255 (red-of fill-color)))
                             (round (* 255 (green-of fill-color)))
                             (round (* 255 (blue-of fill-color)))
                             (round (* 255 (alpha-of fill-color)))))
       (when stroke-color
         (#,aacircleRGBA renderer
                         (round (2d-x center))
                         (round (2d-y center))
                         (round (radius-of instance))
                         (round (* 255 (red-of stroke-color)))
                         (round (* 255 (green-of stroke-color)))
                         (round (* 255 (blue-of stroke-color)))
                         (round (* 255 (alpha-of stroke-color)))))))

    (graphics/ellipse
     (bind ((center (+ translation (position-of instance)))
            (radius (radius-of instance))
            (stroke-color (stroke-color-of instance))
            (fill-color (fill-color-of instance)))
       (when fill-color
         (#,filledEllipseRGBA renderer
                              (round (2d-x center))
                              (round (2d-y center))
                              (round (2d-x radius))
                              (round (2d-y radius))
                              (round (* 255 (red-of fill-color)))
                              (round (* 255 (green-of fill-color)))
                              (round (* 255 (blue-of fill-color)))
                              (round (* 255 (alpha-of fill-color)))))
       (when stroke-color
         (#,aaellipseRGBA renderer
                          (round (2d-x center))
                          (round (2d-y center))
                          (round (2d-x radius))
                          (round (2d-y radius))
                          (round (* 255 (red-of stroke-color)))
                          (round (* 255 (green-of stroke-color)))
                          (round (* 255 (blue-of stroke-color)))
                          (round (* 255 (alpha-of stroke-color)))))))

    (graphics/point
     (bind ((position (+ translation (position-of instance))))
       (set-render-draw-color renderer (stroke-color-of instance))
       (#,SDL_RenderDrawPoint renderer
                              (round (2d-x position))
                              (round (2d-y position)))))

    (output/display
     (iter (for window :in-sequence (windows-of instance))
           (unless (find-sdl-window backend (title-of window))
             (setf (find-sdl-window backend (title-of window))
                   (bind ((title (title-of window))
                          (position (position-of window))
                          (size (size-of window))
                          (x (if position
                                 (2d-x position)
                                 (logior #,SDL_WINDOWPOS_CENTERED_MASK 0)))
                          (y (if position
                                 (2d-y position)
                                 (logior #,SDL_WINDOWPOS_CENTERED_MASK 0))))
                     (if title
                         (#,SDL_CreateWindow title x y (2d-x size) (2d-y size) 0)
                         (#,SDL_CreateWindow "" x y (2d-x size) (2d-y size) #,SDL_WINDOW_BORDERLESS))))))
     (iter (for (title sdl-window) :in-hashtable (sdl-windows-of backend))
           (unless (find title (windows-of instance) :key 'title-of)
             (#,SDL_DestroyWindow sdl-window)
             (setf (find-sdl-window backend title) nil)))
     (iter (for window :in-sequence (windows-of instance))
           (output-to-renderer backend renderer window translation)))

    (output/window
     (bind ((window (find-sdl-window backend (title-of instance)))
            (size (size-of instance))
            (renderer (or (bind ((renderer (#,SDL_GetRenderer window)))
                            (if (cffi:null-pointer-p renderer)
                                nil
                                renderer))
                          (#,SDL_CreateRenderer window -1 0))))
       (set-render-draw-color renderer *color/white*)
       (#,SDL_RenderClear renderer)
       (cffi:with-foreign-object (new-clipping '#,SDL_Rect)
         (macrolet ((set-rect (slot value)
                      `(setf (cffi:foreign-slot-value new-clipping '#,SDL_Rect ,slot) ,value)))
           (set-rect '#,x 0)
           (set-rect '#,y 0)
           (set-rect '#,w (round (2d-x size)))
           (set-rect '#,h (round (2d-y size))))
         (#,SDL_RenderSetClipRect renderer new-clipping)
         (output-to-renderer backend renderer (content-of instance) 0))
       (#,SDL_RenderPresent renderer)))))
