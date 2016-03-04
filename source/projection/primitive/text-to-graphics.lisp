;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text/text->graphics/canvas ()
  ())

;;;;;;
;;; Construction

(def function make-projection/text/text->graphics/canvas ()
  (make-projection 'text/text->graphics/canvas))

;;;;;;
;;; Construction

(def macro text/text->graphics/canvas ()
  `(make-projection/text/text->graphics/canvas))

;;;;;;
;;; IO map

(def iomap iomap/text/text->graphics/canvas/text ()
  ((line-iomaps :type sequence)))

(def iomap iomap/text/text->graphics/canvas/line ()
  ((element-iomaps :type sequence)
   (line-start-character-index :type integer)
   (line-end-character-index :type integer)
   (first-character-indices :type sequence)
   (last-character-indices :type sequence)
   (graphics-element-indices :type sequence)
   (line-y :type number)))

;;;;;;
;;; Forward mapper

(def forward-mapper text/text->graphics/canvas ()
  (pattern-case -reference-
    (((the sequence (elements-of (the text/text document)))
      (the ?type (elt (the sequence document) 3))
      . ?rest)
     (bind (;; TODO:
            (line-index 0)
            (line-iomap (elt (line-iomaps-of -printer-iomap-) line-index))
            (element-index 3)
            (element-iomap (elt (element-iomaps-of line-iomap) 0)))
       (values `((the sequence (elements-of (the graphics/canvas document)))
                 (the graphics/canvas (elt (the sequence document) 1))
                 (the sequence (elements-of (the graphics/canvas document)))
                 (the graphics/canvas (elt (the sequence document) ,line-index))
                 (the sequence (elements-of (the graphics/canvas document)))
                 (the graphics/canvas (elt (the sequence document) ,element-index))
                 (the sequence (elements-of (the graphics/canvas document)))
                 (the graphics/canvas (elt (the sequence document) 0))
                 . ?rest)
               ?rest
               element-iomap)))
    (((the graphics/canvas (printer-output (the text/text document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper text/text->graphics/canvas ()
  (pattern-case -reference-
    (((the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) 1))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) ?line-index))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/text (elt (the sequence document) ?element-index))
      (the string (text-of (the graphics/text document)))
      (the string (subseq (the string document) ?graphics-start-character-index ?graphics-end-character-index)))
     (bind ((line-iomap (elt (line-iomaps-of -printer-iomap-) ?line-index))
            (position (position ?element-index (graphics-element-indices-of line-iomap)))
            (character-index (when position
                               (+ ?graphics-start-character-index (line-start-character-index-of line-iomap) (elt (first-character-indices-of line-iomap) position)))))
       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
    (((the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) 1))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) ?line-index))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) ?element-index))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) 0))
      . ?rest)
     (bind ((line-iomap (elt (line-iomaps-of -printer-iomap-) ?line-index))
            ;; TODO:
            (element-iomap (elt (element-iomaps-of line-iomap) #+nil ?element-index 0)))
       (values `((the sequence (elements-of (the text/text document)))
                 (the ,(document-type (input-of element-iomap)) (elt (the sequence document) ,?element-index)))
               ?rest
               element-iomap)))
    (?a
     (append `((the graphics/canvas (printer-output (the text/text document) ,-projection- ,-recursion-))) -reference-))))

;;;;;;
;;; Printer

(def printer text/text->graphics/canvas ()
  (labels ((line-height (line-start-position line-end-position)
             (or (iter (for element :initially (text/position-element line-start-position) :then (text/next-element -input- element))
                       (for text-element = (text/element -input- element))
                       (etypecase text-element
                         (text/spacing)
                         (text/newline
                          (awhen (font-of text-element)
                            (maximize (2d-y (measure-text " " it)))))
                         (text/string
                          (maximize (+ (2d-y (measure-text " " (font-of text-element)))
                                       (aif (padding-of text-element)
                                            (inset/height it)
                                            0))))
                         (text/graphics
                          (bind ((graphics (output-of (recurse-printer -recursion- (content-of text-element) nil)))
                                 (bounding-rectangle (bounds-of graphics)))
                            (maximize (+ (2d-y (size-of bounding-rectangle))
                                         (aif (padding-of text-element)
                                              (inset/height it)
                                              0))))))
                       (until (eql element (text/position-element line-end-position))))
                 42))
           (find-line-iomap (line-iomaps character-index)
             (iter (for line-iomap-element :initially line-iomaps :then (if (< character-index 0)
                                                                            (previous-element-of line-iomap-element)
                                                                            (next-element-of line-iomap-element)))
                   (while line-iomap-element)
                   (for line-iomap = (value-of line-iomap-element))
                   (for line-start-character-index = (line-start-character-index-of line-iomap))
                   (for line-end-character-index = (line-end-character-index-of line-iomap))
                   (when (<= line-start-character-index character-index line-end-character-index)
                     (return (values line-iomap line-iomap-element)))))
           (find-graphics-character-index (line-iomap character-index)
             (iter (for index :from 0)
                   (for first-character-index :in (first-character-indices-of line-iomap))
                   (for last-character-index :in (last-character-indices-of line-iomap))
                   (when (<= first-character-index character-index last-character-index)
                     (return (values (elt (graphics-element-indices-of line-iomap) index) (- character-index first-character-index))))))
           (make-cursor-elements (position height)
             (list (make-graphics/line (+ position 0)
                                       (+ position (make-2d 0 height))
                                       :stroke-color *color/black*)
                   (make-graphics/line (+ position (make-2d -2 0))
                                       (+ position (make-2d 2 0))
                                       :stroke-color *color/black*)
                   (make-graphics/line (+ position (make-2d -2 height))
                                       (+ position (make-2d 2 height))
                                       :stroke-color *color/black*)))
           (find-character-position (line-iomap character-index)
             (bind ((line-start-character-index (line-start-character-index-of line-iomap))
                    ((:values graphics-element-index graphics-character-index) (find-graphics-character-index line-iomap (- character-index line-start-character-index)))
                    (graphics-element (elt (elements-of (output-of line-iomap)) graphics-element-index)))
               (etypecase graphics-element
                 (graphics/text
                  (bind ((text (subseq (text-of graphics-element) 0 graphics-character-index))
                         (position (+ (position-of graphics-element) (make-2d 0 (line-y-of line-iomap))))
                         (font (font-of graphics-element))
                         (size (measure-text text font))
                         (offset (2d-x size))
                         (height (2d-y size)))
                    (values (+ position offset) height)))
                 (graphics/base
                  (bind ((position (+ (position-of graphics-element) (make-2d 0 (line-y-of line-iomap))))
                         (size (size-of (bounds-of graphics-element)))
                         (offset (if (zerop graphics-character-index) 0 (2d-x size)))
                         (height (2d-y size)))
                    (values (+ position offset) height))))))
           (print-selection-below (line-iomaps)
             (pattern-case (get-selection -input-)
               (((the text/text (text/subbox (the text/text document) ?start-index ?end-index)))
                (bind (((:values start-line-iomap start-line-iomap-element) (find-line-iomap line-iomaps ?start-index))
                       ((:values end-line-iomap end-line-iomap-element) (find-line-iomap line-iomaps ?end-index))
                       ((:values start-position start-height) (find-character-position start-line-iomap ?start-index))
                       ((:values end-position end-height) (find-character-position end-line-iomap ?end-index))
                       (padding 1)
                       (left (- (min (2d-x start-position) (2d-x end-position)) padding))
                       (top (- (min (2d-y start-position) (2d-y end-position)) padding))
                       (right (+ (max (2d-x start-position) (2d-x end-position)) padding))
                       (bottom (+ (max (+ (2d-y start-position) start-height) (+ (2d-y end-position) end-height)) padding)))
                  (iter (for line-iomap-element :initially start-line-iomap-element :then (next-element-of line-iomap-element))
                        (unless (eq line-iomap-element end-line-iomap-element)
                          (bind ((rectangle (bounds-of (last-elt (elements-of (output-of (value-of line-iomap-element)))))))
                            (setf right (max right (2d-x (+ (position-of rectangle) (size-of rectangle)))))))
                        (until (eq line-iomap-element end-line-iomap-element)))
                  (list (make-graphics/rounded-rectangle (make-2d left top)
                                                         (make-2d (- right left) (- bottom top))
                                                         3
                                                         :fill-color *color/solarized/background/light*))))))
           (print-selection-above (line-iomaps)
             (pattern-case (get-selection -input-)
               (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                (bind ((line-iomap (find-line-iomap line-iomaps ?character-index)))
                  (when line-iomap
                    (if (elements-of (output-of line-iomap))
                        (bind (((:values position height) (find-character-position line-iomap ?character-index)))
                          (make-cursor-elements position height))
                        (make-cursor-elements 0 20)))))))
           (print-line (line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height)
             (bind ((element-iomaps nil)
                    (first-character-indices nil)
                    (last-character-indices nil)
                    (graphics-element-indices nil)
                    (graphics-elements (if (text/position= line-start-position line-end-position)
                                           ;; TODO: this case could be merged with the loop
                                           (progn
                                             (push 0 first-character-indices)
                                             (push 1 last-character-indices)
                                             (push 0 graphics-element-indices)
                                             (list (make-graphics/text 0 ""
                                                                       :font (or (font-of (text/element -input- (text/position-element line-start-position))) *font/default*)
                                                                       :font-color *color/default*
                                                                       :fill-color nil)))
                                           (iter (with x = 0)
                                                 (with graphics-element-index = 0)
                                                 (with character-index = 0)
                                                 (for position :initially line-start-position :then (text/next-position -input- position))
                                                 (until (text/position= position line-end-position))
                                                 (for text-element = (text/element -input- (text/position-element position)))
                                                 (for previous-text-element :previous text-element)
                                                 (awhen (padding-of text-element)
                                                   (incf x (left-of it)))
                                                 (etypecase text-element
                                                   (text/spacing
                                                    (push graphics-element-index graphics-element-indices)
                                                    (push character-index first-character-indices)
                                                    (incf character-index)
                                                    (push character-index last-character-indices)
                                                    (incf graphics-element-index)
                                                    (collect (make-graphics/rectangle (make-2d x 0) (make-2d (size-of text-element) line-height) :fill-color nil))
                                                    (incf x (size-of text-element)))
                                                   (text/string
                                                    (unless (eq text-element previous-text-element)
                                                      (bind ((content (content-of text-element))
                                                             (size (measure-text content (font-of text-element))))
                                                        (push character-index first-character-indices)
                                                        (incf character-index (length content))
                                                        (push character-index last-character-indices)
                                                        (awhen (line-color-of text-element)
                                                          (incf graphics-element-index)
                                                          (collect (make-graphics/rectangle (make-2d x 0) (make-2d (2d-x size) line-height) :fill-color it)))
                                                        (push graphics-element-index graphics-element-indices)
                                                        (incf graphics-element-index)
                                                        (collect (make-graphics/text (make-2d x (- line-height (2d-y size) (aif (padding-of text-element) (bottom-of it) 0)))
                                                                                     content
                                                                                     :font (font-of text-element)
                                                                                     :font-color (font-color-of text-element)
                                                                                     :fill-color (fill-color-of text-element)))
                                                        (incf x (2d-x size)))))
                                                   (text/graphics
                                                    (bind ((element-iomap (recurse-printer -recursion- (content-of text-element) nil))
                                                           (graphics (output-of element-iomap))
                                                           (bounding-rectangle (bounds-of graphics)))
                                                      (push element-iomap element-iomaps)
                                                      (push graphics-element-index graphics-element-indices)
                                                      (push character-index first-character-indices)
                                                      (incf character-index)
                                                      (push character-index last-character-indices)
                                                      (incf graphics-element-index)
                                                      (collect (make-graphics/canvas (list graphics) (make-2d x (- line-height (2d-y (size-of bounding-rectangle))))))
                                                      (incf x (2d-x (size-of bounding-rectangle))))))
                                                 (awhen (padding-of text-element)
                                                   (incf x (right-of it)))))))
               (make-instance 'iomap/text/text->graphics/canvas/line
                              :projection -projection- :recursion -recursion-
                              :input -input- :output (make-graphics/canvas graphics-elements (make-2d 0 line-y))
                              :element-iomaps element-iomaps
                              :line-start-character-index line-start-character-index :line-end-character-index line-end-character-index
                              :first-character-indices first-character-indices :last-character-indices last-character-indices
                              :graphics-element-indices graphics-element-indices :line-y line-y)))
           (print-containing-line (start-position line-start-character-index-in line-end-character-index-in line-y previous-element next-element)
             ;; TODO: all this (as) (va) magic is to keep the canvas of a line dependent on as small input as possible
             (bind ((line-start-position (text/line-start-position -input- start-position))
                    (line-end-position (text/line-end-position -input- start-position))
                    (line-length (as (text/length -input- line-start-position line-end-position)))
                    (line-start-character-index (as (or (va line-start-character-index-in) (- (va line-end-character-index-in) (va line-length)))))
                    (line-end-character-index (as (or (va line-end-character-index-in) (+ (va line-start-character-index-in) (va line-length)))))
                    (line-height (line-height line-start-position line-end-position))
                    (previous-line-end-position (text/previous-position -input- line-start-position))
                    (next-line-start-position (text/next-position -input- line-end-position)))
               (make-computed-ll (as (print-line line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height))
                                 (as (or previous-element
                                         (when previous-line-end-position
                                           (print-containing-line previous-line-end-position (as nil) (as (1- (va line-start-character-index))) (- line-y line-height) nil -self-))))
                                 (as (or next-element
                                         (when next-line-start-position
                                           (print-containing-line next-line-start-position (as (1+ (va line-end-character-index))) (as nil) (+ line-y line-height) -self- nil))))))))
    (bind ((line-iomaps (as (bind ((origin-position (text/origin-position -input-))
                                   (origin-line-character-index (when origin-position
                                                                  (text/length -input- (text/line-start-position -input- origin-position) origin-position))))
                              (when origin-position
                                (print-containing-line origin-position (as (- origin-line-character-index)) (as nil) 0 nil nil)))))
           (output (as (make-graphics/canvas (list (make-graphics/canvas (as (print-selection-below (va line-iomaps))) 0)
                                                   (make-graphics/canvas (as (map-ll (va line-iomaps) 'output-of)) 0)
                                                   (make-graphics/canvas (as (print-selection-above (va line-iomaps))) 0))
                                             0))))
      (make-instance 'iomap/text/text->graphics/canvas/text
                     :projection -projection- :recursion -recursion-
                     :input -input- :input-reference -input-reference- :output output
                     :line-iomaps line-iomaps))))

;;;;;;
;;; Reader

(def reader text/text->graphics/canvas
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/text/text->graphics/canvas 'backward-mapper/text/text->graphics/canvas)
                    (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/text/text->graphics/canvas nil)
                    (text/read-operation -printer-input- -gesture-)
                    (document/read-operation -gesture-)
                    (awhen (graphics/read-operation -printer-output- -gesture-)
                      (command/read-backward -recursion- it -printer-iomap- 'backward-mapper/text/text->graphics/canvas nil))
                    (make-nothing-command -gesture-)))
