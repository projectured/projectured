;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/text/text->graphics/canvas/text ()
  ((line-iomaps :type sequence)))

(def iomap iomap/text/text->graphics/canvas/line ()
  ((line-start-character-index :type integer)
   (line-end-character-index :type integer)
   (first-character-indices :type sequence)
   (last-character-indices :type sequence)
   (graphics-element-indices :type sequence)
   (line-y :type number)))

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
;;; Printer

(def printer text/text->graphics/canvas (projection recursion input input-reference)
  (labels ((line-height (line-start-position line-end-position)
             (or (iter (for element :initially (text/position-element line-start-position) :then (text/next-element input element))
                       (for text-element = (text/element input element))
                       (etypecase text-element
                         (text/newline
                          (maximize (2d-y (measure-text " " (font-of text-element)))))
                         (text/spacing)
                         (text/string
                          (maximize (2d-y (measure-text " " (font-of text-element)))))
                         (image/file
                             (bind ((image (make-graphics/image (make-2d 0 0) text-element))
                                    (size (size-of (make-bounding-rectangle image))))
                               (maximize (2d-y size)))))
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
           (make-cursor-elements (location height)
             (list (make-graphics/line (+ location (make-2d 0 0))
                                       (+ location (make-2d 0 height))
                                       :stroke-color *color/black*)
                   (make-graphics/line (+ location (make-2d -2 0))
                                       (+ location (make-2d 2 0))
                                       :stroke-color *color/black*)
                   (make-graphics/line (+ location (make-2d -2 height))
                                       (+ location (make-2d 2 height))
                                       :stroke-color *color/black*)))
           (find-character-location (line-iomap character-index)
             (bind ((line-start-character-index (line-start-character-index-of line-iomap))
                    ((:values graphics-element-index graphics-character-index) (find-graphics-character-index line-iomap (- character-index line-start-character-index)))
                    (graphics-element (elt (elements-of (output-of line-iomap)) graphics-element-index)))
               (etypecase graphics-element
                 (graphics/text
                  (bind ((text (subseq (text-of graphics-element) 0 graphics-character-index))
                         (location (+ (location-of graphics-element) (make-2d 0 (line-y-of line-iomap))))
                         (font (font-of graphics-element))
                         (size (measure-text text font))
                         (offset (2d-x size))
                         (height (2d-y size)))
                    (values (+ location offset) height)))
                 (graphics/image
                  (bind ((location (+ (location-of graphics-element) (make-2d 0 (line-y-of line-iomap))))
                         (size (size-of (make-bounding-rectangle graphics-element)))
                         (offset (if (zerop graphics-character-index) 0 (2d-x size)))
                         (height (2d-y size)))
                    (values (+ location offset) height))))))
           (print-selection-below (line-iomaps)
             (pattern-case (selection-of input)
               (((the text/text (text/subbox (the text/text document) ?start-index ?end-index)))
                (bind (((:values start-line-iomap start-line-iomap-element) (find-line-iomap line-iomaps ?start-index))
                       ((:values end-line-iomap end-line-iomap-element) (find-line-iomap line-iomaps ?end-index))
                       ((:values start-location start-height) (find-character-location start-line-iomap ?start-index))
                       ((:values end-location end-height) (find-character-location end-line-iomap ?end-index))
                       (padding 1)
                       (left (- (min (2d-x start-location) (2d-x end-location)) padding))
                       (top (- (min (2d-y start-location) (2d-y end-location)) padding))
                       (right (+ (max (2d-x start-location) (2d-x end-location)) padding))
                       (bottom (+ (max (+ (2d-y start-location) start-height) (+ (2d-y end-location) end-height)) padding)))
                  (iter (for line-iomap-element :initially start-line-iomap-element :then (next-element-of line-iomap-element))
                        (unless (eq line-iomap-element end-line-iomap-element)
                          (bind ((rectangle (make-bounding-rectangle (last-elt (elements-of (output-of (value-of line-iomap-element)))))))
                            (setf right (max right (2d-x (+ (location-of rectangle) (size-of rectangle)))))))
                        (until (eq line-iomap-element end-line-iomap-element)))
                  (list (make-graphics/rounded-rectangle (make-2d left top)
                                                         (make-2d (- right left) (- bottom top))
                                                         3
                                                         :fill-color *color/solarized/background/light*))))))
           (print-selection-above (line-iomaps)
             (pattern-case (selection-of input)
               (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                (bind ((line-iomap (find-line-iomap line-iomaps ?character-index)))
                  (when line-iomap
                    (if (elements-of (output-of line-iomap))
                        (bind (((:values location height) (find-character-location line-iomap ?character-index)))
                          (make-cursor-elements location height))
                        (make-cursor-elements 0 20)))))))
           (print-line (line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height)
             (bind ((first-character-indices nil)
                    (last-character-indices nil)
                    (graphics-element-indices nil)
                    (graphics-elements (if (text/position= line-start-position line-end-position)
                                           ;; TODO: this case could be embedding in the loop
                                           (progn
                                             (push 0 first-character-indices)
                                             (push 1 last-character-indices)
                                             (push 0 graphics-element-indices)
                                             (list (make-graphics/text (make-2d 0 0) ""
                                                                       :font (font-of (text/element input (text/position-element line-start-position)))
                                                                       :font-color *color/default*
                                                                       :fill-color nil)))
                                           (iter (with x = 0)
                                                 (with graphics-element-index = 0)
                                                 (with character-index = 0)
                                                 (for position :initially line-start-position :then (text/next-position input position))
                                                 (until (text/position= position line-end-position))
                                                 (for text-element = (text/element input (text/position-element position)))
                                                 (for previous-text-element :previous text-element)
                                                 (etypecase text-element
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
                                                        (collect (make-graphics/text (make-2d x (- line-height (2d-y size))) content
                                                                                     :font (font-of text-element)
                                                                                     :font-color (font-color-of text-element)
                                                                                     :fill-color (fill-color-of text-element)))
                                                        (incf x (2d-x size)))))
                                                   (image/file
                                                       (bind ((image (make-graphics/image (make-2d x 0) text-element)))
                                                         (push graphics-element-index graphics-element-indices)
                                                         (push character-index first-character-indices)
                                                         (incf character-index)
                                                         (push character-index last-character-indices)
                                                         (incf graphics-element-index)
                                                         (collect image)
                                                         (incf x (2d-x (size-of (make-bounding-rectangle image)))))))))))
               (make-iomap 'iomap/text/text->graphics/canvas/line
                           :projection projection :recursion recursion
                           :input input :output (make-graphics/canvas graphics-elements (make-2d 0 line-y))
                           :line-start-character-index line-start-character-index :line-end-character-index line-end-character-index
                           :first-character-indices first-character-indices :last-character-indices last-character-indices
                           :graphics-element-indices graphics-element-indices :line-y line-y)))
           (print-containing-line (start-position line-start-character-index-in line-end-character-index-in line-y previous-element next-element)
             ;; TODO: all this (as) (va) magic is to keep the canvas of a line dependent on as small input as possible
             (bind ((line-start-position (text/line-start-position input start-position))
                    (line-end-position (text/line-end-position input start-position))
                    (line-length (as (text/length input line-start-position line-end-position)))
                    (line-start-character-index (as (or (va line-start-character-index-in) (- (va line-end-character-index-in) (va line-length)))))
                    (line-end-character-index (as (or (va line-end-character-index-in) (+ (va line-start-character-index-in) (va line-length)))))
                    (line-height (line-height line-start-position line-end-position))
                    (previous-line-end-position (text/previous-position input line-start-position))
                    (next-line-start-position (text/next-position input line-end-position)))
               (make-computed-ll (as (print-line line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height))
                                 (as (or previous-element
                                         (when previous-line-end-position
                                           (print-containing-line previous-line-end-position (as nil) (as (1- (va line-start-character-index))) (- line-y line-height) nil -self-))))
                                 (as (or next-element
                                         (when next-line-start-position
                                           (print-containing-line next-line-start-position (as (1+ (va line-end-character-index))) (as nil) (+ line-y line-height) -self- nil))))))))
    (bind ((line-iomaps (as (bind ((origin-position (text/origin-position input))
                                   (origin-line-character-index (when origin-position
                                                                  (text/length input (text/line-start-position input origin-position) origin-position))))
                              (when origin-position
                                (print-containing-line origin-position (as (- origin-line-character-index)) (as nil) 0 nil nil)))))
           (output (as (make-graphics/canvas (list (make-graphics/canvas (as (print-selection-below (va line-iomaps))) (make-2d 0 0))
                                                   (make-graphics/canvas (as (map-ll (va line-iomaps) 'output-of)) (make-2d 0 0))
                                                   (make-graphics/canvas (as (print-selection-above (va line-iomaps))) (make-2d 0 0)))
                                             (make-2d 0 0)))))
      (make-iomap 'iomap/text/text->graphics/canvas/text
                  :projection projection :recursion recursion
                  :input input :input-reference input-reference :output output
                  :line-iomaps line-iomaps))))

;;;;;;
;;; Reader

(def function text/text->graphics/canvas/read-backward (command printer-iomap)
  (bind ((operation (operation-of command)))
    (awhen (typecase operation
             (operation/replace-selection
              (pattern-case (selection-of operation)
                (((the string (subseq (the string document) ?graphics-start-character-index ?graphics-end-character-index))
                  (the string (text-of (the graphics/text document)))
                  (the graphics/text (elt (the sequence document) ?graphics-element-index))
                  (the sequence (elements-of (the graphics/canvas document)))
                  (the graphics/canvas (elt (the sequence document) ?graphics-line-canvas-lindex))
                  (the sequence (elements-of (the graphics/canvas document)))
                  . ?rest)
                 (bind ((line-iomap (elt (line-iomaps-of printer-iomap) ?graphics-line-canvas-lindex))
                        (position (position ?graphics-element-index (graphics-element-indices-of line-iomap)))
                        (character-index (when position
                                           (+ ?graphics-start-character-index (line-start-character-index-of line-iomap) (elt (first-character-indices-of line-iomap) position)))))
                   (make-operation/replace-selection (input-of printer-iomap) `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
             (operation/describe
              (pattern-case (selection-of operation)
                (((the string (subseq (the string document) ?graphics-start-character-index ?graphics-end-character-index))
                  (the string (text-of (the graphics/text document)))
                  (the graphics/text (elt (the sequence document) ?graphics-element-index))
                  (the sequence (elements-of (the graphics/canvas document)))
                  . ?rest)
                 (bind ((character-index (+ ?graphics-start-character-index (elt (first-character-indices-of printer-iomap) (position ?graphics-element-index (graphics-element-indices-of printer-iomap))))))
                   (make-operation/describe `((the text/text (text/subseq (the text/text document) ,character-index ,(1+ character-index)))))))))
             (operation/show-context-sensitive-help
              operation))
      (make-command (gesture-of command) it
                    :domain (domain-of command)
                    :description (description-of command)))))

(def reader text/text->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap))
         (text-command (text/read-operation printer-input (gesture-of input)))
         (document-command (document/read-operation (gesture-of input)))
         (graphics-command (awhen (graphics/read-operation (output-of printer-iomap) (gesture-of input))
                             (text/text->graphics/canvas/read-backward it printer-iomap))))
    (merge-commands text-command document-command graphics-command input)))
