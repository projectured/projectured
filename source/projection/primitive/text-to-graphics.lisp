;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/text->graphics/line ()
  ((line-start-character-index :type integer)
   (line-end-character-index :type integer)
   (first-character-indices :type sequence)
   (last-character-indices :type sequence)
   (graphics-element-indices :type sequence)
   (line-y :type number)))

(def iomap iomap/text->graphics/text ()
  ((line-iomaps :type sequence)))

;;;;;;
;;; Projection

;; TODO: rename text/text->graphics/canvas
(def projection text->graphics ()
  ())

;;;;;;
;;; Construction

(def function make-projection/text->graphics ()
  (make-projection 'text->graphics))

;;;;;;
;;; Construction

(def macro text->graphics ()
  `(make-projection/text->graphics))

;;;;;;
;;; Printer

(def printer text->graphics (projection recursion input input-reference)
  (labels ((line-height (line-start-position line-end-position)
             (or (iter (for position :initially line-start-position :then (text/next-position input position))
                       (until (text/position= position line-end-position))
                       (for text-element = (text/element input (text/position-element position)))
                       (etypecase text-element
                         (text/spacing)
                         (text/character
                          (maximize (2d-y (measure-text " " (font-of text-element)))))
                         (text/string
                          (maximize (2d-y (measure-text " " (font-of text-element)))))
                         (image/file
                          (bind ((image (make-graphics/image (make-2d 0 0) text-element))
                                 (size (size-of (make-bounding-rectangle image))))
                            (maximize (2d-y size))))))
                 (2d-y (measure-text " " (font-of (text/element input (text/position-element line-start-position)))))))
           (graphics-character-index (line-iomap character-index)
             (iter (for index :from 0)
                   (for first-character-index :in (first-character-indices-of line-iomap))
                   (for last-character-index :in (last-character-indices-of line-iomap))
                   (when (<= first-character-index character-index last-character-index)
                     (return (values (elt (graphics-element-indices-of line-iomap) index) (- character-index first-character-index))))))
           (print-selection (line-iomaps)
             (pattern-case (selection-of input)
               (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                (iter (for line-iomap-element :initially line-iomaps :then (if (< ?character-index 0)
                                                                               (previous-element-of line-iomap-element)
                                                                               (next-element-of line-iomap-element)))
                      (for line-iomap = (value-of line-iomap-element))
                      (for line-start-character-index = (line-start-character-index-of line-iomap))
                      (for line-end-character-index = (line-end-character-index-of line-iomap))
                      (when (<= line-start-character-index ?character-index line-end-character-index)
                        (bind (((:values graphics-element-index graphics-character-index) (graphics-character-index line-iomap (- ?character-index line-start-character-index)))
                               (graphics-elements (elements-of (output-of line-iomap)))
                               (text-graphics (elt graphics-elements graphics-element-index))
                               (text (subseq (text-of text-graphics) 0 graphics-character-index))
                               (location (+ (location-of text-graphics) (make-2d 0 (line-y-of line-iomap))))
                               (font (font-of text-graphics))
                               (size (measure-text text font))
                               (offset (2d-x size))
                               (height (2d-y size)))
                          (return (list (make-graphics/line (+ location (make-2d offset 0))
                                                            (+ location (make-2d offset height))
                                                            :stroke-color *color/black*)
                                        (make-graphics/line (+ location (make-2d (- offset 2) 0))
                                                            (+ location (make-2d (+ offset 2) 0))
                                                            :stroke-color *color/black*)
                                        (make-graphics/line (+ location (make-2d (- offset 2) height))
                                                            (+ location (make-2d (+ offset 2) height))
                                                            :stroke-color *color/black*)))))))
               #+nil
               (((the text/text (text/subbox (the text/text document) ?b ?c)))
                (iter (with top = nil)
                      (with left = nil)
                      #+nil(with right = nil)
                      (for character-index :from ?b :to ?c)
                      (for (values graphics-element-index graphics-character-index) = (graphics-character-index character-index))
                      (when graphics-character-index
                        (for graphics-element = (elt elements graphics-element-index))
                        (typecase graphics-element
                          (graphics/text
                           (bind ((text (subseq (text-of graphics-element) 0 graphics-character-index))
                                  (location (location-of graphics-element))
                                  (font (font-of graphics-element))
                                  (size (measure-text text font))
                                  (offset (2d-x size))
                                  (height (2d-y size)))
                             (unless left
                               (setf left (+ (2d-x location) offset)))
                             ;; TODO: support rectangular
                             #+nil(setf right (+ (2d-x location) offset))
                             ;; TODO: to support tree
                             (maximizing (+ (2d-x location) offset) :into right)
                             (unless top
                               (setf top (2d-y location)))
                             (maximizing (+ (2d-y location) height) :into bottom)))))
                      (finally
                       (return (list (make-graphics/rectangle (make-2d left top)
                                                              (make-2d (- right left) (- bottom top))
                                                              :fill-color *color/solarized/background/light*))))))))
           (print-line (line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height)
             (bind ((first-character-indices nil)
                    (last-character-indices nil)
                    (graphics-element-indices nil)
                    (graphics-elements (iter (with x = 0)
                                             (with graphics-element-index = 0)
                                             (with character-index = 0)
                                             (for position :initially line-start-position :then (text/next-position input position))
                                             (until (text/position= position line-end-position))
                                             (for text-element = (text/element input (text/position-element position)))
                                             (for previous-text-element :previous text-element)
                                             (unless (eq text-element previous-text-element)
                                               (for content = (content-of text-element))
                                               (for size = (measure-text content (font-of text-element)))
                                               (for text = (make-graphics/text (make-2d x (- line-height (2d-y size))) content
                                                                               :font (font-of text-element)
                                                                               :font-color (font-color-of text-element)
                                                                               :fill-color (fill-color-of text-element)))
                                               (awhen (line-color-of text-element)
                                                 (collect (make-graphics/rectangle (make-2d x 0) (make-2d (2d-x size) line-height) :fill-color it)))
                                               (push graphics-element-index graphics-element-indices)
                                               (incf graphics-element-index)
                                               (push character-index first-character-indices)
                                               (incf character-index (length content))
                                               (push character-index last-character-indices)
                                               (collect text)
                                               (incf x (2d-x size))))))
               (make-iomap 'iomap/text->graphics/line
                           :projection projection :recursion recursion
                           :input input :output (make-graphics/canvas graphics-elements (make-2d 0 line-y))
                           :line-start-character-index line-start-character-index :line-end-character-index line-end-character-index
                           :first-character-indices first-character-indices :last-character-indices last-character-indices
                           :graphics-element-indices graphics-element-indices :line-y line-y)))
           (print-containing-line (start-position line-start-character-index line-end-character-index line-y)
             (bind ((line-start-position (text/line-start-position input start-position))
                    (line-end-position (text/line-end-position input start-position))
                    (line-length (text/length input line-start-position line-end-position))
                    (line-start-character-index (or line-start-character-index (- line-end-character-index line-length)))
                    (line-end-character-index (or line-end-character-index (+ line-start-character-index line-length)))
                    (line-height (line-height line-start-position line-end-position))
                    (line-iomap (print-line line-start-position line-end-position line-start-character-index line-end-character-index line-y line-height)))
               (make-computed-ll (as line-iomap)
                                 (as (awhen (text/previous-position input line-start-position)
                                       (print-containing-line it nil (1- line-start-character-index) (- line-y line-height))))
                                 (as (awhen (text/next-position input line-end-position)
                                       (awhen (text/next-position input it)
                                         (print-containing-line it (1+ line-end-character-index) nil (+ line-y line-height)))))))))
    (bind ((origin-position (text/origin-position input))
           (origin-line-character-index (text/length input (text/line-start-position input origin-position) origin-position))
           (line-iomaps (as (print-containing-line origin-position (- origin-line-character-index) nil 0)))
           (output (as (make-graphics/canvas (list (make-graphics/canvas (as (map-ll (va line-iomaps) 'output-of)) (make-2d 0 0))
                                                   (make-graphics/canvas (as (print-selection (va line-iomaps))) (make-2d 0 0)))
                                             (make-2d 0 0)))))
      (make-iomap 'iomap/text->graphics/text
                  :projection projection :recursion recursion
                  :input input :input-reference input-reference :output output
                  :line-iomaps line-iomaps))))

;;;;;;
;;; Reader

(def function text->graphics/read-backward (command printer-iomap)
  (bind ((operation (operation-of command)))
    (awhen (typecase operation
             (operation/replace-selection
              (pattern-case (selection-of operation)
                (((the string (subseq (the string document) ?graphics-start-character-index ?graphics-end-character-index))
                  (the string (text-of (the graphics/text document)))
                  (the graphics/text (elt (the sequence document) ?graphics-element-index))
                  (the sequence (elements-of (the graphics/canvas document)))
                  . ?rest)
                 (bind ((character-index (+ ?graphics-start-character-index (elt (first-character-indices-of printer-iomap) (position ?graphics-element-index (graphics-element-indices-of printer-iomap))))))
                   (make-operation/replace-selection (input-of printer-iomap) `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
             (operation/describe
              (pattern-case (target-of operation)
                (((the string (subseq (the string document) ?graphics-start-character-index ?graphics-end-character-index))
                  (the string (text-of (the graphics/text document)))
                  (the graphics/text (elt (the sequence document) ?graphics-element-index))
                  (the sequence (elements-of (the graphics/canvas document)))
                  . ?rest)
                 (bind ((character-index (+ ?graphics-start-character-index (elt (first-character-indices-of printer-iomap) (position ?graphics-element-index (graphics-element-indices-of printer-iomap))))))
                   (make-instance 'operation/describe :target `((the text/text (text/subseq (the text/text document) ,character-index ,(1+ character-index)))))))))
             (operation/show-context-sensitive-help
              operation))
      (make-command (gesture-of command) it
                    :domain (domain-of command)
                    :description (description-of command)))))

(def reader text->graphics (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap))
         (text-command (text/read-operation printer-input (gesture-of input)))
         (document-command (document/read-operation (gesture-of input)))
         (graphics-command (awhen (graphics/read-operation (output-of printer-iomap) (gesture-of input))
                             (text->graphics/read-backward it printer-iomap))))
    (merge-commands text-command document-command graphics-command input)))
