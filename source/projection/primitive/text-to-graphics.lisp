;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/text->graphics (iomap)
  ((graphics-element-indices :type sequence)
   (first-character-indicies :type sequence)
   (last-character-indicies :type sequence)))

;;;;;;
;;; Projection

(def projection text->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text->graphics ()
  (make-projection 'text->graphics))

;;;;;;
;;; Construction

(def (macro e) text->graphics ()
  `(make-projection/text->graphics))

;;;;;;
;;; Printer

(def printer text->graphics (projection recursion input input-reference)
  (declare (ignore input-reference))
  (bind ((graphics-element-indices nil)
         (first-character-indicies nil)
         (last-character-indicies nil)
         (line-heights (iter outer
                             (text/map-split input #\NewLine
                                             (lambda (start-element-index start-character-index end-element-index end-character-index)
                                               (declare (ignore start-character-index end-character-index))
                                               (in outer (collect (iter (for index :from start-element-index :below end-element-index)
                                                                        (for element = (elt (elements-of input) index))
                                                                        (etypecase element
                                                                          (text/spacing)
                                                                          (text/character
                                                                           (maximize (2d-y (measure-text " " (font-of element)))))
                                                                          (text/string
                                                                           (maximize (2d-y (measure-text " " (font-of element)))))
                                                                          (image/image
                                                                           ;; TODO:
                                                                           )))))))
                             (until #t)))
         (elements (iter outer
                         (with y = 0)
                         (with x = 0)
                         (with line-index = 0)
                         (with output-index = 0)
                         (with content-index = 0)
                         (for input-index :from 0)
                         (for element :in-sequence (elements-of input))
                         (etypecase element
                           (text/spacing
                            (incf x (ecase (unit-of element)
                                      (:pixel (size-of element))
                                      (:space (* (size-of element)
                                                 (2d-x (measure-text " " (font-of element))))))))
                           (text/character
                            (not-yet-implemented))
                           (text/string
                            (bind ((content (content-of element)))
                              (iter (for line :in (split-sequence #\NewLine content))
                                    (unless (first-iteration-p)
                                      (setf x 0)
                                      (incf y (elt line-heights line-index))
                                      (incf line-index))
                                    (unless (zerop (length line))
                                      (for line-height = (elt line-heights line-index))
                                      (for size = (measure-text line (font-of element)))
                                      (for text = (make-graphics/text (make-2d x (+ y (- line-height (2d-y size)))) line
                                                                      :font (font-of element)
                                                                      :font-color (font-color-of element)
                                                                      :fill-color (fill-color-of element)))
                                      (awhen (line-color-of element)
                                        (incf output-index)
                                        (in outer (collect (make-graphics/rectangle (make-2d x y) (make-2d (2d-x size) line-height) :fill-color it))))
                                      (in outer (collect text))
                                      (incf x (2d-x size))
                                      (push output-index graphics-element-indices)
                                      (incf output-index)
                                      (push content-index first-character-indicies)
                                      (incf content-index (length line))
                                      (push content-index last-character-indicies))
                                    (unless (first-iteration-p)
                                      (incf content-index)))))
                           ;; TODO: recurse and use the resulting graphics
                           (image/image
                            (bind ((image (make-graphics/image (make-2d x y) element))
                                   (size (size-of (make-bounding-rectangle image))))
                              (in outer (collect image))
                              (incf x (2d-x size)))))))
         (selection-elements (labels ((graphics-character-index (text-character-index)
                                        (iter (for index :from 0)
                                              (for first-character-index :in (reverse first-character-indicies))
                                              (for last-character-index :in (reverse last-character-indicies))
                                              (when (<= first-character-index text-character-index last-character-index)
                                                (return (values (elt (reverse graphics-element-indices) index) (- text-character-index first-character-index)))))))
                               (pattern-case (selection-of input)
                                 (((the character (text/elt (the text/text document) ?text-character-index)))
                                  (bind (((:values graphics-element-index graphics-character-index) (graphics-character-index ?text-character-index))
                                         (text-graphics (elt elements graphics-element-index))
                                         (offset-text (subseq (text-of text-graphics) 0 graphics-character-index))
                                         (text (subseq (text-of text-graphics) graphics-character-index (1+ graphics-character-index)))
                                         (location (location-of text-graphics))
                                         (font (font-of text-graphics))
                                         (offset (2d-x (measure-text offset-text font))))
                                    (list (make-graphics/rectangle (+ location (make-2d offset 0)) (measure-text text font)
                                                                   :fill-color *color/solarized/background/light*))))
                                 (((the sequence-position (text/pos (the text/text document) ?text-character-index)))
                                  (bind (((:values graphics-element-index graphics-character-index) (graphics-character-index ?text-character-index))
                                         (text-graphics (elt elements graphics-element-index))
                                         (text (subseq (text-of text-graphics) 0 graphics-character-index))
                                         (location (location-of text-graphics))
                                         (font (font-of text-graphics))
                                         (offset (2d-x (measure-text text font)))
                                         (height (2d-y (measure-text "" font))))
                                    (list (make-graphics/line (+ location (make-2d offset 0))
                                                              (+ location (make-2d offset height))
                                                              :stroke-color *color/black*)))))))
         (output (make-graphics/canvas (append elements selection-elements) (make-2d 0 0))))
    (make-iomap 'iomap/text->graphics
                :projection projection :recursion recursion
                :input input :output output
                :graphics-element-indices (nreverse graphics-element-indices)
                :first-character-indicies (nreverse first-character-indicies)
                :last-character-indicies (nreverse last-character-indicies))))

;;;;;;
;;; Reader

(def reader text->graphics (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion operation))
  (bind ((input (input-of projection-iomap))
         (latest-gesture (first (gestures-of gesture-queue)))
         (text-operation (text/read-operation input latest-gesture))
         (document-operation (document/read-operation input latest-gesture))
         (graphics-operation (graphics/read-operation (output-of projection-iomap) latest-gesture)))
    (merge-operations (merge-operations text-operation document-operation)
                      (typecase graphics-operation
                        (operation/replace-selection
                         (pattern-case (selection-of graphics-operation)
                           (((the character (elt (the string document) ?graphics-character-index))
                             (the string (text-of (the graphics/text document)))
                             (the graphics/text (elt (the list document) ?graphics-element-index))
                             (the list (elements-of (the graphics/canvas document))))
                            (bind ((text-character-index (+ ?graphics-character-index (elt (first-character-indicies-of projection-iomap) (position ?graphics-element-index (graphics-element-indices-of projection-iomap))))))
                              (make-operation/replace-selection input `((the sequence-position (text/pos (the text/text document) ,text-character-index))))))))
                        (operation/describe
                         (pattern-case (target-of graphics-operation)
                           (((the character (elt (the string document) ?graphics-character-index))
                             (the string (text-of (the graphics/text document)))
                             (the graphics/text (elt (the list document) ?graphics-element-index))
                             (the list (elements-of (the graphics/canvas document))))
                            (bind ((text-character-index (+ ?graphics-character-index (elt (first-character-indicies-of projection-iomap) (position ?graphics-element-index (graphics-element-indices-of projection-iomap))))))
                              (make-instance 'operation/describe :target `((the character (text/elt (the text/text document) ,text-character-index))))))))))))
