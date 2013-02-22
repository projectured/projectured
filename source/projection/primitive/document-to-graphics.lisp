;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) document->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/document->graphics ()
  (make-projection 'document->graphics))

;;;;;;
;;; Construction

(def (macro e) document->graphics ()
  '(make-projection/document->graphics))

;;;;;;
;;; Printer

(def printer document->graphics (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (iomap-cs (as (recurse-printer recursion iomap (content-of input) `(content-of ,typed-input-reference) `(elt (the list (elements-of (the graphics/canvas ,output-reference))) 0))))
         (iomap (computed-state-value iomap-cs))
         (output-content (output-of iomap))
         (input-selection (selection-of input)))
    (pattern-case input-selection
      ((?or (the character (elt (the string ?a) ?b))
            (the sequence-position (pos (the string ?a) ?b)))
       (bind ((selection-graphics-cs
               (as (bind ((graphics-reference nil))
                     (map-forward iomap (tree-replace (selection-of input) '(the document document) typed-input-reference)
                                  (lambda (iomap reference)
                                    (declare (ignore iomap))
                                    (setf graphics-reference reference)))
                     (pattern-case graphics-reference
                       ((the character (elt (the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ?a))) ?b)))) ?c))
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (offset-text (subseq (text-of text-graphics) 0 ?c))
                               (text (subseq (text-of text-graphics) ?c (1+ ?c)))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text offset-text font))))
                          (make-graphics/rectangle (+ location (make-2d offset 0)) (measure-text text font))))
                       ((the sequence-position (pos (the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ?a))) ?b)))) ?c))
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (text (subseq (text-of text-graphics) 0 ?c))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text text font)))
                               (height (2d-y (measure-text "" font))))
                          (make-graphics/line (+ location (make-2d offset 0))
                                              (+ location (make-2d offset height))
                                              :stroke-color *color/black*)))))))
              (output (make-graphics/canvas (as (optional-list (output-of (computed-state-value iomap-cs))
                                                               (computed-state-value selection-graphics-cs)))
                                            (make-2d 0 0))))
         (make-iomap/recursive projection recursion input input-reference output output-reference
                               (list (make-iomap/object projection recursion input input-reference output output-reference) iomap))))
      ((the tree/node ?a)
       (not-yet-implemented)
       (bind ((opening-delimiter-graphics-reference nil)
              (closing-delimiter-graphics-reference nil))
         (map-forward iomap (tree-replace `(the character (elt (the string (opening-delimiter ,input-selection "(")) 0))
                                          '(the document document)
                                          typed-input-reference)
                      (lambda (iomap reference)
                        (declare (ignore iomap))
                        (setf opening-delimiter-graphics-reference reference)))
         (map-forward iomap (tree-replace `(the character (elt (the string (closing-delimiter ,input-selection ")")) 0))
                                          '(the document document)
                                          typed-input-reference)
                      (lambda (iomap reference)
                        (declare (ignore iomap))
                        (setf closing-delimiter-graphics-reference reference)))
         (bind ((opening-delimiter-location (apply-reference iomap opening-delimiter-graphics-reference 'print))
                (closing-delimiter-location (apply-reference iomap closing-delimiter-graphics-reference 'print))
                (selection-graphics (make-graphics/rectangle opening-delimiter-location
                                                             (- closing-delimiter-location opening-delimiter-location)
                                                             :fill-color *color/light-cyan*))
                (output (make-graphics/canvas (optional-list selection-graphics output-content) (make-2d 0 0))))
           (make-iomap/recursive projection recursion input input-reference output output-reference
                                 (list (make-iomap/object projection recursion input input-reference output output-reference) iomap)))))
      (?a
       (make-iomap/object projection recursion input input-reference (as (output-of (computed-state-value iomap-cs))) output-reference)))))

;;;;;;
;;; Reader

(def reader document->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection document))
  (bind ((latest-gesture (first (gestures-of gesture-queue))))
    (cond ((typep latest-gesture 'gesture/window/quit)
           (make-operation/quit))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-escape))
           (make-operation/quit))
          (t
           (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation (input-of projection-iomap))))))
