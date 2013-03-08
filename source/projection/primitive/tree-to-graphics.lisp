;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) tree->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/tree->graphics ()
  (make-projection 'tree->graphics))

;;;;;;
;;; Construction

(def (macro e) tree->graphics ()
  `(make-projection/tree->graphics))

;;;;;;
;;; Printer

(def printer tree->graphics (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil))
    (labels ((recurse (input location depth)
               (etypecase input
                 (tree/node
                  (bind ((y (* 100 depth)))
                    (make-graphics/canvas (list* (make-graphics/circle (make-2d 0 y) 16 :stroke-color *color/black* :fill-color *color/light-cyan*)
                                                 (iter (with location = (make-2d 0 y))
                                                       (for child :in (children-of input))
                                                       (for canvas-element = (recurse child (+ location (make-2d 0 50)) (1+ depth)))
                                                       (for bounding-rectangle = (make-bounding-rectangle canvas-element))
                                                       (incf location (+ (2d-x (size-of bounding-rectangle)) 10))
                                                       (collect canvas-element)))
                                          location)))
                 (tree/leaf
                  (recurse (content-of input) location depth))
                 (string
                  (bind ((inset 5)
                         (text (make-graphics/text (make-2d inset inset) input
                                                   :font *font/default*
                                                   :font-color *color/default*
                                                   :fill-color nil))
                         (rectangle (make-graphics/rectangle (make-2d 0 0) (+ (measure-text (text-of text) (font-of text)) (* 2 (make-2d inset inset)))
                                                             :stroke-color *color/black*
                                                             :fill-color *color/light-cyan*)))
                    (make-graphics/canvas (list rectangle text) location))))))
      (bind ((output (recurse input (make-2d 0 0) 0)))
        (make-iomap/recursive projection recursion input input-reference output output-reference
                              (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))))

;;;;;;
;;; Reader

(def reader tree->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
