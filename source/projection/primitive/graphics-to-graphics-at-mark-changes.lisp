;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;; TODO: both animation and debug draw!

;;;;;;
;;; IO map

(def iomap iomap/graphics/canvas->graphics/canvas@mark-changes ()
  ((elements :type sequence)))

;;;;;;
;;; Projection

(def projection graphics/canvas->graphics/canvas@mark-changes ()
  ())

(def projection graphics/viewport->graphics/viewport@mark-changes ()
  ())

;;;;;;
;;; Construction

(def function make-projection/graphics/canvas->graphics/canvas@mark-changes ()
  (make-projection 'graphics/canvas->graphics/canvas@mark-changes))

(def function make-projection/graphics/viewport->graphics/viewport@mark-changes ()
  (make-projection 'graphics/viewport->graphics/viewport@mark-changes))

;;;;;;
;;; Construction

(def macro graphics/canvas->graphics/canvas@mark-changes ()
  '(make-projection/graphics/canvas->graphics/canvas@mark-changes))

(def macro graphics/viewport->graphics/viewport@mark-changes ()
  '(make-projection/graphics/viewport->graphics/viewport@mark-changes))

;;;;;;
;;; Printer

(def function froxxx (start-element)
  (if (typep start-element 'computed-ll)
      (iter (for element :initially start-element :then (hu.dwim.computed-class::cs-value (standard-instance-access element (slot-definition-location (find-slot (class-of element) 'hu.dwim.computed-class::next-element)))))
            (while element)
            (collect (hu.dwim.computed-class::cs-value (standard-instance-access element (slot-definition-location (find-slot (class-of element) 'hu.dwim.computed-class::value))))))
      start-element))

(def printer graphics/canvas->graphics/canvas@mark-changes (projection recursion input input-reference)
  (bind ((output (as (make-graphics/canvas (as* (:recomputation-mode :always)
                                             (bind ((old-elements (coerce (froxxx (hu.dwim.computed-class::cs-value (standard-instance-access input (slot-definition-location (find-slot (class-of input) 'elements))))) 'list))
                                                    (new-elements (coerce (elements-of input) 'list))
                                                    (added-elements (set-difference new-elements old-elements))
                                                    #+nil
                                                    (removed-elements (set-difference old-elements new-elements))
                                                    #+nil
                                                    (kept-elements (intersection old-elements new-elements)))
                                               (append (iter (for element :in new-elements)
                                                             (collect (output-of (recurse-printer recursion element nil))))
                                                       (iter (for element :in added-elements)
                                                             (for bounding-rectangle = (make-bounding-rectangle element))
                                                             (collect (make-graphics/rectangle (location-of bounding-rectangle) (size-of bounding-rectangle)
                                                                                               :stroke-color *color/solarized/red*)))
                                                       #+nil
                                                       (iter (for element :in removed-elements)
                                                             (for bounding-rectangle = (make-bounding-rectangle element))
                                                             (collect (make-graphics/rectangle (location-of bounding-rectangle) (size-of bounding-rectangle)
                                                                                               :stroke-color *color/solarized/blue*))))))
                                           (as (location-of input))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer graphics/viewport->graphics/viewport@mark-changes (projection recursion input input-reference)
  (bind ((output (as (make-graphics/viewport (as (output-of (recurse-printer recursion (content-of input) nil)))
                                             (as (location-of input))
                                             (as (size-of input))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader graphics/canvas->graphics/canvas@mark-changes (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader graphics/viewport->graphics/viewport@mark-changes (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
