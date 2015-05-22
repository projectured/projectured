;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection graphics/canvas->graphics/canvas@clip-changes ()
  ((debug :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/graphics/canvas->graphics/canvas@clip-changes (&key debug)
  (make-projection 'graphics/canvas->graphics/canvas@clip-changes :debug debug))

;;;;;;
;;; Construction

(def macro graphics/canvas->graphics/canvas@clip-changes (&key debug)
  `(make-projection/graphics/canvas->graphics/canvas@clip-changes :debug ,debug))

;;;;;;
;;; Printer

(def function froxxx (start-element)
  (if (typep start-element 'computed-ll)
      (iter (with class = (find-class 'computed-ll))
            (with next-element-location = (slot-definition-location (find-slot class 'next-element)))
            (with value-location = (slot-definition-location (find-slot class 'value)))
            (for element :initially start-element :then (cs-value (standard-instance-access element next-element-location)))
            (while element)
            (collect (cs-value (standard-instance-access element value-location))))
      start-element))

(def printer graphics/canvas->graphics/canvas@clip-changes (projection recursion input input-reference)
  (bind ((output (make-graphics/canvas (as (bind ((clipping-rectangle (or (labels ((rectangle-union* (rectangle-1 rectangle-2)
                                                                                     (if (and rectangle-1 rectangle-2)
                                                                                         (rectangle-union rectangle-1 rectangle-2)
                                                                                         (or rectangle-1 rectangle-2)))
                                                                                   (recurse (element)
                                                                                     (typecase element
                                                                                       (graphics/canvas
                                                                                        ;; TODO: lazy compare?!
                                                                                        (bind ((old-child-elements (coerce (froxxx (cs-value (standard-instance-access element (slot-definition-location (find-slot (class-of element) 'elements))))) 'list))
                                                                                               (new-child-elements (coerce (elements-of element) 'list))
                                                                                               (added-child-elements (set-difference new-child-elements old-child-elements))
                                                                                               (removed-child-elements (set-difference old-child-elements new-child-elements))
                                                                                               (change-rectangle nil)
                                                                                               #+nil
                                                                                               (kept-child-elements (intersection old-elements new-elements)))
                                                                                          (iter (for child-element :in new-child-elements)
                                                                                                (for index :from 0)
                                                                                                ;; KLUDGE: to skip the content of the main canvas (except for the first time)
                                                                                                ;; TODO: we could check the clipping so far, so not to go down outside
                                                                                                (when (or (not -current-value-)
                                                                                                          (not (= (length new-child-elements) 3))
                                                                                                          (not (= index 1)))
                                                                                                  (setf change-rectangle (rectangle-union* change-rectangle (recurse child-element)))))
                                                                                          (iter (for child-element :in added-child-elements)
                                                                                                (setf change-rectangle (rectangle-union* change-rectangle (bounds-of child-element))))
                                                                                          (iter (for child-element :in removed-child-elements)
                                                                                                (setf change-rectangle (rectangle-union* change-rectangle (bounds-of child-element))))
                                                                                          (when change-rectangle
                                                                                            (incf (location-of change-rectangle) (location-of element)))
                                                                                          change-rectangle))
                                                                                       (graphics/viewport
                                                                                        (bind ((change-rectangle (recurse (content-of element))))
                                                                                          (when change-rectangle
                                                                                            (incf (location-of change-rectangle) (location-of element)))
                                                                                          change-rectangle)))))
                                                                            (recurse input))
                                                                          (make-rectangle (make-2d 0 0) (make-2d 0 0))))
                                                  (clipped-content (make-graphics/viewport input (location-of clipping-rectangle) (size-of clipping-rectangle))))
                                             (append (list clipped-content)
                                                     (when (debug-p projection)
                                                       (list (make-graphics/rectangle (location-of clipping-rectangle) (size-of clipping-rectangle) :stroke-color *color/solarized/gray*))))))
                                       (make-2d 0 0))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader graphics/canvas->graphics/canvas@clip-changes (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
