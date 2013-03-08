;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) widget/tooltip->graphics/canvas ()
  ())

(def (projection e) widget/composite->graphics/canvas ()
  ())

(def (projection e) widget/scroll-pane->graphics/canvas ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/widget/tooltip->graphics/canvas ()
  (make-projection 'widget/tooltip->graphics/canvas))

(def (function e) make-projection/widget/composite->graphics/canvas ()
  (make-projection 'widget/composite->graphics/canvas))

(def (function e) make-projection/widget/scroll-pane->graphics/canvas ()
  (make-projection 'widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Construction

(def (macro e) widget/tooltip->graphics/canvas ()
  '(make-projection/widget/tooltip->graphics/canvas))

(def (macro e) widget/composite->graphics/canvas ()
  '(make-projection/widget/composite->graphics/canvas))

(def (macro e) widget/scroll-pane->graphics/canvas ()
  '(make-projection/widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Printer

(def printer widget/tooltip->graphics/canvas (projection recursion iomap input input-reference output-reference)
  (if (visible-p input)
      (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
             (content (content-of input))
             (content-iomap (recurse-printer recursion iomap content
                                             `(the ,(form-type content) (content-of ,typed-input-reference))
                                             `(elt (the list (elements-of (the graphics/canvas ,output-reference))) 0)))
             (content-output (output-of content-iomap))
             (content-rectangle (make-bounding-rectangle content-output))
             (inset (make-2d 3 3))
             (output (make-graphics/canvas (list (make-graphics/rectangle (- (location-of content-rectangle) inset)
                                                                          (+ (size-of content-rectangle) (* 2 inset))
                                                                          :fill-color *color/yellow*
                                                                          :stroke-color *color/blue*)
                                                 content-output)
                                           (location-of input))))
        (make-iomap/recursive projection recursion input input-reference output output-reference
                              (list (make-iomap/object projection recursion input input-reference output output-reference)
                                    content-iomap)))
      (make-iomap/object projection recursion input input-reference (make-graphics/canvas nil (make-2d 0 0)) output-reference)))

(def printer widget/composite->graphics/canvas (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion iomap element
                                                         `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                         `(elt (the list (elements-of (the graphics/canvas ,output-reference))) ,index)))))
         (output (make-graphics/canvas (mapcar 'output-of element-iomaps)
                                       (make-2d 0 0))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 element-iomaps))))

(def printer widget/scroll-pane->graphics/canvas (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (content-iomap (recurse-printer recursion iomap (content-of input) `(content-of ,typed-input-reference)
                                         `(elt (the list (elements-of (the graphics/canvas (content-of (the graphics/viewport (elt (the list (elements-of (the graphics/canvas ,output-reference))) 0)))))) 0)))
         (location (location-of input))
         (size (size-of input))
         (content (make-graphics/canvas (list (output-of content-iomap)) (as (+ (scroll-position-of input) (make-2d 5 5)))))
         (output (make-graphics/canvas (list (make-graphics/viewport content location size)
                                             (make-graphics/rectangle location size :stroke-color *color/black*))
                                       (make-2d 0 0))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                content-iomap))))

;;;;;;
;;; Reader

(def reader widget/tooltip->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion operation document))
  (bind ((input (input-of projection-iomap))
         (latest-gesture (first (gestures-of gesture-queue))))
    (cond ((typep latest-gesture 'gesture/mouse/move)
           (when (visible-p input)
             (make-instance 'operation/widget/hide :widget input)))
          ((and (typep latest-gesture 'gesture/mouse/button/click)
                (eq (button-of latest-gesture) :button-right))
           #+nil
           (typep latest-gesture 'gesture/mouse/hover)
           (bind ((graphics-reference (make-reference (output-of printer-iomap) (location-of latest-gesture)
                                                      `(printer-output (the ,(form-type (input-of printer-iomap)) document)
                                                                       ,(projection-of printer-iomap)
                                                                       ,(recursion-of printer-iomap))))
                  (domain-reference nil))
             (map-backward printer-iomap graphics-reference
                           (lambda (iomap reference)
                             (declare (ignore iomap))
                             (setf domain-reference reference)))
             (make-operation/compound (list (make-instance 'operation/widget/tooltip/move
                                                           :tooltip input
                                                           :location (location-of latest-gesture))
                                            (make-instance 'operation/widget/tooltip/replace-content
                                                           :tooltip input
                                                           :content domain-reference)
                                            (make-instance 'operation/widget/show :widget input))))))))

(def reader widget/composite->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection))
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of projection-iomap)))
        (thereis (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) (1+ index)) gesture-queue operation document))))

(def reader widget/scroll-pane->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection))
  (bind ((latest-gesture (first (gestures-of gesture-queue))))
    (cond ((and (typep latest-gesture 'gesture/mouse/button/click)
                (member (button-of latest-gesture) '(:wheel-up :wheel-down)))
           (make-instance 'operation/widget/scroll-pane/scroll
                          :scroll-pane (input-of projection-iomap)
                          :scroll-delta (bind ((delta (ecase (button-of latest-gesture)
                                                    (:wheel-up 100)
                                                    (:wheel-down -100))))
                                          (if (member :shift (modifiers-of latest-gesture))
                                              (make-2d delta 0)
                                              (make-2d 0 delta)))))
          (t
           (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation document)))))
