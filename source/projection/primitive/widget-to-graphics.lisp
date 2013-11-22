;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection widget/tooltip->graphics/canvas ()
  ())

(def projection widget/composite->graphics/canvas ()
  ())

(def projection widget/tabbed-pane->graphics/canvas ()
  ())

(def projection widget/scroll-pane->graphics/canvas ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/widget/tooltip->graphics/canvas ()
  (make-projection 'widget/tooltip->graphics/canvas))

(def (function e) make-projection/widget/composite->graphics/canvas ()
  (make-projection 'widget/composite->graphics/canvas))

(def (function e) make-projection/widget/tabbed-pane->graphics/canvas ()
  (make-projection 'widget/tabbed-pane->graphics/canvas))

(def (function e) make-projection/widget/scroll-pane->graphics/canvas ()
  (make-projection 'widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Construction

(def (macro e) widget/tooltip->graphics/canvas ()
  '(make-projection/widget/tooltip->graphics/canvas))

(def (macro e) widget/composite->graphics/canvas ()
  '(make-projection/widget/composite->graphics/canvas))

(def (macro e) widget/tabbed-pane->graphics/canvas ()
  '(make-projection/widget/tabbed-pane->graphics/canvas))

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
             (margin (margin-of input))
             (output (make-graphics/canvas (list (make-graphics/rectangle (- (location-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                          (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                                  (+ (top-of margin) (bottom-of margin))))
                                                                          :fill-color *color/pastel-yellow*
                                                                          :stroke-color *color/black*)
                                                 content-output)
                                           (location-of input))))
        (make-iomap/compound projection recursion input input-reference output output-reference
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
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                element-iomaps))))

(def printer widget/tabbed-pane->graphics/canvas (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (selector-iomaps (iter (for index :from 0)
                                (for pairs :in-sequence (selector-element-pairs-of input))
                                (collect (recurse-printer recursion iomap (first pairs)
                                                          `(elt (the list (elt (the list (selector-element-pairs-of ,typed-input-reference)) ,index)) 0)
                                                          `(elt (the list (elements-of (the graphics/canvas (elt (the list (elements-of (the graphics/canvas ,output-reference))) 0)))) ,(1+ index))))))
         (content-iomap (recurse-printer recursion iomap (second (elt (selector-element-pairs-of input) (selected-index-of input)))
                                         `(elt (the list (elt (the list (selector-element-pairs-of ,typed-input-reference)) ,(selected-index-of input))) 1)
                                         `(elt (the list (elements-of (the graphics/canvas (elt (the list (elements-of (the graphics/canvas ,output-reference))) ,(1+ (length (selector-element-pairs-of input))))))) 0)))
         (output (make-graphics/canvas (iter (with x = 0)
                                             (for index :from 0)
                                             (for selector-iomap :in-sequence selector-iomaps)
                                             (for bounding-rectangle = (make-bounding-rectangle (output-of selector-iomap)))
                                             (maximizing (2d-y (size-of bounding-rectangle)) :into height)
                                             (collect (make-graphics/rectangle (+ (location-of bounding-rectangle) x)
                                                                               (size-of bounding-rectangle)
                                                                               :stroke-color *color/black*
                                                                               :fill-color (when (= index (selected-index-of input))
                                                                                             *color/solarized/background/light*))
                                               :into result)
                                             (collect (make-graphics/canvas (list (output-of selector-iomap)) (make-2d x 0)) :into result)
                                             (incf x (+ 5 (2d-x (size-of bounding-rectangle))))
                                             (finally
                                              (return (append result
                                                              (list (make-graphics/canvas (list (output-of content-iomap))
                                                                                          (make-2d 0 height)))))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               content-iomap))))

(def printer widget/scroll-pane->graphics/canvas (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (content-iomap (recurse-printer recursion iomap (content-of input) `(content-of ,typed-input-reference)
                                         `(elt (the list (elements-of (the graphics/canvas (content-of (the graphics/viewport (elt (the list (elements-of (the graphics/canvas ,output-reference))) 0)))))) 0)))
         (location (location-of input))
         (size (size-of input))
         (content (make-graphics/canvas (list (output-of content-iomap)) (as (bind ((margin (margin-of input)))
                                                                               (+ (scroll-position-of input) (make-2d (left-of margin) (right-of margin)))))))
         (output (make-graphics/canvas (list (make-graphics/viewport content location size)
                                             (make-graphics/rectangle location size :stroke-color *color/black*))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               content-iomap))))

;;;;;;
;;; Reader

(def reader widget/tooltip->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion document))
  (bind ((input (input-of projection-iomap))
         (latest-gesture (first (gestures-of gesture-queue))))
    (or (merge-operations (gesture-case latest-gesture
                            ((make-instance 'gesture/mouse/button/click :button :button-right :modifiers nil)
                             :domain "Widget" :help "Describes what is the mouse pointing at"
                             :operation (bind ((graphics-reference (make-reference (output-of printer-iomap) (location-of latest-gesture)
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
                                                                         (make-instance 'operation/widget/show :widget input))))))
                          operation)
        (cond ((typep latest-gesture 'gesture/mouse/move)
               (when (visible-p input)
                 (make-instance 'operation/widget/hide :widget input)))))))

(def reader widget/composite->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection))
  (bind ((operation
          (iter (for index :from 0)
                (for element :in-sequence (elements-of (input-of projection-iomap)))
                (thereis (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) (1+ index)) gesture-queue operation document))))
         ;; KLUDGE:
         (tooltip (elt (elements-of (input-of projection-iomap)) 1)))
    (cond ((typep operation 'operation/show-context-sensitive-help)
           (make-operation/compound (list (make-instance 'operation/widget/tooltip/replace-content :tooltip tooltip :content (make-text/text (mapcan 'make-gesture-help-text (operations-of operation))))
                                          (make-instance 'operation/widget/show :widget tooltip))))
          (t
           ;; KLUDGE:
           (if (visible-p tooltip)
               (make-operation/compound (optional-list operation (make-instance 'operation/widget/hide :widget tooltip)))
               operation)))))

(def reader widget/tabbed-pane->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (input (input-of projection-iomap))
         (child-operation (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation document)))
    (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press :sdl-key-tab :control)
                         :domain "Widget" :help "Selects the next page in the tabbed pane"
                         :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                   :tabbed-pane input
                                                   :selected-index (mod (1+ (selected-index-of input)) (length (selector-element-pairs-of input)))))
                        ((gesture/keyboard/key-press :sdl-key-tab '(:shift :control))
                         :domain "Widget" :help "Selects the previous page in the tabbed pane"
                         :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                   :tabbed-pane input
                                                   :selected-index (mod (1- (selected-index-of input)) (length (selector-element-pairs-of input))))))
                      child-operation)))

(def reader widget/scroll-pane->graphics/canvas (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (child-operation (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation document-iomap)))
    (merge-operations (gesture-case latest-gesture
                        ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers nil)
                         :domain "Widget" :help "Scrolls the content of the pane up"
                         :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                   :scroll-pane (input-of projection-iomap)
                                                   :scroll-delta (make-2d 0 100)))
                        ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers nil)
                         :domain "Widget" :help "Scrolls the content of the pane down"
                         :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                   :scroll-pane (input-of projection-iomap)
                                                   :scroll-delta (make-2d 0 -100)))
                        ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers '(:shift))
                         :domain "Widget" :help "Scrolls the content of the pane left"
                         :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                   :scroll-pane (input-of projection-iomap)
                                                   :scroll-delta (make-2d 100 0)))
                        ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers '(:shift))
                         :domain "Widget" :help "Scrolls the content of the pane right"
                         :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                   :scroll-pane (input-of projection-iomap)
                                                   :scroll-delta (make-2d -100 0))))
                      child-operation)))
