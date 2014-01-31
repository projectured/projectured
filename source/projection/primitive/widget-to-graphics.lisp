;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection widget/label->graphics/canvas ()
  ())

(def projection widget/tooltip->graphics/canvas ()
  ())

(def projection widget/menu->graphics/canvas ()
  ())

(def projection widget/menu-item->graphics/canvas ()
  ())

(def projection widget/shell->graphics/canvas ()
  ())

(def projection widget/composite->graphics/canvas ()
  ())

(def projection widget/split-pane->graphics/canvas ()
  ())

(def projection widget/tabbed-pane->graphics/canvas ()
  ())

(def projection widget/scroll-pane->graphics/canvas ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/widget/label->graphics/canvas ()
  (make-projection 'widget/label->graphics/canvas))

(def (function e) make-projection/widget/tooltip->graphics/canvas ()
  (make-projection 'widget/tooltip->graphics/canvas))

(def (function e) make-projection/widget/menu->graphics/canvas ()
  (make-projection 'widget/menu->graphics/canvas))

(def (function e) make-projection/widget/menu-item->graphics/canvas ()
  (make-projection 'widget/menu-item->graphics/canvas))

(def (function e) make-projection/widget/shell->graphics/canvas ()
  (make-projection 'widget/shell->graphics/canvas))

(def (function e) make-projection/widget/composite->graphics/canvas ()
  (make-projection 'widget/composite->graphics/canvas))

(def (function e) make-projection/widget/split-pane->graphics/canvas ()
  (make-projection 'widget/split-pane->graphics/canvas))

(def (function e) make-projection/widget/tabbed-pane->graphics/canvas ()
  (make-projection 'widget/tabbed-pane->graphics/canvas))

(def (function e) make-projection/widget/scroll-pane->graphics/canvas ()
  (make-projection 'widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Construction

(def (macro e) widget/label->graphics/canvas ()
  '(make-projection/widget/label->graphics/canvas))

(def (macro e) widget/tooltip->graphics/canvas ()
  '(make-projection/widget/tooltip->graphics/canvas))

(def (macro e) widget/menu->graphics/canvas ()
  '(make-projection/widget/menu->graphics/canvas))

(def (macro e) widget/menu-item->graphics/canvas ()
  '(make-projection/widget/menu-item->graphics/canvas))

(def (macro e) widget/shell->graphics/canvas ()
  '(make-projection/widget/shell->graphics/canvas))

(def (macro e) widget/composite->graphics/canvas ()
  '(make-projection/widget/composite->graphics/canvas))

(def (macro e) widget/split-pane->graphics/canvas ()
  '(make-projection/widget/split-pane->graphics/canvas))

(def (macro e) widget/tabbed-pane->graphics/canvas ()
  '(make-projection/widget/tabbed-pane->graphics/canvas))

(def (macro e) widget/scroll-pane->graphics/canvas ()
  '(make-projection/widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Printer

(def printer widget/label->graphics/canvas (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (content (content-of input))
         (content-iomap (recurse-printer recursion content
                                         `((content-of (the widget/label document))
                                           ,@(typed-reference (form-type input) input-reference))))
         (content-output (output-of content-iomap))
         (content-rectangle (make-bounding-rectangle content-output))
         (margin (margin-of input))
         (output (make-graphics/canvas (list (make-graphics/rectangle (- (location-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                      (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                              (+ (top-of margin) (bottom-of margin))))
                                                                      :stroke-color *color/black*)
                                             content-output)
                                       (location-of input))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output nil)
                               content-iomap))))

(def printer widget/tooltip->graphics/canvas (projection recursion input input-reference)
  (if (visible-p input)
      (bind ((content (content-of input))
             (content-iomap (recurse-printer recursion content
                                             `((content-of (the widget/tooltip document))
                                               ,@(typed-reference (form-type input) input-reference))))
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
        (make-iomap/compound projection recursion input input-reference output
                             (list (make-iomap/object projection recursion input input-reference output nil)
                                   content-iomap)))
      (make-iomap/object projection recursion input input-reference (make-graphics/canvas nil (make-2d 0 0)) nil)))

(def printer widget/menu->graphics/canvas (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/menu document)))
                                                           ,@(typed-reference (form-type input) input-reference))))))
         (output (make-graphics/canvas (iter (with y = 0)
                                             (for index :from 0)
                                             (for element-iomap :in-sequence element-iomaps)
                                             (for bounding-rectangle = (make-bounding-rectangle (output-of element-iomap)))
                                             (maximizing (2d-x (size-of bounding-rectangle)) :into width)
                                             (collect (make-graphics/canvas (list (output-of element-iomap)) (make-2d 0 y)) :into result)
                                             (incf y (2d-y (size-of bounding-rectangle)))
                                             (finally (return (list* (make-graphics/rectangle (make-2d 0 0) (make-2d width y)
                                                                                              :stroke-color *color/solarized/content/darker*
                                                                                              :fill-color *color/solarized/background/lighter*)
                                                                     result))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output nil)))))

(def printer widget/menu-item->graphics/canvas (projection recursion input input-reference)
  (if (visible-p input)
      (bind ((content (content-of input))
             (content-iomap (recurse-printer recursion content
                                             `((content-of (the widget/menu-item document))
                                               ,@(typed-reference (form-type input) input-reference))))
             (content-output (output-of content-iomap))
             (content-rectangle (make-bounding-rectangle content-output))
             (margin (margin-of input))
             (output (make-graphics/canvas (list (make-graphics/rectangle (- (location-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                          (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                                  (+ (top-of margin) (bottom-of margin)))))
                                                 content-output)
                                           (make-2d (left-of margin) (top-of margin)))))
        (make-iomap/compound projection recursion input input-reference output
                             (list (make-iomap/object projection recursion input input-reference output nil)
                                   content-iomap)))))

(def printer widget/shell->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input)
                                         `((content-of (the widget/shell document))
                                           ,@(typed-reference (form-type input) input-reference))))
         (tooltip-iomap (recurse-printer recursion (tooltip-of input)
                                         `((tooltip-of (the widget/shell document))
                                           ,@(typed-reference (form-type input) input-reference))))
         (output (make-graphics/canvas (list (output-of content-iomap)
                                             (output-of tooltip-iomap))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

(def printer widget/composite->graphics/canvas (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/composite document)))
                                                           ,@(typed-reference (form-type input) input-reference))))))
         (output (make-graphics/canvas (mapcar 'output-of element-iomaps)
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output nil)
                                element-iomaps))))

(def printer widget/split-pane->graphics/canvas (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/split-pane document)))
                                                           ,@(typed-reference (form-type input) input-reference))))))
         (sizes (iter (for element-iomap :in element-iomaps)
                      (collect (size-of (make-bounding-rectangle (output-of element-iomap))))))
         (height (apply 'max (mapcar '2d-y sizes)))
         (output (make-graphics/canvas (iter (with x = 0)
                                             (for element-iomap :in element-iomaps)
                                             (unless (first-iteration-p)
                                               (bind ((splitter-width 5))
                                                 (collect (make-graphics/rectangle (make-2d (+ x 1) 0) (make-2d (- splitter-width 2) height) :fill-color *color/solarized/background/dark*))
                                                 (incf x splitter-width)))
                                             (collect (make-graphics/canvas (list (output-of element-iomap)) (make-2d x 0)))
                                             (incf x (2d-x (size-of (make-bounding-rectangle (output-of element-iomap))))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output nil) element-iomaps))))

(def printer widget/tabbed-pane->graphics/canvas (projection recursion input input-reference)
  (bind ((selector-iomaps (iter (for index :from 0)
                                (for pairs :in-sequence (selector-element-pairs-of input))
                                (collect (recurse-printer recursion (first pairs)
                                                          `((elt (the sequence document) 0)
                                                            (the sequence (elt (the sequence document) ,index))
                                                            (the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                                                            ,@(typed-reference (form-type input) input-reference))))))
         (content-iomap (recurse-printer recursion (second (elt (selector-element-pairs-of input) (selected-index-of input)))
                                         `((elt (the sequence document) 1)
                                           (the sequence (elt (the sequence document) ,(selected-index-of input)))
                                           (the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                                           ,@(typed-reference (form-type input) input-reference))))
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
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output nil)
                               content-iomap))))

(def printer widget/scroll-pane->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input)
                                         `((content-of (the widget/scroll-pane document))
                                           ,@(typed-reference (form-type input) input-reference))))
         (location (or (location-of input) (make-2d 0 0)))
         (size (size-of input))
         (content (make-graphics/canvas (list (output-of content-iomap))
                                        (as (bind ((margin (margin-of input)))
                                              (+ (scroll-position-of input) (if margin
                                                                                (make-2d (left-of margin) (right-of margin))
                                                                                (make-2d 0 0)))))))
         (output (make-graphics/canvas (list (make-graphics/viewport content location size)
                                             (make-graphics/rectangle location size :stroke-color *color/black*))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output nil)
                               content-iomap))))

;;;;;;
;;; Reader

(def reader widget/label->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))

(def reader widget/tooltip->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (if (and (typep input 'gesture/mouse/move)
             (visible-p printer-input))
        (make-instance 'operation/widget/hide :widget printer-input)
        input)))

(def reader widget/menu->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (document/read-operation (input-of printer-iomap) input))

(def reader widget/menu-item->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (document/read-operation (input-of printer-iomap) input))

(def reader widget/shell->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (content-operation (operation-of (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0))))
         (tooltip (tooltip-of printer-input)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/show-context-sensitive-help
                                (make-operation/compound (list (make-instance 'operation/widget/tooltip/move :tooltip tooltip :location (mouse-position))
                                                               (make-instance 'operation/widget/tooltip/replace-content :tooltip tooltip :content (make-text/text (mapcan 'make-command-help-text (commands-of operation))))
                                                               (make-instance 'operation/widget/show :widget tooltip))))
                               (operation/describe
                                (make-operation/compound (list (make-instance 'operation/widget/tooltip/move :tooltip tooltip :location (mouse-position))
                                                               (make-instance 'operation/widget/tooltip/replace-content :tooltip tooltip :content (target-of operation))
                                                               (make-instance 'operation/widget/show :widget tooltip))))
                               (operation/compound
                                (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null child-operations)
                                    (make-operation/compound child-operations))))
                               (t
                                (if (visible-p tooltip)
                                    (make-operation/compound (optional-list operation (make-instance 'operation/widget/hide :widget tooltip)))
                                    operation)))))
                    (recurse content-operation)))))

(def reader widget/composite->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of printer-iomap)))
        (thereis (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) (1+ index))))))

(def reader widget/split-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of printer-iomap)))
        (thereis (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) (1+ index))))))

(def reader widget/tabbed-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (child-operation (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 1))))
    (merge-commands (gesture-case (gesture-of input)
                      #+nil
                      ((make-instance 'gesture/mouse/button/click :button :button-left :modifiers nil)
                       :domain "Widget" :help "Selects the page in the tabbed pane where the mouse is pointing at"
                       :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                 :tabbed-pane printer-input
                                                 :selected-index 1))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Widget" :help "Selects the next page in the tabbed pane"
                       :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                 :tabbed-pane printer-input
                                                 :selected-index (mod (1+ (selected-index-of printer-input)) (length (selector-element-pairs-of printer-input)))))
                      ((gesture/keyboard/key-press :sdl-key-tab '(:shift :control))
                       :domain "Widget" :help "Selects the previous page in the tabbed pane"
                       :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                 :tabbed-pane printer-input
                                                 :selected-index (mod (1- (selected-index-of printer-input)) (length (selector-element-pairs-of printer-input))))))
                    child-operation)))

(def reader widget/scroll-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((gesture (gesture-of input))
         (child-gesture (progn
                          ;; KLUDGE: copy gesture
                          (setf (location-of gesture) (- (location-of gesture) (scroll-position-of (input-of printer-iomap))))
                          gesture))
         (child-input (make-command child-gesture
                                    (operation-of input)
                                    :domain (domain-of input)
                                    :description (description-of input)))
         (child-operation (recurse-reader recursion child-input (elt (child-iomaps-of printer-iomap) 1))))
    (merge-commands (gesture-case (gesture-of input)
                      ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers nil)
                       :domain "Widget" :help "Scrolls the content of the pane down"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 0 100)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers nil)
                       :domain "Widget" :help "Scrolls the content of the pane up"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 0 -100)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers '(:shift))
                       :domain "Widget" :help "Scrolls the content of the pane right"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 100 0)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers '(:shift))
                       :domain "Widget" :help "Scrolls the content of the pane left"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d -100 0))))
                    child-operation)))
