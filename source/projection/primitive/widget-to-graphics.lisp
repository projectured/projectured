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

(def function make-projection/widget/label->graphics/canvas ()
  (make-projection 'widget/label->graphics/canvas))

(def function make-projection/widget/tooltip->graphics/canvas ()
  (make-projection 'widget/tooltip->graphics/canvas))

(def function make-projection/widget/menu->graphics/canvas ()
  (make-projection 'widget/menu->graphics/canvas))

(def function make-projection/widget/menu-item->graphics/canvas ()
  (make-projection 'widget/menu-item->graphics/canvas))

(def function make-projection/widget/shell->graphics/canvas ()
  (make-projection 'widget/shell->graphics/canvas))

(def function make-projection/widget/composite->graphics/canvas ()
  (make-projection 'widget/composite->graphics/canvas))

(def function make-projection/widget/split-pane->graphics/canvas ()
  (make-projection 'widget/split-pane->graphics/canvas))

(def function make-projection/widget/tabbed-pane->graphics/canvas ()
  (make-projection 'widget/tabbed-pane->graphics/canvas))

(def function make-projection/widget/scroll-pane->graphics/canvas ()
  (make-projection 'widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Construction

(def macro widget/label->graphics/canvas ()
  '(make-projection/widget/label->graphics/canvas))

(def macro widget/tooltip->graphics/canvas ()
  '(make-projection/widget/tooltip->graphics/canvas))

(def macro widget/menu->graphics/canvas ()
  '(make-projection/widget/menu->graphics/canvas))

(def macro widget/menu-item->graphics/canvas ()
  '(make-projection/widget/menu-item->graphics/canvas))

(def macro widget/shell->graphics/canvas ()
  '(make-projection/widget/shell->graphics/canvas))

(def macro widget/composite->graphics/canvas ()
  '(make-projection/widget/composite->graphics/canvas))

(def macro widget/split-pane->graphics/canvas ()
  '(make-projection/widget/split-pane->graphics/canvas))

(def macro widget/tabbed-pane->graphics/canvas ()
  '(make-projection/widget/tabbed-pane->graphics/canvas))

(def macro widget/scroll-pane->graphics/canvas ()
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
                         (list (make-iomap/object projection recursion input input-reference output)
                               content-iomap))))

(def printer widget/tooltip->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input)
                                             `((content-of (the widget/tooltip document))
                                               ,@(typed-reference (form-type input) input-reference)))))
         (output (as (if (visible-p input)
                         (bind ((content-output (output-of (va content-iomap)))
                                (content-rectangle (make-bounding-rectangle content-output))
                                (margin (margin-of input)))
                           (make-graphics/canvas (list (make-graphics/rectangle (- (location-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                                (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                                        (+ (top-of margin) (bottom-of margin))))
                                                                                :fill-color *color/pastel-yellow*
                                                                                :stroke-color *color/black*)
                                                       content-output)
                                                 (location-of input)))
                         (make-graphics/canvas nil (make-2d 0 0))))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

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
                         (list (make-iomap/object projection recursion input input-reference output)))))

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
                             (list (make-iomap/object projection recursion input input-reference output)
                                   content-iomap)))))

(def printer widget/shell->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input)
                                             `((content-of (the widget/shell document))
                                               ,@(typed-reference (form-type input) input-reference)))))
         (tooltip-iomap (as (recurse-printer recursion (tooltip-of input)
                                             `((tooltip-of (the widget/shell document))
                                               ,@(typed-reference (form-type input) input-reference)))))
         (output (as (make-graphics/canvas (as (list (output-of (va content-iomap))
                                                     (output-of (va tooltip-iomap))))
                                           (make-2d 0 0)))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

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
                         (list* (make-iomap/object projection recursion input input-reference output)
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
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

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
                                             (collect (make-graphics/image (- (+ x (location-of bounding-rectangle) (size-of bounding-rectangle)) (make-2d 0 23))
                                                                           (image/image () (resource-pathname "image/close.png")))
                                               :into result)
                                             (collect (make-graphics/rectangle (+ (location-of bounding-rectangle) x)
                                                                               (+ (size-of bounding-rectangle) 16)
                                                                               :stroke-color *color/black*
                                                                               :fill-color (when (= index (selected-index-of input))
                                                                                             *color/solarized/background/light*))
                                               :into result)
                                             (collect (make-graphics/canvas (list (output-of selector-iomap)) (make-2d x 0)) :into result)
                                             (incf x (+ 20 (2d-x (size-of bounding-rectangle))))
                                             (finally
                                              (return (append result
                                                              (list (make-graphics/canvas (list (output-of content-iomap))
                                                                                          (make-2d 0 height)))))))
                                       (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

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
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

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
  input)

(def reader widget/menu-item->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  input)

(def reader widget/shell->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (content-command (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))
         (tooltip (tooltip-of printer-input)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/functional operation)
                               (operation/save-document operation)
                               (operation/load-document operation)
                               (operation/widget/tabbed-pane/select-page operation)
                               (operation/replace-selection
                                (make-operation/replace-selection printer-input (append (selection-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the widget/shell document)))))))
                               (operation/sequence/replace-element-range
                                (make-operation/sequence/replace-element-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the widget/shell document))))) (replacement-of operation)))
                               (operation/number/replace-range
                                (make-operation/number/replace-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the widget/shell document))))) (replacement-of operation)))
                               (operation/replace-target
                                (make-operation/replace-target printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the widget/shell document))))) (replacement-of operation)))
                               (operation/focusing/replace-part
                                operation)
                               (operation/show-context-sensitive-help
                                (make-operation/compound (list (make-instance 'operation/widget/tooltip/move :tooltip tooltip :location (mouse-position))
                                                               (make-instance 'operation/widget/tooltip/replace-content :tooltip tooltip
                                                                              :content (make-text/text (iter (for command :in (commands-of operation))
                                                                                                             (unless (first-iteration-p)
                                                                                                               (collect (text/newline)))
                                                                                                             (appending (make-command-help-text command)))))
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
                                (if (and (or operation (typep (gesture-of input) 'gesture/mouse/move)) (visible-p tooltip))
                                    (make-operation/compound (optional-list operation (make-instance 'operation/widget/hide :widget tooltip)))
                                    operation)))))
                    (recurse (operation-of content-command)))
                  :domain (domain-of content-command)
                  :description (description-of content-command))))

(def reader widget/composite->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of printer-iomap)))
        (thereis (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) (1+ index))))))

(def reader widget/split-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (if (typep (gesture-of input) 'gesture/mouse)
                        (pattern-case (reverse (make-reference (output-of printer-iomap) (mouse-position) nil))
                          (((the sequence (elements-of (the graphics/canvas document)))
                            (the graphics/canvas (elt (the sequence document) ?index))
                            . ?rest)
                           (bind ((index (floor (/ ?index 2)))
                                  (element (elt (elements-of printer-input) index))
                                  (child-iomap (elt (child-iomaps-of printer-iomap) index))
                                  (gesture (gesture-of input))
                                  (child-gesture (progn
                                                   ;; KLUDGE: copy gesture
                                                   (setf (location-of gesture) (- (location-of gesture) (location-of (elt (elements-of (output-of printer-iomap)) ?index))))
                                                   gesture))
                                  (child-input (make-command child-gesture
                                                             (operation-of input)
                                                             :domain (domain-of input)
                                                             :description (description-of input)))
                                  (element-path `((the ,(form-type element) (elt (the sequence document) ,index)) (the sequence (elements-of (the widget/split-pane document)))))
                                  (content-command (recurse-reader recursion child-input child-iomap)))
                             (make-command (gesture-of input)
                                           (operation/extend printer-input element-path (operation-of content-command))
                                           :domain (domain-of content-command)
                                           :description (description-of content-command)))))
                        (pattern-case (reverse (selection-of printer-input))
                          (((the sequence (elements-of (the widget/split-pane document)))
                            (the ?type (elt (the sequence document) ?index))
                            . ?rest)
                           (bind ((child-iomap (elt (child-iomaps-of printer-iomap) ?index))
                                  (gesture (gesture-of input))
                                  (child-gesture (if (typep gesture 'gesture/mouse/button/click)
                                                     ;; TODO: width
                                                     (make-instance 'gesture/mouse/button/click :modifiers (modifiers-of gesture) :button (button-of gesture) :location (- (location-of (gesture-of input)) (make-2d 250 0)))
                                                     (gesture-of input)))
                                  (child-input (make-command child-gesture
                                                             (operation-of input)
                                                             :domain (domain-of input)
                                                             :description (description-of input)))
                                  (element-path `((the ,(form-type (elt (elements-of printer-input) ?index)) (elt (the sequence document) ,?index)) (the sequence (elements-of (the widget/split-pane document)))))
                                  (content-command (recurse-reader recursion child-input child-iomap)))
                             (make-command (gesture-of input)
                                           (operation/extend printer-input element-path (operation-of content-command))
                                           :domain (domain-of content-command)
                                           :description (description-of content-command))))))
                    (make-command/nothing (gesture-of input))))
  #+nil
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of printer-iomap)))
        (thereis (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) (1+ index))))))

(def reader widget/tabbed-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (gesture (gesture-of input)))
    (merge-commands (gesture-case (gesture-of input)
                      ((make-instance 'gesture/mouse/button/click :button :button-left :modifiers nil)
                       :domain "Widget" :description "Selects the page in the tabbed pane where the mouse is pointing at"
                       :operation (pattern-case (reverse (make-reference (output-of printer-iomap) (location-of (gesture-of input)) nil))
                                    (((the sequence (elements-of (the graphics/canvas document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (when (and (< ?index (* 3 (length (selector-element-pairs-of printer-input))))
                                                (= (mod ?index 3) 1))
                                       (make-instance 'operation/widget/tabbed-pane/select-page
                                                      :tabbed-pane printer-input
                                                      :selected-index (floor ?index 3))))))
                      ((make-instance 'gesture/mouse/button/click :button :button-left :modifiers nil)
                       :domain "Widget" :description "Closes the page in the tabbed pane where the mouse is pointing at"
                       :operation (pattern-case (reverse (make-reference (output-of printer-iomap) (location-of (gesture-of input)) nil))
                                    (((the sequence (elements-of (the graphics/canvas document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (when (and (< ?index (* 3 (length (selector-element-pairs-of printer-input))))
                                                (= (mod ?index 3) 0))
                                       (make-operation/functional (lambda ()
                                                                    (bind ((selector-element-pairs (selector-element-pairs-of printer-input))
                                                                           (index (floor ?index 3)))
                                                                      (setf (selector-element-pairs-of printer-input)
                                                                            (append (subseq selector-element-pairs 0 index)
                                                                                    (subseq selector-element-pairs (1+ index) (length selector-element-pairs)))))))))))
                      ((gesture/keyboard/key-press :sdl-key-tab :control)
                       :domain "Widget" :description "Selects the next page in the tabbed pane"
                       :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                 :tabbed-pane printer-input
                                                 :selected-index (mod (1+ (selected-index-of printer-input)) (length (selector-element-pairs-of printer-input)))))
                      ((gesture/keyboard/key-press :sdl-key-tab '(:shift :control))
                       :domain "Widget" :description "Selects the previous page in the tabbed pane"
                       :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                                 :tabbed-pane printer-input
                                                 :selected-index (mod (1- (selected-index-of printer-input)) (length (selector-element-pairs-of printer-input))))))
                    (bind ((element (second (elt (selector-element-pairs-of printer-input) (selected-index-of printer-input))))
                           (element-type (form-type element))
                           (element-reference `((the ,element-type (elt (the sequence document) 1))
                                                (the sequence (elt (the sequence document) ,(selected-index-of printer-input)))
                                                (the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))))
                           (child-input (if (typep gesture 'gesture/mouse/button/click)
                                            ;; TODO: width
                                            (make-command (make-instance 'gesture/mouse/button/click :modifiers (modifiers-of gesture) :button (button-of gesture) :location (- (location-of (gesture-of input)) (make-2d 0 40)))
                                                          (operation-of input)
                                                          :domain (domain-of input)
                                                          :description (description-of input))
                                            input))
                           (child-output (recurse-reader recursion child-input (elt (child-iomaps-of printer-iomap) 0))))
                      (make-command (gesture-of input)
                                    (operation/extend printer-input element-reference (operation-of child-output))
                                    :domain (domain-of child-output)
                                    :description (description-of child-output)))
                    (make-command/nothing (gesture-of input)))))

(def reader widget/scroll-pane->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (gesture (gesture-of input)))
    (merge-commands (gesture-case gesture
                      ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers nil)
                       :domain "Widget" :description "Scrolls the content of the pane down"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 0 100)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers nil)
                       :domain "Widget" :description "Scrolls the content of the pane up"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 0 -100)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-up :modifiers '(:shift))
                       :domain "Widget" :description "Scrolls the content of the pane right"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d 100 0)))
                      ((make-instance 'gesture/mouse/button/click :button :wheel-down :modifiers '(:shift))
                       :domain "Widget" :description "Scrolls the content of the pane left"
                       :operation (make-instance 'operation/widget/scroll-pane/scroll
                                                 :scroll-pane (input-of printer-iomap)
                                                 :scroll-delta (make-2d -100 0))))
                    (document/read-operation gesture)
                    (bind ((content-gesture (progn
                                              ;; KLUDGE: copy gesture
                                              (setf (location-of gesture) (- (location-of gesture) (scroll-position-of (input-of printer-iomap))))
                                              gesture))
                           (content-input (make-command content-gesture (operation-of input) :domain (domain-of input) :description (description-of input)))
                           (content-command (recurse-reader recursion content-input (elt (child-iomaps-of printer-iomap) 0)))
                           (content-path `((the ,(form-type (content-of printer-input)) (content-of (the widget/scroll-pane document))))))
                      (make-command gesture
                                    (operation/extend printer-input content-path (operation-of content-command))
                                    :domain (domain-of content-command)
                                    :description (description-of content-command))))))
