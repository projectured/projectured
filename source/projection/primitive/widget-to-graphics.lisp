;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection widget/label->graphics/canvas ()
  ())

(def projection widget/text->graphics/canvas ()
  ())

(def projection widget/tooltip->graphics/canvas ()
  ())

(def projection widget/menu->graphics/canvas ()
  ())

(def projection widget/menu-item->graphics/canvas ()
  ())

(def projection widget/composite->graphics/canvas ()
  ())

(def projection widget/shell->graphics/canvas ()
  ())

(def projection widget/title-pane->graphics/canvas ()
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

(def function make-projection/widget/text->graphics/canvas ()
  (make-projection 'widget/text->graphics/canvas))

(def function make-projection/widget/tooltip->graphics/canvas ()
  (make-projection 'widget/tooltip->graphics/canvas))

(def function make-projection/widget/menu->graphics/canvas ()
  (make-projection 'widget/menu->graphics/canvas))

(def function make-projection/widget/menu-item->graphics/canvas ()
  (make-projection 'widget/menu-item->graphics/canvas))

(def function make-projection/widget/composite->graphics/canvas ()
  (make-projection 'widget/composite->graphics/canvas))

(def function make-projection/widget/shell->graphics/canvas ()
  (make-projection 'widget/shell->graphics/canvas))

(def function make-projection/widget/title-pane->graphics/canvas ()
  (make-projection 'widget/title-pane->graphics/canvas))

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

(def macro widget/text->graphics/canvas ()
  '(make-projection/widget/text->graphics/canvas))

(def macro widget/tooltip->graphics/canvas ()
  '(make-projection/widget/tooltip->graphics/canvas))

(def macro widget/menu->graphics/canvas ()
  '(make-projection/widget/menu->graphics/canvas))

(def macro widget/menu-item->graphics/canvas ()
  '(make-projection/widget/menu-item->graphics/canvas))

(def macro widget/composite->graphics/canvas ()
  '(make-projection/widget/composite->graphics/canvas))

(def macro widget/shell->graphics/canvas ()
  '(make-projection/widget/shell->graphics/canvas))

(def macro widget/title-pane->graphics/canvas ()
  '(make-projection/widget/title-pane->graphics/canvas))

(def macro widget/split-pane->graphics/canvas ()
  '(make-projection/widget/split-pane->graphics/canvas))

(def macro widget/tabbed-pane->graphics/canvas ()
  '(make-projection/widget/tabbed-pane->graphics/canvas))

(def macro widget/scroll-pane->graphics/canvas ()
  '(make-projection/widget/scroll-pane->graphics/canvas))

;;;;;;
;;; Forward mapper

(def forward-mapper widget/label->graphics/canvas ())

(def forward-mapper widget/text->graphics/canvas ()
  (pattern-case -reference-
    (((the ?type (content-of (the widget/text document)))
      . ?rest)
     (values `((the sequence (elements-of (the graphics/canvas document)))
               (the graphics/canvas (elt (the sequence document) 1))
               (the sequence (elements-of (the graphics/canvas document)))
               (the graphics/canvas (elt (the sequence document) 0)))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 0)))
    (((the graphics/canvas (printer-output (the widget/text document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper widget/tooltip->graphics/canvas ())

(def forward-mapper widget/menu->graphics/canvas ())

(def forward-mapper widget/menu-item->graphics/canvas ())

(def forward-mapper widget/composite->graphics/canvas ()
  (pattern-case -reference-
    (((the sequence (elements-of (the widget/composite document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?index))
            (element (output-of element-iomap)))
       (values `((the sequence (elements-of (the graphics/canvas document)))
                 (the ,(document-type element) (elt (the sequence document) ,?index)))
               ?rest
               element-iomap)))
    (((the graphics/canvas (printer-output (the widget/composite document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper widget/shell->graphics/canvas ())

(def forward-mapper widget/title-pane->graphics/canvas ())

(def forward-mapper widget/split-pane->graphics/canvas ())

(def forward-mapper widget/tabbed-pane->graphics/canvas ())

(def forward-mapper widget/scroll-pane->graphics/canvas ())

;;;;;;
;;; Backward mapper

(def backward-mapper widget/label->graphics/canvas ())

(def backward-mapper widget/text->graphics/canvas ()
  (pattern-case -reference-
    (((the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) 1))
      (the sequence (elements-of (the graphics/canvas document)))
      (the graphics/canvas (elt (the sequence document) 0))
      . ?rest)
     (values `((the ,(document-type (content-of (input-of -printer-iomap-))) (content-of (the widget/text document))))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 0)))
    (?
     (append `((the graphics/canvas (printer-output (the widget/text document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper widget/tooltip->graphics/canvas ())

(def backward-mapper widget/menu->graphics/canvas ())

(def backward-mapper widget/menu-item->graphics/canvas ())

(def backward-mapper widget/composite->graphics/canvas ()
  (pattern-case -reference-
    (((the sequence (elements-of (the graphics/canvas document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?index))
            (element (input-of element-iomap)))
       (values `((the sequence (elements-of (the widget/composite document)))
                 (the ,(document-type element) (elt (the sequence document) ,?index)))
               ?rest
               element-iomap)))
    (?
     (append `((the graphics/canvas (printer-output (the widget/composite document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper widget/shell->graphics/canvas ())

(def backward-mapper widget/title-pane->graphics/canvas ())

(def backward-mapper widget/split-pane->graphics/canvas ())

(def backward-mapper widget/tabbed-pane->graphics/canvas ())

(def backward-mapper widget/scroll-pane->graphics/canvas ())

;;;;;;
;;; Printer

(def function widget/content-top-left (widget)
  (+ (inset/top-left (margin-of widget)) (inset/top-left (border-of widget)) (inset/top-left (padding-of widget))))

(def function widget/make-surrounding-graphics (widget content-size content-fill-color &key margin-corners border-corners (padding-corners nil padding-corners?) content-corners (border-extra-size 0))
  (bind ((margin (margin-of widget))
         (margin-color (margin-color-of widget))
         (border (border-of widget))
         (border-color (border-color-of widget))
         (padding (padding-of widget))
         (padding-color (padding-color-of widget)))
    (append (when margin-color
              (list (if margin-corners
                        (make-graphics/rounded-rectangle 0
                                                         (+ content-size border-extra-size (inset/size margin) (inset/size border) (inset/size padding))
                                                         9
                                                         :corners margin-corners
                                                         :fill-color margin-color)
                        (make-graphics/rectangle 0
                                                 (+ content-size border-extra-size (inset/size margin) (inset/size border) (inset/size padding))
                                                 :fill-color margin-color))))
            (when border-color
              (list (if border-corners
                        (make-graphics/rounded-rectangle (+ (inset/top-left margin) border-extra-size)
                                                         (+ content-size (inset/size border) (inset/size padding))
                                                         9
                                                         :corners border-corners
                                                         :fill-color border-color)
                        (make-graphics/rectangle (+ (inset/top-left margin) border-extra-size)
                                                 (+ content-size (inset/size border) (inset/size padding))
                                                 :fill-color border-color))))
            (when padding-color
              (list (if padding-corners?
                        (make-graphics/rounded-rectangle (+ border-extra-size (inset/top-left margin) (inset/top-left border))
                                                         (+ content-size (inset/size padding))
                                                         9
                                                         :corners padding-corners
                                                         :fill-color padding-color)
                        (make-graphics/rectangle (+ border-extra-size (inset/top-left margin) (inset/top-left border))
                                                 (+ content-size (inset/size padding))
                                                 :fill-color padding-color))))
            (when content-fill-color
              (list (if content-corners
                        (make-graphics/rounded-rectangle (+ border-extra-size (inset/top-left margin) (inset/top-left border) (inset/top-left padding))
                                                         content-size
                                                         9
                                                         :corners content-corners
                                                         :fill-color content-fill-color)
                        (make-graphics/rectangle (+ border-extra-size (inset/top-left margin) (inset/top-left border) (inset/top-left padding))
                                                 content-size
                                                 :fill-color content-fill-color)))))))

(def printer widget/label->graphics/canvas ()
  (bind ((content (content-of -input-))
         (content-iomap (recurse-printer -recursion- content
                                         `((content-of (the widget/label document))
                                           ,@(typed-reference (document-type -input-) -input-reference-))))
         (content-output (output-of content-iomap))
         (content-rectangle (bounds-of content-output))
         (margin (margin-of -input-))
         (output (make-graphics/canvas (list (make-graphics/rectangle (- (position-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                      (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                              (+ (top-of margin) (bottom-of margin))))
                                                                      :stroke-color *color/black*)
                                             content-output)
                                       (position-of -input-))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (list content-iomap))))

(def printer widget/text->graphics/canvas ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the widget/label document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (as (bind ((content-output (output-of (va content-iomap)))
                            (content-bounding-rectangle (bounds-of (output-of (va content-iomap)))))
                       (make-graphics/canvas (list (make-graphics/canvas (widget/make-surrounding-graphics -input- (size-of content-bounding-rectangle) (content-fill-color-of -input-) :margin-corners '(nil)) 0)
                                                   (make-graphics/canvas (list content-output) (widget/content-top-left -input-)))
                                             (position-of -input-))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

(def printer widget/tooltip->graphics/canvas ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the widget/tooltip document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (as (if (visible-p -input-)
                         (bind ((content-output (output-of (va content-iomap)))
                                (content-rectangle (bounds-of content-output))
                                (margin (margin-of -input-)))
                           (make-output/window (position-of -input-)
                                               (size-of -input-)
                                               (make-graphics/canvas (list (make-graphics/rounded-rectangle (- (position-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                                                            (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                                                                    (+ (top-of margin) (bottom-of margin))))
                                                                                                            9
                                                                                                            :fill-color (color/lighten *color/solarized/yellow* 0.75))
                                                                           content-output)
                                                                     0)))
                         (make-graphics/canvas nil 0)))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

(def printer widget/menu->graphics/canvas ()
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of -input-))
                               (collect (recurse-printer -recursion- element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/menu document)))
                                                           ,@(typed-reference (document-type -input-) -input-reference-))))))
         (output (make-graphics/canvas (iter (with y = 0)
                                             (for index :from 0)
                                             (for element-iomap :in-sequence element-iomaps)
                                             (for bounding-rectangle = (bounds-of (output-of element-iomap)))
                                             (maximizing (2d-x (size-of bounding-rectangle)) :into width)
                                             (collect (make-graphics/canvas (list (output-of element-iomap)) (make-2d 0 y)) :into result)
                                             (incf y (2d-y (size-of bounding-rectangle)))
                                             (finally (return (list* (make-graphics/rectangle 0 (make-2d width y)
                                                                                              :stroke-color *color/solarized/content/darker*
                                                                                              :fill-color *color/solarized/yellow*)
                                                                     result))))
                                       0)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output
                         (list (make-iomap -projection- -recursion- -input- -input-reference- output)))))

(def printer widget/menu-item->graphics/canvas ()
  (if (visible-p -input-)
      (bind ((content (content-of -input-))
             (content-iomap (recurse-printer -recursion- content
                                             `((content-of (the widget/menu-item document))
                                               ,@(typed-reference (document-type -input-) -input-reference-))))
             (content-output (output-of content-iomap))
             (content-rectangle (bounds-of content-output))
             (margin (margin-of -input-))
             (output (make-graphics/canvas (list (make-graphics/rectangle (- (position-of content-rectangle) (make-2d (left-of margin) (top-of margin)))
                                                                          (+ (size-of content-rectangle) (make-2d (+ (left-of margin) (right-of margin))
                                                                                                                  (+ (top-of margin) (bottom-of margin)))))
                                                 content-output)
                                           (make-2d (left-of margin) (top-of margin)))))
        (make-iomap/compound -projection- -recursion- -input- -input-reference- output
                             (list (make-iomap -projection- -recursion- -input- -input-reference- output)
                                   content-iomap)))))

(def printer widget/composite->graphics/canvas ()
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of -input-))
                               (collect (recurse-printer -recursion- element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/composite document)))
                                                           ,@(typed-reference (document-type -input-) -input-reference-))))))
         (output (make-graphics/canvas (mapcar 'output-of element-iomaps)
                                       (position-of -input-))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer widget/shell->graphics/canvas ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the widget/shell document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (tooltip-iomap (as (when (tooltip-of -input-)
                              (recurse-printer -recursion- (tooltip-of -input-)
                                               `((tooltip-of (the widget/shell document))
                                                 ,@(typed-reference (document-type -input-) -input-reference-))))))
         (content-bounding-rectangle (bounds-of (output-of (va content-iomap))))
         (output (as (bind ((tooltip (when (va tooltip-iomap)
                                       (output-of (va tooltip-iomap)))))
                       (make-output/display (optional-list (make-output/window (make-2d 200 200) ; TODO: initial position
                                                                               (size-of -input-)
                                                                               (make-graphics/canvas (as (append (widget/make-surrounding-graphics -input- (size-of content-bounding-rectangle) (content-fill-color-of -input-))
                                                                                                                 (list (make-graphics/canvas (list (output-of (va content-iomap)))
                                                                                                                                             (widget/content-top-left -input-)))))
                                                                                                     0)
                                                                               :title "ProjecturEd")
                                                           (when (typep tooltip 'output/window)
                                                             tooltip)))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va content-iomap))))))

(def printer widget/title-pane->graphics/canvas ()
  (bind ((margin (margin-of -input-))
         (title-iomap (recurse-printer -recursion- (title-of -input-) nil))
         (title-bounding-rectangle (bounds-of (output-of title-iomap)))
         (title-graphics (make-graphics/canvas (list (make-graphics/rounded-rectangle 0
                                                                                      (+ (size-of title-bounding-rectangle) (make-2d 15 10))
                                                                                      9
                                                                                      :corners '(:bottom-left :bottom-right)
                                                                                      :fill-color (title-fill-color-of -input-))
                                                     (make-graphics/canvas (list (output-of title-iomap)) (make-2d 8 5)))
                                               0))
         (title-bounding-rectangle (bounds-of title-graphics))
         (content-iomap (recurse-printer -recursion- (content-of -input-)
                                         `((content-of (the widget/scroll-pane document))
                                           ,@(typed-reference (document-type -input-) -input-reference-))))
         (content-bounding-rectangle (bounds-of (output-of content-iomap)))
         (border-extra-size (make-2d 0 (2d-y (size-of title-bounding-rectangle))))
         (output (make-graphics/canvas (append (widget/make-surrounding-graphics -input- (size-of content-bounding-rectangle) (content-fill-color-of -input-)
                                                                                 :border-corners '(:top-left) :border-extra-size border-extra-size)
                                               (list (make-graphics/canvas (list title-graphics) (inset/top-left margin)))
                                               (list (make-graphics/canvas (list (output-of content-iomap))
                                                                           (+ (widget/content-top-left -input-) border-extra-size))))
                                       0)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (list content-iomap))))

(def printer widget/split-pane->graphics/canvas ()
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of -input-))
                               (collect (recurse-printer -recursion- element
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the widget/split-pane document)))
                                                           ,@(typed-reference (document-type -input-) -input-reference-))))))
         (sizes (iter (for element-iomap :in element-iomaps)
                      (collect (size-of (bounds-of (output-of element-iomap))))))
         (width (apply 'max (mapcar '2d-x sizes)))
         (height (apply 'max (mapcar '2d-y sizes)))
         (output (make-graphics/canvas (iter (with d = 0)
                                             (with orientation = (orientation-of -input-))
                                             (for element-iomap :in element-iomaps)
                                             (unless (first-iteration-p)
                                               (bind ((splitter-size 5))
                                                 (collect (make-graphics/rectangle (ecase orientation
                                                                                     (:horizontal (make-2d (+ d 2) 0))
                                                                                     (:vertical (make-2d 0 (+ d 2))))
                                                                                   (ecase orientation
                                                                                     (:horizontal (make-2d (- splitter-size 2) height))
                                                                                     (:vertical (make-2d width (- splitter-size 2))))
                                                                                   :fill-color *color/solarized/content/darker*))
                                                 (incf d (1+ splitter-size))))
                                             (collect (make-graphics/canvas (list (output-of element-iomap)) (ecase orientation
                                                                                                               (:horizontal (make-2d d 0))
                                                                                                               (:vertical (make-2d 0 d)))))
                                             (for size = (size-of (bounds-of (output-of element-iomap))))
                                             (incf d (ecase orientation
                                                       (:horizontal (2d-x size))
                                                       (:vertical (2d-y size)))))
                                       0)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer widget/tabbed-pane->graphics/canvas ()
  (bind ((selector-iomaps (iter (for index :from 0)
                                (for pairs :in-sequence (selector-element-pairs-of -input-))
                                (collect (recurse-printer -recursion- (first pairs)
                                                          `((elt (the sequence document) 0)
                                                            (the sequence (elt (the sequence document) ,index))
                                                            (the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                                                            ,@(typed-reference (document-type -input-) -input-reference-))))))
         (content-iomap (when selector-iomaps
                          (recurse-printer -recursion- (second (elt (selector-element-pairs-of -input-) (selected-index-of -input-)))
                                           `((elt (the sequence document) 1)
                                             (the sequence (elt (the sequence document) ,(selected-index-of -input-)))
                                             (the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                                             ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (make-graphics/canvas (when selector-iomaps
                                         (iter (with x = 0)
                                               (for index :from 0)
                                               (for selector-iomap :in-sequence selector-iomaps)
                                               (for bounding-rectangle = (bounds-of (output-of selector-iomap)))
                                               (maximizing (2d-y (size-of bounding-rectangle)) :into height)
                                               (collect (make-graphics/rounded-rectangle (+ x (position-of bounding-rectangle))
                                                                                         (+ (size-of bounding-rectangle) (make-2d 40 10))
                                                                                         9
                                                                                         :corners '(:bottom-left :bottom-right)
                                                                                         :fill-color (if (= index (selected-index-of -input-))
                                                                                                         *color/solarized/content/lighter*
                                                                                                         *color/solarized/content/darker*))
                                                 :into selector-graphics)
                                               (collect (make-graphics/image (+ x (position-of bounding-rectangle) (size-of bounding-rectangle) (make-2d 16 -12))
                                                                             (image/file () (resource-pathname "image/close.png")))
                                                 :into selector-graphics)
                                               (collect (make-graphics/canvas (list (output-of selector-iomap)) (+ x (make-2d 8 5))) :into selector-graphics)
                                               (incf x (+ 40 (2d-x (size-of bounding-rectangle))))
                                               (finally
                                                (return (bind ((border-extra-size (make-2d 0 (+ 10 height))))
                                                          (append selector-graphics
                                                                  (widget/make-surrounding-graphics -input- (size-of (bounds-of (output-of content-iomap))) nil
                                                                                                    :border-corners '(:top-left) :border-extra-size border-extra-size)
                                                                  (when content-iomap
                                                                    (list (make-graphics/canvas (list (output-of content-iomap)) (+ border-extra-size (widget/content-top-left -input-)))))))))))
                                       0)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (list content-iomap))))

(def printer widget/scroll-pane->graphics/canvas ()
  (bind ((content-iomap (recurse-printer -recursion- (content-of -input-)
                                         `((content-of (the widget/scroll-pane document))
                                           ,@(typed-reference (document-type -input-) -input-reference-))))
         (position (or (position-of -input-) 0))
         (size (size-of -input-))
         (content (make-graphics/canvas (list (output-of content-iomap)) (as (scroll-position-of -input-))))
         (output (make-graphics/canvas (append (widget/make-surrounding-graphics -input- size (content-fill-color-of -input-)
                                                                                 :padding-corners nil)
                                               (list (make-graphics/canvas (list (make-graphics/viewport content position size)) (widget/content-top-left -input-))))
                                       0)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (list content-iomap))))

;;;;;;
;;; Reader

(def reader widget/label->graphics/canvas ()
  (merge-commands (command/extend (recurse-reader -recursion- -input- (elt (child-iomaps-of -printer-iomap-) 0))
                                  -printer-input-
                                  `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/label document)))))
                  (make-nothing-command -gesture-)))

(def reader widget/text->graphics/canvas ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/widget/text->graphics/canvas 'backward-mapper/widget/text->graphics/canvas)
                  (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/widget/text->graphics/canvas nil)
                  (awhen (graphics/read-operation (output-of -printer-iomap-) -gesture-)
                    (command/read-backward -recursion- it -printer-iomap- 'backward-mapper/widget/text->graphics/canvas nil))
                  (make-nothing-command -gesture-)))

(def reader widget/tooltip->graphics/canvas ()
  (if (and (typep -input- 'gesture/mouse/move)
           (visible-p -printer-input-))
      (make-instance 'operation/widget/hide :widget -printer-input-)
      -input-))

(def reader widget/menu->graphics/canvas ()
  -input-)

(def reader widget/menu-item->graphics/canvas ()
  -input-)

(def reader widget/composite->graphics/canvas ()
  (merge-commands (unless (typep -gesture- 'gesture/mouse)
                    (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/widget/composite->graphics/canvas 'backward-mapper/widget/composite->graphics/canvas))
                  (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/widget/composite->graphics/canvas nil)
                  (awhen (graphics/read-operation (output-of -printer-iomap-) -gesture-)
                    (command/read-backward -recursion- it -printer-iomap- 'backward-mapper/widget/composite->graphics/canvas nil))
                  (make-nothing-command -gesture-)))

(def reader widget/shell->graphics/canvas ()
  (bind ((content-command (recurse-reader -recursion- -input- (elt (child-iomaps-of -printer-iomap-) 0)))
         (tooltip (tooltip-of -printer-input-)))
    (merge-commands (labels ((recurse (operation)
                               (typecase operation
                                 (operation/quit operation)
                                 (operation/functional operation)
                                 (operation/select-next-alternative operation)
                                 (operation/save-document operation)
                                 (operation/load-document operation)
                                 (operation/widget/tabbed-pane/select-page operation)
                                 (operation/widget/scroll-pane/scroll operation)
                                 (operation/replace-selection
                                  (make-operation/replace-selection -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation))))
                                 (operation/sequence/replace-range
                                  (make-operation/sequence/replace-range -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation)) (replacement-of operation)))
                                 (operation/string/replace-range
                                  (make-operation/string/replace-range -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation)) (replacement-of operation)))
                                 (operation/text/replace-range
                                  (make-operation/text/replace-range -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation)) (replacement-of operation)))
                                 (operation/number/replace-range
                                  (make-operation/number/replace-range -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation)) (replacement-of operation)))
                                 (operation/replace-target
                                  (make-operation/replace-target -printer-input- (append `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/shell document)))) (selection-of operation)) (replacement-of operation)))
                                 (operation/focusing/replace-part
                                  operation)
                                 (operation/show-context-sensitive-help
                                  (bind ((content (make-help/context-sensitive (commands-of operation)))
                                         (size (size-of (make-bounding-rectangle (printer-output content -recursion-)))))
                                    (make-operation/replace-target -printer-input-
                                                                   '((the widget/tooltip (tooltip-of (the widget/shell document))))
                                                                   (widget/tooltip (:position (mouse-position-of -gesture-) :size size)
                                                                     content))))
                                 (operation/describe
                                  (bind ((content (document/reference ()
                                                    (selection-of operation)))
                                         (size (size-of (make-bounding-rectangle (printer-output content -recursion-)))))
                                    (make-operation/replace-target -printer-input-
                                                                   '((the widget/tooltip (tooltip-of (the widget/shell document))))
                                                                   (widget/tooltip (:position (mouse-position-of -gesture-) :size size)
                                                                     content))))
                                 (operation/show-annotation
                                  (bind ((content (annotation-of (eval-reference (document-of operation) (flatten-reference (selection-of operation)))))
                                         (size (size-of (make-bounding-rectangle (printer-output content -recursion-)))))
                                    (make-operation/replace-target -printer-input-
                                                                   '((the widget/tooltip (tooltip-of (the widget/shell document))))
                                                                   (widget/tooltip (:position (mouse-position-of -gesture-) :size size)
                                                                     content))))
                                 (operation/compound
                                  (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                    (unless (some 'null child-operations)
                                      (make-operation/compound child-operations)))))))
                      (bind ((operation (recurse (operation-of content-command))))
                        (when operation
                          (if (and (tooltip-of -printer-input-)
                                   (visible-p (tooltip-of -printer-input-)))
                              (make-operation/compound (optional-list (make-command -gesture-
                                                                                    operation
                                                                                    :domain (domain-of content-command)
                                                                                    :description (description-of content-command))
                                                                      (make-instance 'operation/widget/hide :widget tooltip)))
                              (make-command -gesture-
                                            operation
                                            :domain (domain-of content-command)
                                            :description (description-of content-command))))))
                    (document/read-operation -gesture-)
                    (make-nothing-command -gesture-))))

(def reader widget/title-pane->graphics/canvas ()
  (merge-commands (bind ((content-gesture -gesture-)
                         (content-input (make-command content-gesture (operation-of -input-) :domain (domain-of -input-) :description (description-of -input-)))
                         (content-command (recurse-reader -recursion- content-input (elt (child-iomaps-of -printer-iomap-) 0)))
                         (content-path `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/scroll-pane document))))))
                    (make-command -gesture-
                                  (operation/extend -printer-input- content-path (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (document/read-operation -gesture-)
                  (make-nothing-command -gesture-)))

(def reader widget/split-pane->graphics/canvas ()
  (merge-commands (if (typep -gesture- 'gesture/mouse)
                      (pattern-case (make-reference (output-of -printer-iomap-) (mouse-position-of -gesture-) nil)
                        (((the sequence (elements-of (the graphics/canvas document)))
                          (the graphics/canvas (elt (the sequence document) ?index))
                          . ?rest)
                         (bind ((index (floor (/ ?index 2)))
                                (element (elt (elements-of -printer-input-) index))
                                (child-iomap (elt (child-iomaps-of -printer-iomap-) index))
                                (gesture -gesture-)
                                (child-gesture (progn
                                                 ;; KLUDGE: copy gesture
                                                 (setf (mouse-position-of gesture) (- (mouse-position-of gesture) (widget/content-top-left element) (position-of (elt (elements-of (output-of -printer-iomap-)) ?index))))
                                                 gesture))
                                (child-input (make-command child-gesture
                                                           (operation-of -input-)
                                                           :domain (domain-of -input-)
                                                           :description (description-of -input-)))
                                (element-path `((the ,(document-type element) (elt (the sequence document) ,index)) (the sequence (elements-of (the widget/split-pane document)))))
                                (content-command (recurse-reader -recursion- child-input child-iomap)))
                           (make-command -gesture-
                                         (operation/extend -printer-input- element-path (operation-of content-command))
                                         :domain (domain-of content-command)
                                         :description (description-of content-command)))))
                      (pattern-case (selection-of -printer-input-)
                        (((the sequence (elements-of (the widget/split-pane document)))
                          (the ?type (elt (the sequence document) ?index))
                          . ?rest)
                         (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?index))
                                (child-gesture (if (typep -gesture- 'gesture/mouse/click)
                                                   ;; TODO: width
                                                   (make-instance 'gesture/mouse/click :modifiers (modifiers-of -gesture-) :button (button-of -gesture-) :position (- (position-of -gesture-) (make-2d 250 0)))
                                                   -gesture-))
                                (child-input (make-command child-gesture
                                                           (operation-of -input-)
                                                           :domain (domain-of -input-)
                                                           :description (description-of -input-)))
                                (element-path `((the ,(document-type (elt (elements-of -printer-input-) ?index)) (elt (the sequence document) ,?index)) (the sequence (elements-of (the widget/split-pane document)))))
                                (content-command (recurse-reader -recursion- child-input child-iomap)))
                           (make-command -gesture-
                                         (operation/extend -printer-input- element-path (operation-of content-command))
                                         :domain (domain-of content-command)
                                         :description (description-of content-command))))))
                  (document/read-operation -gesture-)
                  (make-nothing-command -gesture-))
  #+nil
  (iter (for index :from 0)
        (for element :in-sequence (elements-of (input-of -printer-iomap-)))
        (thereis (recurse-reader -recursion- input (elt (child-iomaps-of -printer-iomap-) (1+ index))))))

(def reader widget/tabbed-pane->graphics/canvas ()
  (merge-commands (gesture-case -gesture-
                    ((make-instance 'gesture/mouse/click :button :button-left :modifiers nil)
                     :domain "Widget" :description "Selects the page in the tabbed pane where the mouse is pointing at"
                     :operation (pattern-case (make-reference (output-of -printer-iomap-) (position-of -gesture-) nil)
                                  (((the sequence (elements-of (the graphics/canvas document)))
                                    (the ?type (elt (the sequence document) ?index))
                                    . ?rest)
                                   (when (and (< ?index (* 3 (length (selector-element-pairs-of -printer-input-))))
                                              (= (mod ?index 3) 1))
                                     (make-instance 'operation/widget/tabbed-pane/select-page
                                                    :tabbed-pane -printer-input-
                                                    :selected-index (floor ?index 3))))))
                    ((make-instance 'gesture/mouse/click :button :button-left :modifiers nil)
                     :domain "Widget" :description "Closes the page in the tabbed pane where the mouse is pointing at"
                     :operation (pattern-case (make-reference (output-of -printer-iomap-) (position-of -gesture-) nil)
                                  (((the sequence (elements-of (the graphics/canvas document)))
                                    (the ?type (elt (the sequence document) ?index))
                                    . ?rest)
                                   (when (and (< ?index (* 3 (length (selector-element-pairs-of -printer-input-))))
                                              (= (mod ?index 3) 0))
                                     (make-operation/functional (lambda ()
                                                                  (bind ((selector-element-pairs (selector-element-pairs-of -printer-input-))
                                                                         (index (floor ?index 3)))
                                                                    (setf (selector-element-pairs-of -printer-input-)
                                                                          (append (subseq selector-element-pairs 0 index)
                                                                                  (subseq selector-element-pairs (1+ index) (length selector-element-pairs)))))))))))
                    ((make-key-press-gesture :scancode-tab :control)
                     :domain "Widget" :description "Selects the next page in the tabbed pane"
                     :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                               :tabbed-pane -printer-input-
                                               :selected-index (mod (1+ (selected-index-of -printer-input-)) (length (selector-element-pairs-of -printer-input-)))))
                    ((make-key-press-gesture :scancode-tab '(:shift :control))
                     :domain "Widget" :description "Selects the previous page in the tabbed pane"
                     :operation (make-instance 'operation/widget/tabbed-pane/select-page
                                               :tabbed-pane -printer-input-
                                               :selected-index (mod (1- (selected-index-of -printer-input-)) (length (selector-element-pairs-of -printer-input-))))))
                  (when (selector-element-pairs-of -printer-input-)
                    (bind ((element (second (elt (selector-element-pairs-of -printer-input-) (selected-index-of -printer-input-))))
                           (element-type (document-type element))
                           (element-reference `((the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                                                (the sequence (elt (the sequence document) ,(selected-index-of -printer-input-)))
                                                (the ,element-type (elt (the sequence document) 1))))
                           (child-input (if (typep -gesture- 'gesture/mouse/click)
                                            ;; TODO: width
                                            (make-command (make-instance 'gesture/mouse/click :modifiers (modifiers-of -gesture-) :button (button-of -gesture-) :position (- (position-of -gesture-) (make-2d 0 40)))
                                                          (operation-of -input-)
                                                          :domain (domain-of -input-)
                                                          :description (description-of -input-))
                                            -input-))
                           (child-output (recurse-reader -recursion- child-input (elt (child-iomaps-of -printer-iomap-) 0))))
                      (make-command -gesture-
                                    (operation/extend -printer-input- element-reference (operation-of child-output))
                                    :domain (domain-of child-output)
                                    :description (description-of child-output))))
                  (document/read-operation -gesture-)
                  (make-nothing-command -gesture-)))

(def reader widget/scroll-pane->graphics/canvas ()
  (merge-commands (gesture-case -gesture-
                    ((make-instance 'gesture/mouse/scroll :scroll 0 :modifiers nil)
                     :domain "Widget" :description "Scrolls the pane vertically"
                     :operation (make-instance 'operation/widget/scroll-pane/scroll
                                               :scroll-pane (input-of -printer-iomap-)
                                               :scroll-delta (* 100 (scroll-of -gesture-))))
                    ((make-instance 'gesture/mouse/scroll :scroll 0 :modifiers '(:shift))
                     :domain "Widget" :description "Scrolls the pane horizontally"
                     :operation (make-instance 'operation/widget/scroll-pane/scroll
                                               :scroll-pane (input-of -printer-iomap-)
                                               :scroll-delta (* 100 (make-2d (2d-y (scroll-of -gesture-)) (2d-x (scroll-of -gesture-)))))))
                  (bind ((content-gesture (progn
                                            ;; KLUDGE: copy gesture
                                            (setf (mouse-position-of -gesture-) (- (mouse-position-of -gesture-) (scroll-position-of (input-of -printer-iomap-))))
                                            -gesture-))
                         (content-input (make-command content-gesture (operation-of -input-) :domain (domain-of -input-) :description (description-of -input-)))
                         (content-command (recurse-reader -recursion- content-input (elt (child-iomaps-of -printer-iomap-) 0)))
                         (content-path `((the ,(document-type (content-of -printer-input-)) (content-of (the widget/scroll-pane document))))))
                    (make-command -gesture-
                                  (operation/extend -printer-input- content-path (operation-of content-command))
                                  :domain (domain-of content-command)
                                  :description (description-of content-command)))
                  (document/read-operation -gesture-)
                  (make-nothing-command -gesture-)))
