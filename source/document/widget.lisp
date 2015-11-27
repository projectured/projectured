;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def special-variable *inset/default*)

(def document inset ()
  ((top :type number)
   (bottom :type number)
   (left :type number)
   (right :type number)))

(def document widget/base ()
  ((visible #t :type boolean)
   (margin *inset/default* :type inset)
   (margin-color nil :type style/color)
   (border *inset/default* :type inset)
   (border-color nil :type style/color)
   (padding *inset/default* :type inset)
   (padding-color nil :type style/color)))

(def document widget/label (widget/base)
  ((position :type 2d)
   (content :type t)))

(def document widget/text (widget/base)
  ((position :type 2d)
   (content :type t)
   (content-fill-color nil :type style/color)))

(def document widget/checkbox (widget/base)
  ((position :type 2d)
   (content :type t)))

(def document widget/button (widget/base)
  ((position :type 2d)
   (size :type 2d)
   (content :type t)))

(def document widget/tooltip (widget/base)
  ((position :type 2d)
   (size :type 2d)
   (content :type t)))

(def document widget/menu (widget/base)
  ((elements :type sequence)))

(def document widget/menu-item (widget/base)
  ((content :type t)))

(def document widget/composite (widget/base)
  ((position :type 2d)
   (elements :type sequence)))

(def document widget/shell (widget/base)
  ((content :type t)
   (content-fill-color nil :type style/color)
   (size :type 2d)
   (tooltip :type widget/tooltip)
   (menu-bar :type widget/menu)
   (context-menu :type widget/menu)))

(def document widget/title-pane (widget/base)
  ((title :type t)
   (title-fill-color :type style/color)
   (content :type t)
   (content-fill-color nil :type style/color)))

(def document widget/split-pane (widget/base)
  ((orientation :type (member :horizontal :vertical))
   (elements :type sequence)
   (sizes :type sequence)))

(def document widget/tabbed-pane (widget/base)
  ((selector-element-pairs :type sequence)
   (selected-index :type integer)))

(def document widget/scroll-pane (widget/base)
  ((content :type t)
   (content-fill-color nil :type style/color)
   (position :type 2d)
   (size :type 2d)
   (scroll-position :type 2d)))

;;;;;;
;;; Construction

(def function make-inset (&key top bottom left right horizontal vertical all)
  (make-instance 'inset
                 :top (or top vertical all 0)
                 :bottom (or bottom vertical all 0)
                 :left (or left horizontal all 0)
                 :right (or right horizontal all 0)))

(def function make-widget/label (position content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/label
         :position position
         :content content
         args))

(def function make-widget/text (position content &rest args &key content-fill-color margin margin-color border border-color padding padding-color selection)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/text
         :position position
         :content content
         args))

(def function make-widget/checkbox (position content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/checkbox
         :position position
         :content content
         args))

(def function make-widget/button (position size content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/button
         :position position
         :size size
         :content content
         args))

(def function make-widget/tooltip (position size content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/tooltip
         :visible #t
         :position position
         :size size
         :content content
         args))

(def function make-widget/menu (elements &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/menu
         :elements elements
         args))

(def function make-widget/menu-item (content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/menu-item
         :content content
         args))

(def function make-widget/composite (position elements &rest args &key margin margin-color border border-color padding padding-color selection)
  (declare (ignore margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/composite
         :position position
         :elements elements
         args))

(def function make-widget/shell (content &rest args &key content-fill-color size margin margin-color border border-color padding padding-color selection)
  (declare (ignore content-fill-color size margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/shell
         :content content
         :tooltip nil
         args))

(def function make-widget/title-pane (title content &rest args &key content-fill-color margin margin-color border border-color padding padding-color title-fill-color selection)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color title-fill-color selection))
  (apply #'make-instance 'widget/title-pane
         :title title
         :content content
         args))

(def function make-widget/split-pane (orientation elements &rest args &key margin margin-color border border-color padding padding-color selection)
  (declare (ignore margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/split-pane
         :orientation (or orientation :horizontal)
         :elements elements
         :sizes nil
         args))

(def function make-widget/tabbed-pane (selector-element-pairs selected-index &rest args &key margin margin-color border border-color padding padding-color selection)
  (declare (ignore margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/tabbed-pane
         :selector-element-pairs selector-element-pairs
         :selected-index selected-index
         args))

(def function make-widget/scroll-pane (content &rest args &key content-fill-color position size margin margin-color border border-color padding padding-color selection)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/scroll-pane
         :content content
         :position position
         :size size
         :scroll-position 0
         args))

;;;;;;
;;; Construction

(def macro widget/label ((&rest args &key position margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/label ,position ,(first content) ,@(remove-from-plist args :position)))

(def macro widget/text ((&rest args &key position content-fill-color margin margin-color border border-color padding padding-color fill-color selection) &body content)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/text ,position ,(first content) ,@(remove-from-plist args :position)))

(def macro widget/checkbox ((&rest args &key position margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/checkbox ,position ,(first content) ,@args))

(def macro widget/button ((&rest args &key position size margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/button ,position ,size ,(first content) ,@args))

(def macro widget/tooltip ((&rest args &key position size margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/tooltip ,position ,size ,(first content) ,@(remove-from-plist args :position :size)))

(def macro widget/menu-item ((&rest args &key margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/menu-item ,(first content) ,@args))

(def macro widget/menu ((&rest args &key margin margin-color border border-color padding padding-color fill-color) &body elements)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/menu (list ,@elements) ,@args))

(def macro widget/shell ((&rest args &key content-fill-color size margin margin-color border border-color padding padding-color fill-color selection) &body content)
  (declare (ignore content-fill-color size margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/shell ,(first content) ,@args))

(def macro widget/composite ((&rest args &key position margin margin-color border border-color padding padding-color fill-color selection) &body elements)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/composite ,position (list ,@elements) ,@(remove-from-plist args :position)))

(def macro widget/title-pane ((&rest args &key title content-fill-color margin margin-color border border-color padding padding-color fill-color title-fill-color selection) &body content)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color fill-color title-fill-color selection))
  `(make-widget/title-pane ,title ,(first content) ,@(remove-from-plist args :title)))

(def macro widget/split-pane ((&rest args &key orientation margin margin-color border border-color padding padding-color fill-color selection) &body elements)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/split-pane ,orientation (list ,@elements) ,@(remove-from-plist args :orientation)))

(def macro widget/tabbed-pane ((&rest args &key margin margin-color border border-color padding padding-color fill-color selection) &body selector-element-pairs)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/tabbed-pane (list ,@(iter (for pair :in-sequence selector-element-pairs)
                                          (collect `(list ,(first pair) ,(second pair)))))
                            0
                            ,@args))

(def macro widget/scroll-pane ((&rest args &key content-fill-color position size margin margin-color border border-color padding padding-color fill-color selection) &body content)
  (declare (ignore content-fill-color position size margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/scroll-pane ,(first content) ,@args))

;;;;;;
;;; Operation

(def operation operation/widget/hide ()
  ((widget :type widget/base)))

(def operation operation/widget/show (operation)
  ((widget :type widget/base)))

(def operation operation/widget/tabbed-pane/select-page (operation)
  ((tabbed-pane :type widget/tabbed-pane)
   (selected-index :type integer)))

(def operation operation/widget/scroll-pane/scroll (operation)
  ((scroll-pane :type widget/scroll-pane)
   (scroll-delta :type 2d)))

;;;;;;
;;; Evaluator

(def evaluator operation/widget/hide (operation)
  (setf (visible-p (widget-of operation)) #f))

(def evaluator operation/widget/show (operation)
  (setf (visible-p (widget-of operation)) #t))

(def evaluator operation/widget/tabbed-pane/select-page (operation)
  (setf (selected-index-of (tabbed-pane-of operation)) (selected-index-of operation)))

(def evaluator operation/widget/scroll-pane/scroll (operation)
  (bind ((scroll-pane (scroll-pane-of operation)))
    (setf (scroll-position-of scroll-pane) (+ (scroll-position-of scroll-pane)
                                              (scroll-delta-of operation)))))

;;;;;
;;; API

(def special-variable *inset/default* (make-inset :all 0))

(def function inset/size (inset)
  (make-2d (+ (left-of inset) (right-of inset))
           (+ (top-of inset) (bottom-of inset))))

(def function inset/width (inset)
  (+ (left-of inset) (right-of inset)))

(def function inset/height (inset)
  (+ (top-of inset) (bottom-of inset)))

(def function inset/top-left (inset)
  (make-2d (left-of inset) (top-of inset)))

(def function inset/top-right (inset)
  (make-2d (right-of inset) (top-of inset)))

(def function inset/bottom-left (inset)
  (make-2d (left-of inset) (bottom-of inset)))

(def function inset/bottom-right (inset)
  (make-2d (right-of inset) (bottom-of inset)))
