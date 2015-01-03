;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

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
  ((location :type 2d)
   (content :type t)))

(def document widget/text (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/checkbox (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/button (widget/base)
  ((location :type 2d)
   (size :type 2d)
   (content :type t)))

(def document widget/tooltip (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/menu (widget/base)
  ((elements :type sequence)))

(def document widget/menu-item (widget/base)
  ((content :type t)))

(def document widget/composite (widget/base)
  ((location :type 2d)
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
   (location :type 2d)
   (size :type 2d)
   (scroll-position :type 2d)))

;;;;;;
;;; Construction

(def function make-inset (&key top bottom left right horizontal vertical all)
  (make-instance 'inset
                 :top (or top vertical all)
                 :bottom (or bottom vertical all)
                 :left (or left horizontal all)
                 :right (or right horizontal all)))

(def function make-widget/label (location content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/label
         :location location
         :content content
         args))

(def function make-widget/text (location content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/text
         :location location
         :content content
         args))

(def function make-widget/checkbox (location content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/checkbox
         :location location
         :content content
         args))

(def function make-widget/button (location size content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/button
         :location location
         :size size
         :content content
         args))

(def function make-widget/tooltip (location content &rest args &key margin margin-color border border-color padding padding-color)
  (declare (ignore margin margin-color border border-color padding padding-color))
  (apply #'make-instance 'widget/tooltip
         :visible #f
         :location location
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

(def function make-widget/composite (location elements &rest args &key margin margin-color border border-color padding padding-color selection)
  (declare (ignore margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/composite
         :location location
         :elements elements
         args))

(def function make-widget/shell (content &rest args &key content-fill-color size margin margin-color border border-color padding padding-color selection)
  (declare (ignore content-fill-color size margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/shell
         :content content
         :tooltip (make-widget/tooltip (make-2d 0 0) nil :margin (make-inset :all 5))
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

(def function make-widget/scroll-pane (content &rest args &key content-fill-color location size margin margin-color border border-color padding padding-color selection)
  (declare (ignore content-fill-color margin margin-color border border-color padding padding-color selection))
  (apply #'make-instance 'widget/scroll-pane
         :content content
         :location location
         :size size
         ;; TODO: make all slots of computed-class computed by default
         :scroll-position (as (make-2d 0 0))
         args))

;;;;;;
;;; Construction

(def macro widget/label ((&rest args &key location margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/label ,location ,(first content) ,@args))

(def macro widget/text ((&rest args &key location margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/text ,location ,(first content) ,@args))

(def macro widget/checkbox ((&rest args &key location margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/checkbox ,location ,(first content) ,@args))

(def macro widget/button ((&rest args &key location size margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/button ,location ,size ,(first content) ,@args))

(def macro widget/tooltip ((&rest args &key location margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/tooltip ,location ,(first content) ,@args))

(def macro widget/menu-item ((&rest args &key margin margin-color border border-color padding padding-color fill-color) &body content)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/menu-item ,(first content) ,@args))

(def macro widget/menu ((&rest args &key margin margin-color border border-color padding padding-color fill-color) &body elements)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color))
  `(make-widget/menu (list ,@elements) ,@args))

(def macro widget/shell ((&rest args &key content-fill-color size margin margin-color border border-color padding padding-color fill-color selection) &body content)
  (declare (ignore content-fill-color size margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/shell ,(first content) ,@args))

(def macro widget/composite ((&rest args &key location margin margin-color border border-color padding padding-color fill-color selection) &body elements)
  (declare (ignore margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/composite ,location (list ,@elements) ,@args))

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

(def macro widget/scroll-pane ((&rest args &key content-fill-color location size margin margin-color border border-color padding padding-color fill-color selection) &body content)
  (declare (ignore content-fill-color location size margin margin-color border border-color padding padding-color fill-color selection))
  `(make-widget/scroll-pane ,(first content) ,@args))

;;;;;;
;;; Operation data structure

(def operation operation/widget/hide (operation)
  ((widget :type widget/base)))

(def operation operation/widget/show (operation)
  ((widget :type widget/base)))

(def operation operation/widget/tooltip/move (operation)
  ((tooltip :type tooltip)
   (location :type location)))

(def operation operation/widget/tooltip/replace-content (operation)
  ((tooltip :type tooltip)
   (content :type content)))

(def operation operation/widget/tabbed-pane/select-page (operation)
  ((tabbed-pane :type widget/tabbed-pane)
   (selected-index :type integer)))

(def operation operation/widget/scroll-pane/scroll (operation)
  ((scroll-pane :type widget/scroll-pane)
   (scroll-delta :type 2d)))

;;;;;;
;;; Run

(def method run-operation ((operation operation/widget/hide))
  (setf (visible-p (widget-of operation)) #f))

(def method run-operation ((operation operation/widget/show))
  (setf (visible-p (widget-of operation)) #t))

(def method run-operation ((operation operation/widget/tooltip/move))
  (setf (location-of (tooltip-of operation)) (location-of operation)))

(def method run-operation ((operation operation/widget/tooltip/replace-content))
  (setf (content-of (tooltip-of operation)) (content-of operation)))

(def method run-operation ((operation operation/widget/tabbed-pane/select-page))
  (setf (selected-index-of (tabbed-pane-of operation)) (selected-index-of operation)))

(def method run-operation ((operation operation/widget/scroll-pane/scroll))
  (bind ((scroll-pane (scroll-pane-of operation)))
    (setf (scroll-position-of scroll-pane) (+ (scroll-position-of scroll-pane)
                                              (scroll-delta-of operation)))))

;;;;;
;;; API

(def special-variable *inset/default* (make-inset :all 0))

(def function inset/size (inset)
  (make-2d (+ (left-of inset)
              (right-of inset))
           (+ (top-of inset)
              (bottom-of inset))))

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
