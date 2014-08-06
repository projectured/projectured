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
  ((visible :type boolean)
   (border :type inset)
   (border-color :type style/color)
   (margin :type inset)
   (margin-color :type style/color)
   (padding :type inset)
   (padding-color :type style/color)))

(def document widget/label (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/checkbox (widget/base)
  ())

(def document widget/button (widget/base)
  ())

(def document widget/text-field (widget/base)
  ())

(def document widget/tooltip (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/menu (widget/base)
  ((elements :type sequence)))

(def document widget/menu-item (widget/base)
  ((content :type t)))

(def document widget/shell (widget/base)
  ((content :type t)
   (tooltip :type widget/tooltip)
   (menu-bar :type widget/menu)
   (context-menu :type widget/menu)))

(def document widget/composite (widget/base)
  ((elements :type sequence)))

(def document widget/split-pane (widget/base)
  ((orientation :type (member :horizontal :vertical))
   (elements :type sequence)
   (sizes :type sequence)))

(def document widget/tabbed-pane (widget/base)
  ((selector-element-pairs :type sequence)
   (selected-index :type integer)))

(def document widget/scroll-pane (widget/base)
  ((content :type t)
   (location :type 2d)
   (size :type 2d)
   (scroll-position :type 2d)))

;;;;;;
;;; Construction

(def (function e) make-inset (&key top bottom left right horizontal vertical all)
  (make-instance 'inset
                 :top (or top vertical all)
                 :bottom (or bottom vertical all)
                 :left (or left horizontal all)
                 :right (or right horizontal all)))

(def (function e) make-widget/label (location content &key margin)
  (make-instance 'widget/label
                 :location location
                 :content content
                 :margin margin))

(def (function e) make-widget/tooltip (location content &key margin)
  (make-instance 'widget/tooltip
                 :visible #f
                 :location location
                 :content content
                 :margin margin))

(def (function e) make-widget/menu (elements &key margin)
  (make-instance 'widget/menu
                 :visible #t
                 :elements elements
                 :margin margin))

(def (function e) make-widget/menu-item (content &key margin)
  (make-instance 'widget/menu-item
                 :visible #t
                 :content content
                 :margin (or margin (make-inset :all 5))))

(def (function e) make-widget/shell (content &key margin padding)
  (make-instance 'widget/shell
                 :content content
                 :margin margin
                 :padding padding
                 :tooltip (make-widget/tooltip (make-2d 0 0) nil :margin (make-inset :all 5))))

(def (function e) make-widget/composite (elements)
  (make-instance 'widget/composite
                 :elements elements))

(def (function e) make-widget/split-pane (orientation elements &key selection)
  (make-instance 'widget/split-pane
                 :orientation orientation
                 :elements elements
                 :sizes nil
                 :selection selection))

(def (function e) make-widget/tabbed-pane (selector-element-pairs selected-index)
  (make-instance 'widget/tabbed-pane
                 :selector-element-pairs selector-element-pairs
                 :selected-index selected-index))

(def (function e) make-widget/scroll-pane (content &key location size margin padding)
  (make-instance 'widget/scroll-pane
                 :content content
                 :location location
                 :size size
                 ;; TODO: make all slots of computed-class computed by default
                 :scroll-position (as (make-2d 0 0))
                 :margin margin
                 :padding padding))

;;;;;;
;;; Construction

;; TODO: add widget prefix

(def (macro e) widget/label ((&key location margin) &body content)
  `(make-widget/label ,location ,(first content) :margin ,margin))

(def (macro e) widget/tooltip ((&key location margin) &body content)
  `(make-widget/tooltip ,location ,(first content) :margin ,margin))

(def (macro e) widget/menu-item ((&key) &body content)
  `(make-widget/menu-item ,(first content)))

(def (macro e) widget/menu ((&key) &body elements)
  `(make-widget/menu (list ,@elements)))

(def (macro e) widget/shell ((&key margin padding) &body content)
  `(make-widget/shell ,(first content) :margin ,margin :padding ,padding))

(def (macro e) widget/composite ((&key) &body elements)
  `(make-widget/composite (list ,@elements)))

(def (macro e) widget/split-pane ((&key orientation selection) &body elements)
  `(make-widget/split-pane ,orientation (list ,@elements) :selection ,selection))

(def (macro e) widget/tabbed-pane ((&key) &body selector-element-pairs)
  `(make-widget/tabbed-pane (list ,@(iter (for pair :in-sequence selector-element-pairs)
                                          (collect `(list ,(first pair) ,(second pair)))))
                            0))

(def (macro e) widget/scroll-pane ((&key location size margin padding) &body content)
  `(make-widget/scroll-pane ,(first content)
                            :location ,location
                            :size ,size
                            :margin ,margin
                            :padding ,padding))

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
;;; Redo

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
