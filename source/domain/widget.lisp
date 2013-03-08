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

(def document widget/panel ()
  ((content :type t)
   (location :type 2d)))

(def document widget/composite (widget/base)
  ((elements :type sequence)))

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

(def (function e) make-widget/tooltip (location content &key margin)
  (make-instance 'widget/tooltip
                 :visible #f
                 :location location
                 :content content
                 :margin margin))

(def (function e) make-widget/menu (elements)
  (make-instance 'widget/menu
                 :elements elements))

(def (function e) make-widget/menu-item (content)
  (make-instance 'widget/menu-item
                 :content content))

(def (function e) make-widget/panel (location content)
  (make-instance 'widget/panel
                 :location location
                 :content content))

(def (function e) make-widget/composite (elements)
  (make-instance 'widget/composite
                 :elements elements))

(def (function e) make-widget/scroll-pane (content &key location size margin padding)
  (make-instance 'widget/scroll-pane
                 :content content
                 :location location
                 :size size
                 :scroll-position (make-2d 0 0)
                 :margin margin
                 :padding padding))

;;;;;;
;;; Construction

(def (macro e) tooltip ((&key location margin) &body content)
  `(make-widget/tooltip ,location ,(first content) :margin ,margin))

(def (macro e) menu-item ((&key) &body content)
  `(make-widget/menu-item ,(first content)))

(def (macro e) menu ((&key) &body elements)
  `(make-widget/menu (list ,@elements)))

(def (macro e) panel ((&key location) &body content)
  `(make-widget/panel ,location ,(first content)))

(def (macro e) composite ((&key) &body elements)
  `(make-widget/composite (list ,@elements)))

(def (macro e) scroll-pane ((&key location size margin padding) &body content)
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

(def operation operation/widget/scroll-pane/scroll (operation)
  ((scroll-pane :type widget/scroll-pane)
   (scroll-delta :type 2d)))

;;;;;;
;;; Redo

(def method redo-operation ((operation operation/widget/hide))
  (setf (visible-p (widget-of operation)) #f))

(def method redo-operation ((operation operation/widget/show))
  (setf (visible-p (widget-of operation)) #t))

(def method redo-operation ((operation operation/widget/tooltip/move))
  (setf (location-of (tooltip-of operation)) (location-of operation)))

(def method redo-operation ((operation operation/widget/tooltip/replace-content))
  (setf (content-of (tooltip-of operation)) (content-of operation)))

(def method redo-operation ((operation operation/widget/scroll-pane/scroll))
  (bind ((scroll-pane (scroll-pane-of operation)))
    (setf (scroll-position-of scroll-pane) (+ (scroll-position-of scroll-pane)
                                              (scroll-delta-of operation)))))
