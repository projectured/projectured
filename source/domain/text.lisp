;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Text domain provides:
;;;;  - paragraph, chapter, color, font, style
;;;;  - character range selection

;;;;;;
;;; Text document classes

(def document text/base ()
  ())

(def document text/document (text/base)
  ((elements :type sequence)))

(def document text/paragraph (text/base)
  ((elements :type sequence))
  (:documentation "A paragraph provides automatic word wrapping."))

;;;;;;
;;; Text document constructors

(def (function e) make-text/document (elements)
  (make-instance 'text/document :elements elements))

(def (function e) make-text/paragraph (elements)
  (make-instance 'text/paragraph :elements elements))

;;;;;;
;;; Text operation classes

(def operation operation/text/replace-stroke-color (operation)
  ((selection :type selection)
   (color :type vector)))

(def operation operation/text/replace-font (operation)
  ((selection :type selection)
   (font :type t)))

;;;;;;;
;;; Text operation constructors

(def (function e) make-operation/text/replace-stroke-color (selection color)
  (make-instance 'operation/text/replace-stroke-color :selection selection :color color))

(def (function e) make-operation/text/replace-font (selection font)
  (make-instance 'make-operation/text/replace-font :selection selection :font font))

;;;;;;;
;;; Text operation API implementation

(def method redo-operation ((operation operation/text/replace-stroke-color))
  (not-yet-implemented))

(def method redo-operation ((operation operation/text/replace-font))
  (not-yet-implemented))
