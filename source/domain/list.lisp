;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Data structure

(def document list/base ()
  ())

(def document list/list (list/base)
  ((elements :type sequence)))

(def document list/element (list/base)
  ((content :type t)))

;;;;;;
;;; Construction

(def (function e) make-list/list (elements)
  (make-instance 'list/list :elements elements))

(def (function e) make-list/element (content)
  (make-instance 'list/element :content content))

;;;;;;
;;; Construction

(def (macro e) list/list (() &body elements)
  `(make-list/list (list ,@elements)))

(def (macro e) list/element (() &body content)
  `(make-list/element ,(first content)))
