;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document clipboard/base ()
  ())

(def document clipboard/slice (clipboard/base)
  ((content :type t)
   (slice :type t)))

(def document clipboard/collection (clipboard/base)
  ((content :type t)
   (elements :type sequence)))

;;;;;;
;;; Construction

(def function make-clipboard/slice (content &key slice selection)
  (make-instance 'clipboard/slice :content content :slice slice :selection selection))

(def function make-clipboard/collection (content &key elements selection)
  (make-instance 'clipboard/collection :content content :elements elements :selection selection))

;;;;;;
;;; Construction

(def macro clipboard/slice ((&key slice selection) &body content)
  `(make-clipboard/slice ,(first content) :selection ,selection :slice ,slice))

(def macro clipboard/collection ((&key elements selection) &body content)
  `(make-clipboard/collection ,(first content) :elements (list ,@elements) :selection ,selection))
