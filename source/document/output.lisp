;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document output/base ()
  ())

(def document output/display (output/base)
  ((windows :type sequence)))

(def document output/window (output/base)
  ((position :type 2d)
   (size :type 2d)
   (content :type t)
   (title :type string)))

;;;;;;
;;; Construction

(def function make-output/display (windows &key selection)
  (make-instance 'output/display :windows windows :selection selection))

(def function make-output/window (position size content &key title selection)
  (make-instance 'output/window :position position :size size :content content :title title :selection selection))

;;;;;;
;;; Construction

(def macro output/display ((&key selection) &body windows)
  `(make-output/display (list ,@windows) :selection ,selection))

(def macro output/window ((&key position size title selection) &body content)
  `(make-output/window ,position ,size ,(first content) :title ,title :selection ,selection))
