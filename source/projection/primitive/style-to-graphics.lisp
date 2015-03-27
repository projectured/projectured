;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection style/color->graphics/rectangle ()
  ())

(def projection style/font->graphics/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/style/color->graphics/rectangle ()
  (make-projection 'style/color->graphics/rectangle))

(def function make-projection/style/font->graphics/text ()
  (make-projection 'style/font->graphics/text))

;;;;;;
;;; Construction

(def macro style/color->graphics/rectangle ()
  '(make-projection/style/color->graphics/rectangle))

(def macro style/font->graphics/text ()
  '(make-projection/style/font->graphics/text))

;;;;;;
;;; Printer

(def printer style/color->graphics/rectangle (projection recursion input input-reference)
  (bind ((output (make-graphics/rounded-rectangle (make-2d 0 0) (make-2d 100 50) 9 :fill-color input)))
    (make-iomap/object projection recursion input input-reference output)))

(def printer style/font->graphics/text (projection recursion input input-reference)
  (bind ((output (make-graphics/text (make-2d 0 0) "lorem ipsum" :font input :font-color *color/default*)))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader style/color->graphics/rectangle (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader style/font->graphics/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
