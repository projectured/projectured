;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def method raw-of :around ((color style/color))
  (or (call-next-method)
      (setf (raw-of color) (format nil "#~2,'0X~2,'0X~2,'0X" (red-of color) (green-of color) (blue-of color)))))
