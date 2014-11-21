;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/search->widget/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/search->widget/text ()
  (make-projection 'document/search->widget/text))

;;;;;;
;;; Construction

(def macro document/search->widget/text ()
  '(make-projection/document/search->widget/text))

;;;;;;
;;; Printer

(def printer document/search->widget/text (projection recursion input input-reference)
  (bind ((search-string (search-of input))
         (empty? (zerop (length search-string)))
         (output (widget/text (:location (make-2d 0 0) :margin (make-inset :all 5))
                   (text/text () (text/string (if empty?
                                                  "enter text"
                                                  search-string)
                                              :font *font/ubuntu/regular/24*
                                              :font-color (if empty? *color/solarized/gray* *color/black*))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader document/search->widget/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
