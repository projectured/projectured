;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) styled-string->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/styled-string->graphics ()
  (make-projection 'styled-string->graphics))

;;;;;;
;;; Construction

(def (macro e) styled-string->graphics ()
  `(make-projection/styled-string->graphics))

;;;;;;
;;; Printer

(def printer styled-string->graphics (projection recursion input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (elements (iter outer
                         (with y = 0)
                         (with x = 0)
                         (with height = 0)
                         (with output-index = 0)
                         (for input-index :from 0)
                         (for element :in (elements-of input))
                         (for content = (content-of element))
                         (for styled-string-reference = `(content-of (the styled-string/string (elt (the list (elements-of ,typed-input-reference)) ,input-index))))
                         (iter (with content-index = 0)
                               (for line :in (split-sequence #\NewLine content))
                               (for text-reference = `(text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ,output-reference))) ,output-index))))
                               (unless (first-iteration-p)
                                 (setf x 0)
                                 (incf y height)
                                 (setf height 0))
                               (push (make-iomap/string content styled-string-reference content-index
                                                        line text-reference 0 (length line))
                                     child-iomaps)
                               (for text = (make-graphics/text (make-2d x y) line
                                                               :color (color-of element)
                                                               :font (font-of element)))
                               (for size = (size-of (make-bounding-rectangle text)))
                               (incf x (2d-x size))
                               (setf height (max height (2d-y size)))
                               (in outer (collect text))
                               (incf output-index)
                               (incf content-index (1+ (length line))))))
         (output (make-graphics/canvas elements (make-2d 0 0))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader styled-string->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
