;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection styled-string->graphics ()
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

(def printer styled-string->graphics (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (elements (iter outer
                         (with y = 0)
                         (with x = 0)
                         (with height = 0)
                         (with output-index = 0)
                         (for input-index :from 0)
                         (for element :in (elements-of input))
                         (etypecase element
                           (text/string
                            (bind ((content (content-of element))
                                   (styled-string-reference `(content-of (the text/string (elt (the list (elements-of ,typed-input-reference)) ,input-index)))))
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
                                                                    :font (font-of element)
                                                                    :font-color (font-color-of element)
                                                                    :fill-color (fill-color-of element)))
                                    (for size = (size-of (make-bounding-rectangle text)))
                                    (incf x (2d-x size))
                                    (setf height (max height (2d-y size)))
                                    (in outer (collect text))
                                    (incf output-index)
                                    (incf content-index (1+ (length line))))))
                           (image/image
                            (bind ((image (make-graphics/image (make-2d x y) (make-image/image (filename-of element))))
                                   (size (size-of (make-bounding-rectangle image))))
                              (in outer (collect image))
                              (incf x (2d-x size))
                              (setf height (max height (2d-y size))))))))
         (output (make-graphics/canvas elements (make-2d 0 0))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader styled-string->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion operation))
  (bind ((input (input-of projection-iomap))
         (document (input-of document-iomap))
         (latest-gesture (first (gestures-of gesture-queue))))
    (cond ((and (typep latest-gesture 'gesture/mouse/button/click)
                (eq (button-of latest-gesture) :button-left))
           (bind ((graphics-reference (make-reference (output-of printer-iomap) (location-of latest-gesture)
                                                      `(printer-output (the ,(form-type (input-of printer-iomap)) document)
                                                                       ,(projection-of printer-iomap)
                                                                       ,(recursion-of printer-iomap))))
                  (domain-reference nil))
             (map-backward projection-iomap graphics-reference
                           (lambda (iomap reference)
                             (declare (ignore iomap))
                             (setf domain-reference reference)))
             (pattern-case domain-reference
               ((the character (elt (the string ?a) ?b))
                (make-operation/replace-selection document
                                                  ;; KLUDGE: get document reference
                                                  (tree-replace `(the sequence-position (pos (the string ,?a) ,?b))
                                                                (input-reference-of projection-iomap)
                                                                '(content-of (the document document))))))))
          (t (text/read-operation document input latest-gesture)))))
