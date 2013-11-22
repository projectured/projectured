;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text->graphics ()
  (make-projection 'text->graphics))

;;;;;;
;;; Construction

(def (macro e) text->graphics ()
  `(make-projection/text->graphics))

;;;;;;
;;; Printer

(def printer text->graphics (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (elements (iter outer
                         (with y = 0)
                         (with x = 0)
                         (with height = 0)
                         (with output-index = 0)
                         (with content-index = 0)
                         (for input-index :from 0)
                         (for element :in-sequence (elements-of input))
                         (etypecase element
                           (text/string
                            (bind ((content (content-of element)))
                              (iter (for line :in (split-sequence #\NewLine content))
                                    (for text-reference = `(the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ,output-reference))) ,output-index)))))
                                    (unless (first-iteration-p)
                                      (setf x 0)
                                      (incf y height)
                                      (setf height 0))
                                    (push (make-iomap/text* projection recursion
                                                            input typed-input-reference content-index
                                                            line text-reference 0
                                                            (length line))
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
                                    (incf content-index (length line))
                                    (unless (first-iteration-p)
                                      (incf content-index)))))
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

(def reader text->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion operation))
  (bind ((input (input-of projection-iomap))
         (document (input-of document-iomap))
         (latest-gesture (first (gestures-of gesture-queue)))
         (text-operation (text/read-operation document input latest-gesture)))
    (merge-operations (gesture-case latest-gesture
                        ((make-instance 'gesture/mouse/button/click :button :button-left :modifiers nil)
                         :domain "Widget" :help "Moves the selection to where the mouse is pointing at"
                         :operation (bind ((graphics-reference (make-reference (output-of printer-iomap)
                                                                               (if (typep latest-gesture 'gesture/mouse/button/click)
                                                                                   (location-of latest-gesture)
                                                                                   (mouse-position))
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
                                                                                         '(content-of (the document document)))))
                                        ((the character (text/elt (the text/text ?a) ?b))
                                         (make-operation/replace-selection document
                                                                           ;; KLUDGE: get document reference
                                                                           (tree-replace `(the sequence-position (text/pos (the text/text ,?a) ,?b))
                                                                                         (input-reference-of projection-iomap)
                                                                                         '(content-of (the document document)))))))))
                      text-operation)))
