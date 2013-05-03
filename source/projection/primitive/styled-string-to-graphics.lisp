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
    (make-iomap/recursive projection recursion input input-reference output output-reference
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
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (member (key-of latest-gesture) '(:sdl-key-home :sdl-key-end))
                (equal '(:sdl-key-mod-lctrl) (modifiers-of latest-gesture)))
           (make-operation/replace-selection document
                                             (ecase (key-of latest-gesture)
                                               (:sdl-key-home
                                                ;; KLUDGE: get document reference
                                                `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text (content-of (the document document))))) 0)))) 0)))
                                               (:sdl-key-end
                                                `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text (content-of (the document document)))))
                                                                                                                           ,(1- (length (elements-of input)))))))
                                                                             ,(length (content-of (last-elt (elements-of input))))))))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (member (key-of latest-gesture) '(:sdl-key-left :sdl-key-right :sdl-key-up :sdl-key-down :sdl-key-home :sdl-key-end :sdl-key-pageup :sdl-key-pagedown))
                (null (modifiers-of latest-gesture)))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text ?a))) ?element-index)))) ?character-index))
              (bind ((string (text/as-string input))
                     (character-index (text/index input ?element-index ?character-index))
                     (lines (split-sequence #\NewLine string))
                     (width (iter (for line :in lines)
                                  (maximizing (1+ (length line)))))
                     (height (1+ (count #\NewLine string)))
                     ((:values x y) (iter (with line-begin-index = 0)
                                          (for line-y :from 0)
                                          (for line :in lines)
                                          (for line-end-index = (+ line-begin-index (length line)))
                                          (when (<= line-begin-index character-index line-end-index)
                                            (return (values (- character-index line-begin-index) line-y)))
                                          (setf line-begin-index (1+ line-end-index)))))
                (ecase (key-of latest-gesture)
                  (:sdl-key-left (decf x))
                  (:sdl-key-right (incf x))
                  (:sdl-key-up (decf y))
                  (:sdl-key-down (incf y))
                  (:sdl-key-home (setf x 0))
                  (:sdl-key-end (setf x width))
                  (:sdl-key-pageup (decf y 10))
                  (:sdl-key-pagedown (incf y 10)))
                (when (< x 0)
                  (setf x 0))
                (when (< y 0)
                  (setf y 0))
                (when (>= x width)
                  (setf x (1- width)))
                (when (>= y height)
                  (setf y (1- height)))
                (bind ((character-index (iter (with line-begin-index = 0)
                                              (for line-y :from 0)
                                              (for line :in lines)
                                              (for line-end-index = (+ line-begin-index 1 (length line)))
                                              (when (= y line-y)
                                                (return (+ line-begin-index (if (> x (length line))
                                                                                (length line)
                                                                                x))))
                                              (setf line-begin-index line-end-index)))
                       (selection `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text ,?a)))
                                                                                                             ,(text/element-index input character-index)))))
                                                               ,(text/character-index input character-index)))))
                  (make-operation/replace-selection document selection))))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (null (set-difference (modifiers-of latest-gesture) '(:sdl-key-lshift :sdl-key-mod-rshift)))
                (or (graphic-char-p (character-of latest-gesture))
                    (whitespace? (character-of latest-gesture))))
           (bind ((character (character-of latest-gesture))
                  (replacement (cond ((eq character #\Return)
                                      (string #\NewLine))
                                     (t (string character)))))
             (pattern-case (selection-of document)
               ((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b))
                (make-operation/number/replace-range document (selection-of document) replacement))
               ((the sequence-position (pos (the string ?a) ?b))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (selection-of document) replacement)
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1+ ?b))))))))))
          ((key-press? latest-gesture :key :sdl-key-delete)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string ?a) ?b))
              (when (< ?b (text/length (content-of document)))
                (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,?b ,(1+ ?b))) "")))))
          ((key-press? latest-gesture :key :sdl-key-backspace)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string ?a) ?b))
              (when (> ?b 0)
                (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,(1- ?b) ,?b)) "")
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1- ?b)))))))))))))
