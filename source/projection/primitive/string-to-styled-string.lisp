;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) string->styled-string ()
  ((font-provider :type function)
   (font-color-provider :type function)
   (fill-color-provider :type function)
   (line-color-provider :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/string->styled-string (&key font-provider font-color-provider fill-color-provider line-color-provider)
  (make-projection 'string->styled-string
                   :font-provider font-provider
                   :font-color-provider font-color-provider
                   :fill-color-provider fill-color-provider
                   :line-color-provider line-color-provider))

;;;;;;
;;; Construction

(def (macro e) string->styled-string (&key font-provider font-color-provider fill-color-provider line-color-provider)
  `(make-projection/string->styled-string :font-provider ,font-provider
                                          :font-color-provider ,font-color-provider
                                          :fill-color-provider ,fill-color-provider
                                          :line-color-provider ,line-color-provider))

;;;;;;
;;; Printer

(def printer string->styled-string (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (stream (make-string-output-stream))
         (font *font/default*)
         (font-color *color/default*)
         (fill-color nil)
         (line-color nil)
         (input-offset 0)
         (output-index 0)
         (elements (flet ((next-styled-string ()
                            (bind ((content (get-output-stream-string stream)))
                              (unless (string= content "")
                                (push (make-iomap/string input input-reference input-offset content
                                                         `(content-of (the styled-string/string (elt (the list (elements-of (the styled-string/document ,output-reference))) ,output-index))) 0 (length content))
                                      child-iomaps)
                                (incf output-index)
                                (list (make-styled-string/string content :font font :font-color font-color :fill-color fill-color :line-color line-color))))))
                     (iter (with font-provider = (font-provider-of projection))
                           (with font-color-provider = (font-color-provider-of projection))
                           (with fill-color-provider = (fill-color-provider-of projection))
                           (with line-color-provider = (line-color-provider-of projection))
                           (for character-index :from 0)
                           (for character :in-sequence input)
                           (for character-reference = `(the character (elt (the string ,input-reference) ,character-index)))
                           (for character-font = (or (when font-provider (funcall font-provider iomap character-reference))
                                                     *font/default*))
                           (for character-font-color = (or (when font-color-provider (funcall font-color-provider iomap character-reference))
                                                           *color/default*))
                           (for character-fill-color = (or (when fill-color-provider (funcall fill-color-provider iomap character-reference))
                                                           nil))
                           (for character-line-color = (or (when line-color-provider (funcall line-color-provider iomap character-reference))
                                                           nil))
                           (when (or (not (color= font-color character-font-color))
                                     (not (color=* fill-color character-fill-color))
                                     (not (color=* line-color character-line-color))
                                     (not (eq font character-font)))
                             (appending (next-styled-string) :into styled-strings)
                             (setf stream (make-string-output-stream))
                             (setf input-offset character-index)
                             (setf font character-font)
                             (setf font-color character-font-color)
                             (setf fill-color character-fill-color)
                             (setf line-color character-line-color))
                           (write-char character stream)
                           (finally
                            (return (append styled-strings (next-styled-string)))))))
         (output (make-styled-string/document elements)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader string->styled-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore recursion operation))
  (bind ((input (input-of projection-iomap))
         (latest-gesture (first (gestures-of gesture-queue))))
    (cond ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-c)
                (member :sdl-key-mod-lctrl (modifiers-of latest-gesture)))
           ;; TODO: create operation
           (setf (font-color-provider-of projection) (constantly *color/black*))
           (make-operation/compound nil))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (member (key-of latest-gesture) '(:sdl-key-home :sdl-key-end))
                (member :sdl-key-mod-lctrl (modifiers-of latest-gesture)))
           (bind ((index (ecase (key-of latest-gesture)
                           (:sdl-key-home 0)
                           (:sdl-key-end (length (content-of document))))))
             (make-operation/replace-selection document
                                               ;; KLUDGE: get document reference
                                               `(the sequence-position (pos (the string (content-of (the document document))) ,index)))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (member (key-of latest-gesture) '(:sdl-key-left :sdl-key-right :sdl-key-up :sdl-key-down :sdl-key-home :sdl-key-end :sdl-key-pageup :sdl-key-pagedown)))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string ?a) ?character-index))
              (bind ((lines (split-sequence #\NewLine input))
                     (width (iter (for line :in lines)
                                  (maximizing (1+ (length line)))))
                     (height (1+ (count #\NewLine input)))
                     ((:values x y) (iter (with line-begin-index = 0)
                                          (for line-y :from 0)
                                          (for line :in lines)
                                          (for line-end-index = (+ line-begin-index 1 (length line)))
                                          (when (<= line-begin-index ?character-index line-end-index)
                                            (return (values (- ?character-index line-begin-index) line-y)))
                                          (setf line-begin-index line-end-index))))
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
                                              (setf line-begin-index line-end-index))))
                  (make-operation/replace-selection document
                                                    `(the sequence-position (pos (the string ,?a) ,character-index))))))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (null (set-difference (modifiers-of latest-gesture) '(:sdl-key-lshift :sdl-key-mod-rshift)))
                (graphic-char-p (character-of latest-gesture)))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b))
              (make-operation/number/replace-range document (selection-of document) (string (character-of latest-gesture))))
             ((the sequence-position (pos (the string ?a) ?b))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (selection-of document) (string (character-of latest-gesture)))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1+ ?b)))))))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-delete)
                (null (modifiers-of latest-gesture)))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string ?a) ?b))
              (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,?b ,(1+ ?b))) ""))))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-backspace))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string ?a) ?b))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,(1- ?b) ,?b)) "")
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1- ?b)))))))))
          ((and (typep latest-gesture 'gesture/mouse/button/click)
                (eq (button-of latest-gesture) :button-left))
           (bind ((graphics-reference (make-reference (output-of printer-iomap) (location-of latest-gesture)
                                                      `(printer-output (the ,(form-type (input-of printer-iomap)) document)
                                                                       ,(projection-of printer-iomap)
                                                                       ,(recursion-of printer-iomap))))
                  (domain-reference nil))
             (map-backward printer-iomap graphics-reference
                           (lambda (iomap reference)
                             (declare (ignore iomap))
                             (setf domain-reference reference)))
             (pattern-case domain-reference
               ((the character (elt (the string ?a) ?b))
                (make-operation/replace-selection document
                                                  ;; KLUDGE: get document reference
                                                  (tree-replace `(the sequence-position (pos (the string ,?a) ,?b))
                                                                '(content-of (the widget/scroll-pane (elt (the list (elements-of (the widget/composite document))) 0)))
                                                                'document)))))))))
