;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Data structure

(def document text/base ()
  ((indentation :type integer)))

(def document text/element (text/base)
  ((font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

(def document text/character (text/element)
  ((content :type character)))

(def document text/string (text/element)
  ((content :type string)))

(def document text/text (text/base)
  ((elements :type sequence)))

(def document text/paragraph (text/base)
  ((elements :type sequence)
   (alignment :type (member :left :center :right :justified))))

;;;;;;
;;; Construction

(def (function e) make-text/character (content &key font font-color fill-color line-color)
  (make-instance 'text/character
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def (function e) make-text/string (content &key font font-color fill-color line-color)
  (make-instance 'text/string
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def (function e) make-text/text (elements)
  (make-instance 'text/text :elements elements))

(def (function e) make-text/paragraph (elements &key alignment)
  (make-instance 'text/paragraph :elements elements :alignment alignment))

;;;;;;
;;; Construction

(def (macro e) text/character (content &key font font-color fill-color line-color)
  `(make-text/character ,content :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/string (content &key font font-color fill-color line-color)
  `(make-text/string ,content :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/newline ()
  `(make-text/string "
" :font *font/default* :font-color *color/default*))

(def (macro e) text/text ((&key) &body elements)
  `(make-text/text (list ,@elements)))

(def (macro e) text/paragraph ((&key alignment) &body elements)
  `(make-text/paragraph (list ,@elements) :alignment ,alignment))

;;;;;;
;;; Operation data structure

(def operation operation/text/base (operation)
  ())

(def operation operation/text/replace-font (operation/text/base)
  ((selection :type selection)
   (font :type style/font)))

(def operation operation/text/replace-font-color (operation/text/base)
  ((selection :type selection)
   (font-color :type style/color)))

;;;;;;;
;;; Operation construction

(def (function e) make-operation/text/replace-font (selection font)
  (make-instance 'make-operation/text/replace-font :selection selection :font font))

(def (function e) make-operation/text/replace-font-color (selection color)
  (make-instance 'operation/text/replace-font-color :selection selection :color color))

;;;;;;;
;;; Operation API

(def method redo-operation ((operation operation/text/replace-font))
  (not-yet-implemented))

(def method redo-operation ((operation operation/text/replace-font-color))
  (not-yet-implemented))

;;;;;;
;;; API

(def (function e) text/length (text)
  (iter (for element :in-sequence (elements-of text))
        (summing
         (typecase element
           (text/string
            (length (content-of element)))
           (t 0)))))

(def (function e) text/substring (text start-element-index start-character-index end-element-index end-character-index)
  (make-text/text
   (iter (with elements = (elements-of text))
         (with elements-length = (length elements))
         (for element-index :from start-element-index :to end-element-index)
         (until (= element-index elements-length))
         (for element = (elt elements element-index))
         (typecase element
           (text/string
            (bind ((content (content-of element))
                   (content-length (length content))
                   (element-start-character-index (if (= element-index start-element-index)
                                                      start-character-index
                                                      0))
                   (element-end-character-index (if (= element-index end-element-index)
                                                    end-character-index
                                                    content-length)))
              (if (and (= element-start-character-index 0)
                       (= element-end-character-index content-length))
                  (collect element)
                  (bind ((word-part (subseq content element-start-character-index element-end-character-index)))
                    (unless (zerop (length word-part))
                      (collect (make-text/string word-part
                                                          :font (font-of element)
                                                          :font-color (font-color-of element)
                                                          :fill-color (fill-color-of element)
                                                          :line-color (line-color-of element))))))))
           (t
            (collect element))))))

(def (function e) text/find (text start-element-index start-character-index test)
  (iter (with elements = (elements-of text))
        (with element-index = start-element-index)
        (for element = (elt elements element-index))
        (for character-index :from start-character-index)
        (typecase element
          (text/string
           (for content = (content-of element))
           (when (= character-index (length content))
             (setf character-index -1)
             (incf element-index)
             (if (= element-index (length elements))
                 (return (values element-index 0))
                 (next-iteration)))
           (when (funcall test (elt content character-index))
             (return (values element-index character-index))))
          (t
           (setf character-index -1)
           (incf element-index)))))

(def (function e) text/count (text character)
  (iter (for element :in-sequence (elements-of text))
        (summing
         (typecase element
           (text/string
            (funcall 'count character (content-of element)))
           (t 0)))))

(def (function e) text/as-string (text)
  (with-output-to-string (stream)
    (iter (for element :in-sequence (elements-of text))
          (typecase element
            (text/string
             (write-string (content-of element) stream))))))

(def (function e) text/split (text split-character)
  (iter (with length = (text/length text))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
        (collect (text/substring text start-element-index start-character-index end-element-index end-character-index))
        (for index = (1+ (text/index text end-element-index end-character-index)))
        (while (< index length))
        (setf start-element-index (text/element-index text index))
        (setf start-character-index (text/character-index text index))))

(def (function e) text/map-split (text split-character function)
  (iter (with elements = (elements-of text))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
        (funcall function start-element-index start-character-index end-element-index end-character-index)
        (while (< end-element-index (length elements)))
        (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
        (while (< start-element-index (length elements)))))

(def (function e) text/next-index (text element-index character-index)
  (bind ((element (elt (elements-of text) element-index)))
    (typecase element
      (text/string
       (if (< (1+ character-index) (length (content-of element)))
           (values element-index (1+ character-index))
           (values (1+ element-index) 0)))
      (t (values (1+ element-index) 0)))))

(def (function e) text/concatenate (&rest style-strings)
  (make-text/text (apply #'append (mapcar #'elements-of style-strings))))

(def (function e) text/element-index (text index)
  (iter (for element-index :from 0)
        (for element :in-sequence (elements-of text))
        (typecase element
          (text/string (decf index (length (content-of element)))))
        (when (<= index 0)
          (return element-index))))

(def (function e) text/character-index (text index)
  (iter (for element :in-sequence (elements-of text))
        (typecase element
          (text/string
           (for length = (length (content-of element)))
           (if (<= index length)
               (return index)
               (decf index length))))))

(def (function e) text/index (text element-index character-index)
  (+ (iter (for index :from 0 :below element-index)
           (for element :in-sequence (elements-of text))
           (summing
            (typecase element
              (text/string
               (length (content-of element)))
              (t 0))))
     character-index))

(def (function e) text/read-operation (document text gesture)
  (cond ((and (typep gesture 'gesture/keyboard/key-press)
              (member (key-of gesture) '(:sdl-key-home :sdl-key-end))
              (equal '(:sdl-key-mod-lctrl) (modifiers-of gesture)))
         (make-operation/replace-selection document
                                           (ecase (key-of gesture)
                                             (:sdl-key-home
                                              ;; KLUDGE: get document reference
                                              `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text (content-of (the document document))))) 0)))) 0)))
                                             (:sdl-key-end
                                              `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text (content-of (the document document)))))
                                                                                                                         ,(1- (length (elements-of text)))))))
                                                                           ,(length (content-of (last-elt (elements-of text))))))))))
        ((and (typep gesture 'gesture/keyboard/key-press)
              (member (key-of gesture) '(:sdl-key-left :sdl-key-right :sdl-key-up :sdl-key-down :sdl-key-home :sdl-key-end :sdl-key-pageup :sdl-key-pagedown))
              (or (null (modifiers-of gesture))
                  (equal (modifiers-of gesture) '(:sdl-key-mod-lctrl))))
         (pattern-case (selection-of document)
           ((the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text ?a))) ?element-index)))) ?character-index))
            (bind ((string (text/as-string text))
                   (string-length (length string))
                   (character-index (text/index text ?element-index ?character-index))
                   (lines (split-sequence #\NewLine string))
                   (height (1+ (count #\NewLine string)))
                   ((:values x y) (iter (with line-begin-index = 0)
                                        (for line-y :from 0)
                                        (for line :in lines)
                                        (for line-end-index = (+ line-begin-index (length line)))
                                        (when (<= line-begin-index character-index line-end-index)
                                          (return (values (- character-index line-begin-index) line-y)))
                                        (setf line-begin-index (1+ line-end-index))))
                   (line (elt lines y))
                   (line-length (length line))
                   (character-before (when (> character-index 0) (elt string (1- character-index))))
                   (character-after (when (< character-index string-length) (elt string character-index)))
                   (new-character-index nil))
              (ecase (key-of gesture)
                (:sdl-key-left
                 (if (equal (modifiers-of gesture) '(:sdl-key-mod-lctrl))
                     (unless (= character-index 0)
                       (setf new-character-index
                             (1+ (or (if (alphanumericp character-before)
                                         (position-if-not 'alphanumericp string :end character-index :from-end #t)
                                         (position-if-not 'alphanumericp string :end (or (position-if 'alphanumericp string :end character-index :from-end #t) 0) :from-end #t))
                                     -1))))
                     (if (= x 0)
                         (unless (= y 0)
                           (decf y)
                           (setf x (length (elt lines y))))
                         (decf x))))
                (:sdl-key-right
                 (if (equal (modifiers-of gesture) '(:sdl-key-mod-lctrl))
                     (unless (= character-index string-length)
                       (setf new-character-index
                             (or (if (alphanumericp character-after)
                                     (position-if-not 'alphanumericp string :start character-index)
                                     (position-if-not 'alphanumericp string :start (or (position-if 'alphanumericp string :start character-index)
                                                                                       string-length)))
                                 string-length)))
                     (if (= x (length line))
                         (unless (= y (1- height))
                           (setf x 0)
                           (incf y))
                         (incf x))))
                (:sdl-key-up
                 (unless (= y 0)
                   (decf y)))
                (:sdl-key-down
                 (unless (= y (1- height))
                   (incf y)))
                (:sdl-key-home
                 (setf x 0))
                (:sdl-key-end
                 (setf x line-length))
                (:sdl-key-pageup
                 (if (> y 10)
                     (decf y 10)
                     (setf y 0)))
                (:sdl-key-pagedown
                 (if (< y (1- (- height 10)))
                     (incf y 10)
                     (setf y (1- height)))))
              (when (> x (length (elt lines y)))
                (setf x (length (elt lines y))))
              (bind ((character-index (or new-character-index
                                          (iter (with line-begin-index = 0)
                                                (for line-y :from 0)
                                                (for line :in lines)
                                                (for line-end-index = (+ line-begin-index (length line) 1))
                                                (when (= y line-y)
                                                  (return (+ line-begin-index x)))
                                                (setf line-begin-index line-end-index))))
                     (selection `(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text ,?a)))
                                                                                                           ,(text/element-index text character-index)))))
                                                             ,(text/character-index text character-index)))))
                (make-operation/replace-selection document selection))))))
        ((and (typep gesture 'gesture/keyboard/key-press)
              (null (set-difference (modifiers-of gesture) '(:sdl-key-lshift :sdl-key-mod-rshift)))
              (or (graphic-char-p (character-of gesture))
                  (whitespace? (character-of gesture))))
         (bind ((character (character-of gesture))
                (replacement (cond ((eq character #\Return)
                                    (string #\NewLine))
                                   (t (string character)))))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b))
              (make-operation/number/replace-range document (selection-of document) replacement))
             ((the sequence-position (pos (the string ?a) ?b))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (selection-of document) replacement)
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1+ ?b))))))))))
        ((key-press? gesture :key :sdl-key-delete)
         (pattern-case (selection-of document)
           ((the sequence-position (pos (the string ?a) ?b))
            (when (< ?b (text/length (content-of document)))
              (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,?b ,(1+ ?b))) "")))))
        ((key-press? gesture :key :sdl-key-backspace)
         (pattern-case (selection-of document)
           ((the sequence-position (pos (the string ?a) ?b))
            (when (> ?b 0)
              (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,(1- ?b) ,?b)) "")
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1- ?b))))))))))))
