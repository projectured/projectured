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
