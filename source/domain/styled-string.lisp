;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Data structure

(def document styled-string/base ()
  ())

(def document styled-string/document (styled-string/base)
  ((elements :type sequence)))

(def document styled-string/string (styled-string/base)
  ((content :type string)
   (font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

;;;;;;
;;; Construction

(def (function e) make-styled-string/document (elements)
  (make-instance 'styled-string/document :elements elements))

(def (function e) make-styled-string/string (content &key font font-color fill-color line-color)
  (make-instance 'styled-string/string
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

;;;;;;
;;; API

(def (function e) styled-string/substring (styled-string start-element-index start-character-index end-element-index end-character-index)
  (make-styled-string/document
   (iter (with elements = (elements-of styled-string))
         (for element-index :from start-element-index :to end-element-index)
         (until (= element-index (length elements)))
         (for element = (elt elements element-index))
         (for font = (font-of element))
         (for font-color = (font-color-of element))
         (for content = (content-of element))
         (for word-part = (subseq content
                                  (if (= element-index start-element-index)
                                      start-character-index
                                      0)
                                  (if (= element-index end-element-index)
                                      end-character-index
                                      (length content))))
         (unless (zerop (length word-part))
           (collect (make-styled-string/string word-part :font font :font-color font-color))))))

(def (function e) styled-string/find (styled-string start-element-index start-character-index test)
  (iter (with elements = (elements-of styled-string))
        (with element-index = start-element-index)
        (for element = (elt elements element-index))
        (for content = (content-of element))
        (for character-index :from start-character-index)
        (when (= character-index (length content))
          (setf character-index -1)
          (incf element-index)
          (if (= element-index (length elements))
              (return (values element-index 0))
              (next-iteration)))
        (when (funcall test (elt content character-index))
          (return (values element-index character-index)))))
