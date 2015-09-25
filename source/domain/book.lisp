;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document book/base ()
  ((collapsed :type boolean)))

(def document book/book (book/base)
  ((title :type string)
   (author :type sequence)
   (elements :type sequence)))

(def document book/chapter (book/base)
  ((title :type string)
   (numbering :type string)
   (elements :type sequence)))

(def document book/paragraph (book/base)
  ((alignment :type (member :left :center :right :justified))
   (content :type text/text)))

(def document book/list (book/base)
  ((elements :type sequence)))

(def document book/picture (book/base)
  ((title :type string)
   (alignment :type (member :left :center :right :justified))
   (content :type image/file)))

;;;;;;
;;; Construction

(def function make-book/book (elements &key (title "") author collapsed selection)
  (make-instance 'book/book :title title :author author :elements elements :collapsed collapsed :selection selection))

(def function make-book/chapter (elements &key (title "") numbering collapsed selection)
  (make-instance 'book/chapter :title title :numbering numbering :collapsed collapsed :elements elements :selection selection))

(def function make-book/paragraph (content &key (alignment :left) collapsed selection)
  (make-instance 'book/paragraph :content content :alignment alignment :collapsed collapsed :selection selection))

(def function make-book/list (elements &key collapsed selection)
  (make-instance 'book/list :elements elements :collapsed collapsed :selection selection))

(def function make-book/picture (content &key title (alignment :left) collapsed selection)
  (make-instance 'book/picture :content content :title title :alignment alignment :collapsed collapsed :selection selection))

;;;;;;
;;; Construction

(def macro book/book ((&key (title "") author collapsed key predicate selection) &body elements)
  `(make-book/book (document/sequence (:key ,key :predicate ,predicate) ,@elements)
                   :title ,title :author ,author :collapsed ,collapsed :selection ,selection))

(def macro book/chapter ((&key (title "") numbering collapsed key predicate selection) &body elements)
  `(make-book/chapter (document/sequence (:key ,key :predicate ,predicate) ,@elements)
                      :title ,title :numbering ,numbering :collapsed ,collapsed :selection ,selection))

(def macro book/paragraph ((&key (alignment :left) collapsed selection) &body content)
  `(make-book/paragraph ,(first content) :alignment ,alignment :collapsed ,collapsed :selection ,selection))

(def macro book/list ((&key collapsed key predicate selection) &body elements)
  `(make-book/list (document/sequence (:key ,key :predicate ,predicate) ,@elements)
                   :collapsed ,collapsed :selection ,selection))

(def macro book/picture ((&key (title "") (alignment :left) collapsed selection) &body content)
  `(make-book/picture ,(first content) :title ,title :alignment ,alignment :collapsed ,collapsed :selection ,selection))
