;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document book/base ()
  ())

(def document book/book (book/base)
  ((title :type string)
   (author :type sequence)
   (elements :type sequence)))

(def document book/chapter (book/base)
  ((title :type string)
   (numbering :type string)
   (expanded :type boolean)
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

(def function make-book/book (elements &key (title "") author selection)
  (make-instance 'book/book :title title :author author :elements elements :selection selection))

(def function make-book/chapter (elements &key (title "") numbering (expanded #t) selection)
  (make-instance 'book/chapter :title title :numbering numbering :expanded expanded :elements elements :selection selection))

(def function make-book/paragraph (content &key (alignment :left) selection)
  (make-instance 'book/paragraph :content content :alignment alignment :selection selection))

(def function make-book/list (elements &key selection)
  (make-instance 'book/list :elements elements :selection selection))

(def function make-book/picture (content &key title (alignment :left) selection)
  (make-instance 'book/picture :content content :title title :alignment alignment :selection selection))

;;;;;;
;;; Construction

(def macro book/book ((&key (title "") author selection) &body elements)
  `(make-book/book (list-ll ,@elements) :title ,title :author ,author :selection ,selection))

(def macro book/chapter ((&key (title "") numbering (expanded #t) selection) &body elements)
  `(make-book/chapter (list-ll ,@elements) :title ,title :numbering ,numbering :expanded ,expanded :selection ,selection))

(def macro book/paragraph ((&key (alignment :left) selection) &body content)
  `(make-book/paragraph ,(first content) :alignment ,alignment :selection ,selection))

(def macro book/list ((&key selection) &body elements)
  `(make-book/list (list-ll ,@elements) :selection ,selection))

(def macro book/picture ((&key (title "") (alignment :left) selection) &body content)
  `(make-book/picture ,(first content) :title ,title :alignment ,alignment :selection ,selection))
