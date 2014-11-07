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
   (authors :type sequence)
   (elements :type sequence)))

(def document book/chapter (book/base)
  ((title :type string)
   (expanded :type boolean)
   (elements :type sequence)))

(def document book/paragraph (book/base)
  ((alignment :type (member :left :center :right :justified))
   (content :type t)))

(def document book/picture (book/base)
  ((title :type string)
   (alignment :type (member :left :center :right :justified))
   (content :type image/image)))

;;;;;;
;;; Construction

(def function make-book/book (elements &key (title "") authors selection)
  (make-instance 'book/book :title title :authors authors :elements (ll elements) :selection selection))

(def function make-book/chapter (elements &key (title "") (expanded #t) selection)
  (make-instance 'book/chapter :title title :expanded expanded :elements (ll elements) :selection selection))

(def function make-book/paragraph (content &key (alignment :left) selection)
  (make-instance 'book/paragraph :content content :alignment alignment :selection selection))

(def function make-book/picture (content &key title (alignment :left) selection)
  (make-instance 'book/picture :content content :title title :alignment alignment :selection selection))

;;;;;;
;;; Construction

(def macro book/book ((&key (title "") authors selection) &body elements)
  `(make-book/book (list ,@elements) :title ,title :authors ,authors :selection ,selection))

(def macro book/chapter ((&key (title "") (expanded #t) selection) &body elements)
  `(make-book/chapter (list ,@elements) :title ,title :expanded ,expanded :selection ,selection))

(def macro book/paragraph ((&key (alignment :left) selection) &body content)
  `(make-book/paragraph ,(first content) :alignment ,alignment :selection ,selection))

(def macro book/picture ((&key (title "") (alignment :left) selection) &body content)
  `(make-book/picture ,(first content) :title ,title :alignment ,alignment :selection ,selection))
