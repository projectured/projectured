;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Document

(def document book/base (document/base)
  ())

(def document book/book (book/base)
  ((title :type string)
   (authors :type sequence)
   (elements :type sequence)))

(def document book/chapter (book/base)
  ((title :type string)
   (elements :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-book/book (elements &key title authors)
  (make-instance 'book/book :title title :authors authors :elements elements))

(def (function e) make-book/chapter (elements &key title)
  (make-instance 'book/chapter :title title :elements elements))

;;;;;;
;;; Construction

(def (macro e) book/book ((&key title authors) &body elements)
  `(make-book/book (list ,@elements) :title ,title :authors ,authors))

(def (macro e) book/chapter ((&key title) &body elements)
  `(make-book/chapter (list ,@elements) :title ,title))
