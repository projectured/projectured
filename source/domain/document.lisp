;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/base ()
  ())

(def document document/document (document/base)
  ((filename :type pathname)
   (content :type t)))

(def document document/nothing (document/base)
  ((value "Empty document" :type string :allocation :class :computed-in nil)))

(def document document/insertion (document/base)
  ((prefix "Insert a new " :type string :allocation :class :computed-in nil)
   (value :type string)
   (suffix " here" :type string :allocation :class :computed-in nil)
   (font nil :type style/font)))

(def document document/search (document/base)
  ((content :type t)
   (search :type string)))

(def document document/clipboard (document/base)
  ((content :type t)
   (slice :type t)))

;;;;;;
;;; Construction

(def function make-document/document (content &key filename selection)
  (make-instance 'document/document :content content :filename filename :selection selection))

(def function make-document/nothing (&key selection)
  (make-instance 'document/nothing :selection selection))

(def function make-document/insertion (&key selection (value "") font)
  (make-instance 'document/insertion :selection selection :value value :font font))

(def function make-document/search (content &key search selection)
  (make-instance 'document/search :content content :search (or search "") :selection selection))

(def function make-document/clipboard (content &key selection slice)
  (make-instance 'document/clipboard :content content :selection selection :slice slice))

;;;;;;
;;; Construction

(def macro document/document ((&key filename selection) &body content)
  `(make-document/document ,(first content) :filename ,filename :selection ,selection))

(def macro document/nothing (&key selection)
  `(make-document/nothing :selection ,selection))

(def macro document/insertion (&key selection (value "") font)
  `(make-document/insertion :selection ,selection :value ,value :font ,font))

(def macro document/search ((&key selection search) &body content)
  `(make-document/search ,(first content) :search ,search :selection ,selection))

(def macro document/clipboard ((&key selection slice) &body content)
  `(make-document/clipboard ,(first content) :selection ,selection :slice ,slice))
