;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document xml/base ()
  ())

(def document xml/element (xml/base)
  ((name :type string)
   (attributes :type sequence)
   (children :type sequence)))

(def document xml/attribute (xml/base)
  ((name :type string)
   (value :type string)))

(def document xml/text (xml/base)
  ((value :type string)))

;;;;;;
;;; Construction

(def (function e) make-xml/element (name attributes children &key selection)
  (make-instance 'xml/element :name name :attributes attributes :children children :selection selection))

(def (function e) make-xml/attribute (name value &key selection)
  (make-instance 'xml/attribute :name name :value value :selection selection))

(def (function e) make-xml/text (value &key selection)
  (make-instance 'xml/text :value value :selection selection))

;;;;;;
;;; Construction

(def (macro e) xml/element ((name attributes &key selection) &body children)
  `(make-xml/element ,name (list ,@attributes) (list ,@children) :selection ,selection))

(def (macro e) xml/attribute ((&key selection) name value)
  `(make-xml/attribute ,name ,value :selection ,selection))

(def (macro e) xml/text ((&key selection) &body value)
  `(make-xml/text ,(first value) :selection ,selection))

;;;;;;
;;; Reference

(def function xml/start-tag (element)
  (name-of element))

(def function xml/end-tag (element)
  (name-of element))

(def function (setf xml/start-tag) (new-value element)
  (setf (name-of element) new-value))

(def function (setf xml/end-tag) (new-value element)
  (setf (name-of element) new-value))
