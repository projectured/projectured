;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document xml/base (document/base)
  ())

(def document xml/element (xml/base)
  ((name :type string)
   (attributes :type sequence)
   (children :type sequence)))

(def document xml/attribute (xml/base)
  ((name :type string)
   (value :type string)))

(def document xml/text (xml/base)
  ((text :type string)))

;;;;;;
;;; Construction

(def (function e) make-xml/element (name attributes children)
  (make-instance 'xml/element :name name :attributes attributes :children children))

(def (function e) make-xml/attribute (name value)
  (make-instance 'xml/attribute :name name :value value))

(def (function e) make-xml/text (text)
  (make-instance 'xml/text :text text))

;;;;;;
;;; Construction

(def (macro e) xml/element (name attributes &body children)
  `(make-xml/element ,name (list ,@attributes) (list ,@children)))

(def (macro e) xml/attribute (name value)
  `(make-xml/attribute ,name ,value))

(def (macro e) xml/text (text)
  `(make-xml/text ,text))

;;;;;;
;;; Reference

(def function xml/start-tag (element)
  (name-of element))

(def function xml/end-tag (element)
  (name-of element))
