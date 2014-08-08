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

(def function make-xml/element (name attributes children &key selection)
  (make-instance 'xml/element :name name :attributes attributes :children children :selection selection))

(def function make-xml/attribute (name value &key selection)
  (make-instance 'xml/attribute :name name :value value :selection selection))

(def function make-xml/text (value &key selection)
  (make-instance 'xml/text :value value :selection selection))

;;;;;;
;;; Construction

(def macro xml/element ((name attributes &key selection) &body children)
  `(make-xml/element ,name (list ,@attributes) (list ,@children) :selection ,selection))

(def macro xml/attribute ((&key selection) name value)
  `(make-xml/attribute ,name ,value :selection ,selection))

(def macro xml/text ((&key selection) &body value)
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

;;;;;;
;;; API

(def (function e) xml/load-document (input)
  (bind ((seed (make-array 0 :adjustable #t :fill-pointer 0)))
    (s-xml:start-parse-xml input
                           (make-instance 's-xml:xml-parser-state
                                          :seed (list seed)
                                          :new-element-hook (lambda (name attributes seed)
                                                              (declare (ignore name attributes))
                                                              (cons (make-array 0 :adjustable #t :fill-pointer 0) seed))
                                          :finish-element-hook (lambda (name attributes parent-seed seed)
                                                                 (declare (ignore parent-seed))
                                                                 (vector-push-extend (make-xml/element (string name)
                                                                                                       (map 'vector (lambda (element) (make-xml/attribute (string (car element)) (string (cdr element)))) attributes)
                                                                                                       (pop seed))
                                                                                     (car seed))
                                                                 seed)
                                          :text-hook (lambda (string seed)
                                                       (vector-push-extend (make-xml/text string) (car seed))
                                                       seed)))
    (first-elt seed)))
