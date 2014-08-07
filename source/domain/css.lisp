;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document css/base ()
  ())

(def document css/attribute (css/base)
  ((name :type string)
   (value :type string)))

(def document css/rule (css/base)
  ((selector :type string)
   (attributes :type sequence)))

;;;;;;
;;; Construction

(def function make-css/attribute (name value &key selection)
  (make-instance 'css/attribute :name name :value value :selection selection))

(def function make-css/rule (selector attributes &key selection)
  (make-instance 'css/rule :selector selector :attributes attributes :selection selection))

;;;;;;
;;; Construction

(def macro css/attribute ((&key selection) name value)
  `(make-css/attribute ,name ,value :selection ,selection))

(def macro css/rule ((selector &key selection) &body attributes)
  `(make-css/rule ,selector (list ,@attributes) :selection ,selection))
