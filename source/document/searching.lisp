;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document searching/base ()
  ())

(def document searching/parameters (searching/base)
  ((text :type string)
   (case-sensitive :type boolean)))

(def document searching/search (searching/base)
  ((parameters :type string)
   (document :type document)
   (result :type document)))

(def document searching/result (searching/base)
  ((elements :type sequence)))

(def document searching/result-element (searching/base)
  ((document :type document)
   (path :type reference)
   (context-depth :type number)))

;;;;;;
;;; Construction

(def function make-searching/search (parameters document &key result selection)
  (make-instance 'searching/search :parameters parameters :document document :result result :selection selection))

(def function make-searching/result (elements &key selection)
  (make-instance 'searching/result :elements elements :selection selection))

(def function make-searching/result-element (document path &key context-depth selection)
  (make-instance 'searching/result-element :document document :path path :context-depth (or context-depth 0) :selection selection))

;;;;;;
;;; Construction

(def macro searching/search ((&key result selection) &body body)
  `(make-searching/search ,(first body) ,(second body) :result ,result :selection ,selection))

(def macro searching/result ((&key selection) &body elements)
  `(make-searching/result (list ,@elements) :selection ,selection))

(def macro searching/result-element ((path &key context-depth selection) &body document)
  `(make-searching/result-element ,(first document) ,path :context-depth ,context-depth :selection ,selection))
