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

(def document searching/search (searching/base)
  ((document :type document)
   (search :type string)
   (result :type document)))

(def document searching/result (searching/base)
  ((elements :type sequence)))

(def document searching/result-element (searching/base)
  ((document :type document)
   (path :type reference)))

;;;;;;
;;; Construction

(def function make-searching/search (document search &key result selection)
  (make-instance 'searching/search :document document :search search :result result :selection selection))

(def function make-searching/result (elements &key selection)
  (make-instance 'searching/result :elements elements :selection selection))

(def function make-searching/result-element (document path &key selection)
  (make-instance 'searching/result-element :document document :path path :selection selection))

;;;;;;
;;; Construction

(def macro searching/search ((search &key result selection) &body document)
  `(make-searching/search ,(first document) ,search :result ,result :selection ,selection))

(def macro searching/result ((&key selection) &body elements)
  `(make-searching/result (list ,@elements) :selection ,selection))

(def macro searching/result-element ((path &key selection) &body document)
  `(make-searching/result-element ,(first document) ,path :selection ,selection))
