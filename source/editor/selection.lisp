;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Selection API
;;;
;;; A selection represents an argument for operations.

(def (generic e) selection? (object)
  (:documentation "Returns TRUE if OBJECT is a selection, otherwise returns FALSE. Purely functional."))

;;;;;;
;;; Selection classes

;; TODO: appearance
(def class* selection ()
  ()
  (:documentation "Base class for selections."))

(def class* selection/single (selection)
  ((target :type t))
  (:documentation "A selection that refers to a single target."))

(def class* selection/range (selection)
  ((begin :type selection/single)
   (end :type selection/single))
  (:documentation "A selection that covers a range of targets from BEGIN to END, both ends are inclusive."))

(def class* selection/multiple (selection)
  ((elements :type sequence))
  (:documentation "A selection that handles a sequence of selections as a whole."))

;;;;;
;;; Selection constructors

(def (function e) make-selection/single (target)
  (make-instance 'selection/single :target target))

(def (function e) make-selection/range (begin end)
  (make-instance 'selection/range :begin begin :end end))

(def (function e) make-selection/multiple (elements)
  (make-instance 'selection/multiple :elements elements))

;;;;;;
;;; Selection API implementation

(def method selection? (object)
  (typep object 'selection))
