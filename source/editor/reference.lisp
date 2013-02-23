;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Reference API

(def (generic e) reference? (object)
  (:documentation "Returns TRUE if OBJECT is a reference, otherwise returns FALSE. Purely functional."))

(def (function e) eval-reference (document reference)
  "Returns the value of REFERENCE within DOCUMENT. Purely functional."
  (bind ((sb-ext::*evaluator-mode* :interpret))
    (eval `(bind ((document ',document))
             (declare (ignorable document))
             ,reference))))

(def (function e) (setf eval-reference) (new-value document reference)
  "Sets the value of REFERENCE within DOCUMENT to NEW-VALUE. Has side effects on DOCUMENT."
  (bind ((sb-ext::*evaluator-mode* :interpret))
    (eval `(bind ((document ,document))
             (declare (ignorable document))
             (setf ,reference ',new-value)))))

(def type reference ()
  '(or number symbol string cons))

(def (macro e) value (reference value)
  (declare (ignore reference))
  value)

;;;;;;
;;; Reference applier

(def (namespace e) reference-applier)

(def (definer e) reference-applier (name arguments &body forms)
  `(setf (find-reference-applier ',name) (lambda ,arguments ,@forms)))

(def (function e) apply-reference (iomap reference function)
  (cond ((equal `(the ,(form-type (input-of iomap)) ,(input-reference-of iomap)) reference)
         (input-of iomap))
        ((equal `(the ,(form-type (output-of iomap)) ,(output-reference-of iomap)) reference)
         (output-of iomap))
        (t
         (funcall (reference-applier-of iomap) iomap reference function))))

;;;;;;
;;; Reference API implementation

(def method reference? (object)
  (typep object 'reference))
