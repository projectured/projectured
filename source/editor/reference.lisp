;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Reference API

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

;;;;;;
;;; Reference API implementation

(def function typed-reference (type reference)
  (assert (symbolp type))
  (assert (or (not (consp (first reference)))
              (not (eq 'the (first (first reference))))))
  (when reference
    `((the ,type ,(first reference)) ,@(rest reference))))
