;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document evaluator/evaluator ()
  ((form :type t)
   (result :type t)
   (on-demand :type boolean)
   (dynamic-environment-provider :type function)))

;;;;;;
;;; Construction

(def (function e) make-evaluator/evaluator (form &key on-demand (dynamic-environment-provider (compose 'make-common-lisp/constant 'funcall)))
  (make-instance 'evaluator/evaluator
                 :form form :result nil
                 :on-demand on-demand :dynamic-environment-provider dynamic-environment-provider))
