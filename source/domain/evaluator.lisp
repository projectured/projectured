;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document evaluator/evaluator (selection/base)
  ((on-demand :type boolean)
   (form :type t)
   (result :type t)
   (dynamic-environment-provider :type function)))

;;;;;;
;;; Construction

(def (function e) make-evaluator/evaluator (form &key on-demand (dynamic-environment-provider 'funcall))
  (make-instance 'evaluator/evaluator :form form :on-demand on-demand :dynamic-environment-provider dynamic-environment-provider))
