;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection API

(def (definer e :available-flags "e") projection (name supers slots &optional options)
  `(progn
     (def class* ,name (,@(append supers '(projection))) ,slots)
     ,@(when (getf -options- :export) `((export ',name)))
     ,@options))

(def (function e) make-projection (name &rest args)
  (apply #'make-instance name
         :reader (find-reader name)
         :printer (find-printer name)
         args))

;;;;;;
;;; Projection classes

(def class* projection ()
  ((reader :type function)
   (printer :type function))
  (:documentation "A projection describes a bidirectional transformation from one domain to another."))
