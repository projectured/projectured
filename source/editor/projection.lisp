;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def definer projection (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'projection) (member 'projection supers))
                     supers
                     (append supers '(projection)))))
    `(def class* ,name ,supers ,slots ,@options (:metaclass computed-class))))

;;;;;;
;;; Class

(def projection projection ()
  ((reader :type function)
   (printer :type function))
  (:documentation "A projection describes a bidirectional transformation from one domain to another."))

;;;;;;
;;; API

(def function make-projection (name &rest args)
  (bind ((reader (find-reader name))
         (printer (find-printer name)))
    (apply #'make-instance name :reader reader :printer printer args)))
