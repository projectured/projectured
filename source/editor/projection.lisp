;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection API

(def (definer e :available-flags "e") projection (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'projection) (member 'projection supers))
                     supers
                     (append supers '(projection)))))
    `(progn
       (def class* ,name ,supers ,slots ,@options)
       ,@(when (getf -options- :export) `((export ',name))))))

(def function make-projection (name &rest args)
  (apply #'make-instance name
         :reader (find-reader name)
         :printer (find-printer name)
         args))

;;;;;;
;;; Projection classes

(def projection projection ()
  ((reader :type function)
   (printer :type function)
   (next :type projection))
  (:documentation "A projection describes a bidirectional transformation from one domain to another."))
