;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def (definer :available-flags "e") document (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'document) (member 'document supers))
                     supers
                     (append supers '(document)))))
    `(progn
       (def class* ,name ,supers ,slots ,@options (:metaclass computed-class))
       ,@(when (getf -options- :export) `((export ',name))))))

;;;;;;
;;; Data structure

(def document document ()
  ((selection nil :type selection)
   (annotation nil :type document)))

;;;;;;
;;; API implementation

(def function print-document (document stream)
  (princ (output-of (apply-printer document (make-projection/t->string))) stream))
