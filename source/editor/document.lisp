;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def definer document (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'document) (member 'document supers))
                     supers
                     (append supers '(document)))))
    `(def class* ,name ,supers ,slots ,@options (:metaclass computed-class))))

;;;;;;
;;; Class

(def document document ()
  ((selection nil :type selection)
   (annotation nil :type document)))

;;;;;;
;;; Type

(def type primitive-document ()
  '(or number symbol string sequence))

(def type generic-document ()
  '(or primitive-document document))

;;;;;;
;;; API

(def function print-document (document stream)
  (princ (output-of (apply-printer document (make-projection/t->string))) stream))

(def function document-type (instance)
  (typecase instance
    (null 'sequence)
    (cons 'sequence)
    (number 'number)
    (string 'string)
    (vector 'vector)
    (computed-ll 'sequence)
    (document/sequence 'sequence)
    (document/string 'document/string)
    (sequence 'sequence)
    (t (type-of instance))))
