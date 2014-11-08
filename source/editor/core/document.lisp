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
    (if *use-computed-class*
        `(progn
           (def computed-class* ,name ,supers
             ,(iter (for slot :in slots)
                    (collect (append slot (unless (find :computed-in slot) (list :computed-in 'projectured)))))
             ,@options)
           ,@(when (getf -options- :export) `((export ',name))))
        `(progn
           (def class* ,name ,supers ,slots ,@options)
           ,@(when (getf -options- :export) `((export ',name)))))))

;;;;;;
;;; Data structure

(def document document ()
  ((selection nil :type selection)
   (projection nil :type projection)))

;;;;;;
;;; API implementation

(def function print-document (document stream)
  (princ (output-of (apply-printer document (make-projection/t->string))) stream))
