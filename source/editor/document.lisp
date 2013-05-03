;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def (computed-universe e) projectured ()
  ()
  (:computed-state-factory-name as))

(def (generic e) document? (object)
  (:documentation "Returns TRUE if OBJECT is a document, otherwise returns FALSE. Purely functional."))

(def (generic e) print-document (document &optional stream)
  (:documentation "Prints the content of DOCUMENT to STREAM. Has side effects on STREAM."))

(def (generic e) make-projection/t->string (content)
  (:documentation "Returns a new projection object that projects CONTENT to string."))

(def (definer :available-flags "e") document (name supers slots &rest options)
  `(def computed-class* ,name ,supers
     ,(iter (for slot :in slots)
            (collect (append slot (list :computed-in 'projectured))))
     ,@options))

;;;;;;
;;; Data structure

(def document document ()
  ((content :type t)
   (selection :type selection)))

;;;;;;
;;; Construction

(def (function e) make-document (content &key selection)
  (make-instance 'document
                 :content content
                 :selection selection))

;;;;;;
;;; Construction

(def (macro e) document ((&key selection) &body content)
  `(make-document ,(first content) :selection ,selection))

;;;;;;
;;; API implementation

(def method document? (object)
  (typep object 'document))

(def method print-document (document &optional (stream *standard-output*))
  (princ (output-of (apply-printer document (make-projection/t->string))) stream)
  document)
