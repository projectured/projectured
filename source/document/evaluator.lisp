;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document evaluator/base ()
  ())

(def document evaluator/toplevel (evaluator/base)
  ((elements :type sequence)))

(def document evaluator/form (evaluator/base)
  ((form :type document)
   (result :type document)))

;;;;;;
;;; Construction

(def function make-evaluator/toplevel (elements &key selection)
  (make-instance 'evaluator/toplevel :elements elements :selection selection))

(def function make-evaluator/form (form &key result selection)
  (make-instance 'evaluator/form :form form :result result :selection selection))

;;;;;;
;;; Construction

(def macro evaluator/toplevel ((&key selection) &body elements)
  `(make-evaluator/toplevel (document/sequence (:selection (rest ,selection)) ,@elements) :selection ,selection))

(def macro evaluator/form ((&key selection) &body body)
  `(make-evaluator/form ,(first body) :result ,(second body) :selection ,selection))

;;;;;;
;;; API

(def maker eval ()
  (evaluator/toplevel (:selection '((the sequence (elements-of (the evaluator/toplevel document)))
                                    (the evaluator/form (elt (the sequence document) 0))
                                    (the common-lisp/insertion (form-of (the evaluator/form document)))
                                    (the string (value-of (the common-lisp/insertion document)))
                                    (the string (subseq (the string document) 0 0))))
    (evaluator/form (:selection '((the common-lisp/insertion (form-of (the evaluator/form document)))
                                  (the string (value-of (the common-lisp/insertion document)))
                                  (the string (subseq (the string document) 0 0))))
      (make-common-lisp/insertion "" 'common-lisp/complete-document
                                  :default-value "enter form"
                                  :selection '((the string (value-of (the common-lisp/insertion document)))
                                               (the string (subseq (the string document) 0 0)))))))

(def loader eval (filename)
  (with-input-from-file (input filename :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:deserialize input)))

(def saver eval (filename document)
  (with-output-to-file (output filename :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:serialize document :output output)))
