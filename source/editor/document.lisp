;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; API

(def (generic e) print-document (document stream)
  (:documentation "Prints the content of DOCUMENT to STREAM. Has side effects on STREAM."))

(def (definer :available-flags "e") document (name supers slots &rest options)
  (if *use-computed-class*
      `(def computed-class* ,name ,supers
         ,(iter (for slot :in slots)
                (collect (append slot (list :computed-in 'projectured))))
         ,@options)
      `(def class* ,name ,supers ,slots ,@options)))

;;;;;;
;;; Data structure

(def document selection/base ()
  ((selection nil :type selection)))

;; KLUDGE:
(def document document/base (selection/base)
  ())

(def document document/document (document/base)
  ((content :type t)))

(def document document/nothing (document/base)
  ((value "Empty document" :type string :allocation :class)))

(def document document/insertion (document/base)
  ((prefix "Insert a new " :type string :allocation :class)
   (value :type string)
   (suffix " here" :type string :allocation :class)))

(def document document/clipboard (document/base)
  ((content :type t)
   (slice :type t)))

;;;;;;
;;; Construction

(def (function e) make-document/document (content &key selection)
  (make-instance 'document/document :content content :selection selection))

(def (function e) make-document/nothing (&key selection)
  (make-instance 'document/nothing :selection selection))

(def (function e) make-document/insertion (&key selection (value ""))
  (make-instance 'document/insertion :selection selection :value value))

(def (function e) make-document/clipboard (content &key selection slice)
  (make-instance 'document/clipboard :content content :selection selection :slice slice))

;;;;;;
;;; Construction

(def (macro e) document/document ((&key selection) &body content)
  `(make-document/document ,(first content) :selection ,selection))

(def (macro e) document/nothing (&key selection)
  `(make-document/nothing :selection ,selection))

(def (macro e) document/insertion (&key selection (value ""))
  `(make-document/insertion :selection ,selection :value ,value))

(def (macro e) document/clipboard ((&key selection slice) &body content)
  `(make-document/clipboard ,(first content) :selection ,selection :slice ,slice))

;;;;;;
;;; API implementation

(def method print-document (document stream)
  (princ (output-of (apply-printer document (make-projection/t->string))) stream))

(def (macro e) completion-prefix-switch (prefix &body cases)
  `(switch (,prefix :test 'string=)
     ,@cases
     (t (values nil
                (bind ((matching-prefixes (remove-if-not (curry 'starts-with-subseq ,prefix) (list ,@(mapcar 'first cases))))
                       (common-prefix (reduce 'longest-common-prefix matching-prefixes :initial-value (first matching-prefixes))))
                  (subseq common-prefix (min (length common-prefix) (length ,prefix))))))))
