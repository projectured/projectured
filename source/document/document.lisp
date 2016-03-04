;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/base ()
  ())

(def document document/nothing (document/base)
  ((value "Empty document" :type string :allocation :class)))

(def document document/insertion (document/base)
  ((prefix "Insert a new " :type string :allocation :class)
   (value :type string)
   (suffix " here" :type string :allocation :class)
   (font nil :type style/font)))

(def document document/reference (document/base)
  ((path :type reference)))

(def document document/reflection ()
  ((content :type t)
   (last-commands :type sequence)))

;;;;;;
;;; Construction

(def function make-document/nothing (&key selection)
  (make-instance 'document/nothing :selection selection))

(def function make-document/insertion (&key selection (value "") font)
  (make-instance 'document/insertion :selection selection :value value :font font))

(def function make-document/reference (path &key selection)
  (make-instance 'document/reference :path path :selection selection))

(def function make-document/reflection (content &key selection)
  (make-instance 'document/reflection :content content :last-commands nil :selection selection))

;;;;;;
;;; Construction

(def macro document/nothing ((&key selection))
  `(make-document/nothing :selection ,selection))

(def macro document/insertion ((&key selection font) &body value)
  `(make-document/insertion :selection ,selection :value ,(or value "") :font ,font))

(def macro document/reference ((&key selection) &body path)
  `(make-document/reference ,(first path) :selection ,selection))

(def macro document/reflection ((&key selection) &body content)
  `(make-instance 'document/reflection :content ,(first content) :last-commands nil :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/load-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/save-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/export-document ()
  ((document :type document)
   (filename :type pathname)))

;;;;;;
;;; Construction

(def function make-operation/load-document (document filename)
  (make-instance 'operation/load-document :document document :filename filename))

(def function make-operation/save-document (document filename)
  (make-instance 'operation/save-document :document document :filename filename))

(def function make-operation/export-document (document filename)
  (make-instance 'operation/export-document :document document :filename filename))

;;;;;;
;;; Evaluator

(def evaluator operation/load-document (operation)
  (setf (content-of (document-of operation)) (call-loader (filename-of operation))))

(def evaluator operation/save-document (operation)
  (call-saver (filename-of operation) (content-of (document-of operation))))

(def evaluator operation/export-document (operation)
  (with-output-to-file (output (filename-of operation) :if-does-not-exist :create :if-exists :overwrite :element-type 'character)
    (print-document (content-of (document-of operation)) output)))
