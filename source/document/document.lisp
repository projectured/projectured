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

(def document document/document (document/base)
  ((filename :type pathname)
   (content :type t)))

(def document document/nothing (document/base)
  ((value "Empty document" :type string :allocation :class)))

(def document document/insertion (document/base)
  ((prefix "Insert a new " :type string :allocation :class)
   (value :type string)
   (suffix " here" :type string :allocation :class)
   (font nil :type style/font)))

(def document document/search (document/base)
  ((content :type t)
   (search :type string)))

(def document document/search-result (document/base)
  ((elements :type sequence)))

(def document document/clipboard (document/base)
  ((content :type t)
   (slice :type t)))

(def document document/history (document/base)
  ((operations :type sequence)))

(def document document/reference (document/base)
  ((path :type reference)))

(def document document/reflection ()
  ((content :type t)
   (last-commands :type sequence)))

;;;;;;
;;; Construction

(def function make-document/document (content &key filename selection)
  (make-instance 'document/document :content content :filename filename :selection selection))

(def function make-document/nothing (&key selection)
  (make-instance 'document/nothing :selection selection))

(def function make-document/insertion (&key selection (value "") font)
  (make-instance 'document/insertion :selection selection :value value :font font))

(def function make-document/search (content &key search selection)
  (make-instance 'document/search :content content :search (or search "") :selection selection))

(def function make-document/search-result (elements &key selection)
  (make-instance 'document/search-result :elements elements :selection selection))

(def function make-document/clipboard (content &key selection slice)
  (make-instance 'document/clipboard :content content :selection selection :slice slice))

(def function make-document/history (operations &key selection)
  (make-instance 'document/history :operations operations :selection selection))

(def function make-document/reference (path &key selection)
  (make-instance 'document/reference :path path :selection selection))

(def function make-document/reflection (content &key selection)
  (make-instance 'document/reflection :content content :last-commands nil :selection selection))

;;;;;;
;;; Construction

(def macro document/document ((&key filename selection) &body content)
  `(make-document/document ,(first content) :filename ,filename :selection ,selection))

(def macro document/nothing ((&key selection))
  `(make-document/nothing :selection ,selection))

(def macro document/insertion ((&key selection font) &body value)
  `(make-document/insertion :selection ,selection :value ,(or value "") :font ,font))

(def macro document/search ((&key selection search) &body content)
  `(make-document/search ,(first content) :search ,search :selection ,selection))

(def macro document/search-result ((&key selection) &body elements)
  `(make-document/search-result (list ,@elements) :selection ,selection))

(def macro document/clipboard ((&key selection slice) &body content)
  `(make-document/clipboard ,(first content) :selection ,selection :slice ,slice))

(def macro document/history ((&key selection) &body operations)
  `(make-document/history (list ,@operations) :selection ,selection))

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
