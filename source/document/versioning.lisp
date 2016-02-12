;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document versioning/base ()
  ())

(def document versioning/version (versioning/base)
  ((name :type string)
   (author :type string)
   (creation :type local-time:timestamp)))

(def document versioning/object-version (versioning/base)
  ((version :type versioning/version)
   (content :type generic-document)))

(def document versioning/versioned-object (versioning/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction

(def function make-versioning/version (name &key author creation selection)
  (make-instance 'versioning/version :name name :author author :creation creation :selection selection))

(def function make-versioning/object-version (version content &key selection)
  (make-instance 'versioning/object-version :version version :content content :selection selection))

(def function make-versioning/versioned-object (elements &key selection)
  (make-instance 'versioning/versioned-object :elements elements :selection selection))

;;;;;;
;;; Construction

(def macro versioning/version (name &key author creation selection)
  `(make-versioning/version ,name :author ,author :creation ,creation :selection ,selection))

(def macro versioning/object-version ((version &key selection) content)
  `(make-versioning/object-version ,version ,content :selection ,selection))

(def macro versioning/versioned-object ((&key selection) &body elements)
  `(make-versioning/versioned-object (list ,@elements) :selection ,selection))
