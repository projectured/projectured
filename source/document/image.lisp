;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document image/base ()
  ())

(def document image/file (image/base)
  ((filename :type pathname)
   (raw :type t)))

(def document image/memory (image/base)
  ((raw :type t)))

;;;;;;
;;; Construction

(def function make-image/file (filename &key selection)
  (make-instance 'image/file :filename filename :raw nil :selection selection))

(def function make-image/memory (raw &key selection)
  (make-instance 'image/memory :raw raw :selection selection))

;;;;;;
;;; Construction

(def macro image/file ((&key selection) &body filename)
  `(make-image/file ,(first filename) :selection ,selection))

(def macro image/memory ((&key selection) &body raw)
  `(make-image/memory ,(first raw) :selection ,selection))
