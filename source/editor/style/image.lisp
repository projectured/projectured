;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
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

(def function make-image/file (filename &key projection selection)
  (make-instance 'image/file :filename filename :raw nil :projection projection :selection selection))

(def function make-image/memory (raw &key projection selection)
  (make-instance 'image/memory :raw raw :projection projection :selection selection))

;;;;;;
;;; Construction

(def macro image/file ((&key projection selection) &body filename)
  `(make-image/file ,(first filename) :projection ,projection :selection ,selection))

(def macro image/memory ((&key projection selection) &body raw)
  `(make-image/memory ,(first raw) :projection ,projection :selection ,selection))
