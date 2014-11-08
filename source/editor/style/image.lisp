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

;; TODO: rename image/file
(def document image/image (image/base)
  ((filename :type pathname)
   (raw :type t)))

(def document image/memory (image/base)
  ((raw :type t)))

;;;;;;
;;; Construction

(def function make-image/image (filename &key projection selection)
  (make-instance 'image/image :filename filename :raw nil :projection projection :selection selection))

(def function make-image/memory (raw &key projection selection)
  (make-instance 'image/memory :raw raw :projection projection :selection selection))

;;;;;;
;;; Construction

(def macro image/image ((&key projection selection) &body filename)
  `(make-image/image ,(first filename) :projection ,projection :selection ,selection))

(def macro image/memory ((&key projection selection) &body raw)
  `(make-image/memory ,(first raw) :projection ,projection :selection ,selection))
