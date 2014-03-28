;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document image/image ()
  ((filename :type pathname)
   (raw :type t)))

;;;;;;
;;; Construction

(def (function e) make-image/image (filename &key projection selection)
  (make-instance 'image/image
                 :filename filename :raw nil
                 :projection projection :selection selection))

;;;;;;
;;; Construction

(def (macro e) image/image ((&key projection selection) &body filename)
  `(make-image/image ,(first filename) :projection ,projection :selection ,selection))
