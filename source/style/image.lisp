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

(def (function e) make-image/image (filename)
  (make-instance 'image/image
                 :filename filename
                 :raw nil))

;;;;;;
;;; Construction

(def (macro e) image/image (filename)
  `(make-image/image ,filename))
