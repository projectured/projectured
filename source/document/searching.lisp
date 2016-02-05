;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document searching/base ()
  ())

(def document searching/document (searching/base)
  ((content :type t)
   (search :type string)))

(def document searching/result (searching/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction
