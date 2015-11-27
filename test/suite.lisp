;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Test logger

(def logger test ())

;;;;;;
;;; Root test suite

(def suite* (test :in root-suite))
