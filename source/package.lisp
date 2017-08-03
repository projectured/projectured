;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :projectured
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.util)
  (:shadow #:as)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)))
