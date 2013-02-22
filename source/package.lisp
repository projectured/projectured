;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :projectured
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))
