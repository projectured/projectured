;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :projectured.documentation
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :projectured
        :hu.dwim.presentation
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.presentation)))
