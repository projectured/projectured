;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :projectured.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :projectured)
  (:readtable-setup (setup-readtable/same-as-package :projectured))
  (:shadow :test :factorial))

(hu.dwim.common:import-all-owned-symbols :projectured :projectured.test)
