;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :projectured.sdl
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.util
        :projectured)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-case-preserving-syntax :packages '(:hu.dwim.sdl.ffi))))

(in-package :projectured.sdl)

(import-all-owned-symbols :projectured :projectured.sdl)
