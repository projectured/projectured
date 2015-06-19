;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.sdl.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :projectured.test
  :licence "BSD"
  :author "Levente Mészáros"
  :description "Test suite for projectured with SDL backend."
  :depends-on (:projectured.sdl
               :projectured.test))
