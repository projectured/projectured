;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :projectured.slime.test
  :class hu.dwim.test-system
  :package-name :projectured.test
  :description "Test suite for projectured with SLIME backend."
  :depends-on (:projectured.slime
               :projectured.test))
