;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :projectured
  :licence "BSD"
  :author "Levente Mészáros"
  :description "Test suite."
  :depends-on (:projectured.document
               :projectured.editor
               :projectured.projection
               :projectured.swank
               :hu.dwim.logger
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "text" :depends-on ("suite"))))))
