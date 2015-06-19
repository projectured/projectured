;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :licence "BSD"
  :author "Levente Mészáros"
  :description "Test suite for projectured without backend."
  :depends-on (:projectured+swank
               :hu.dwim.logger
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "demo" :depends-on ("document" "projection"))
                             (:file "document" :depends-on ("suite"))
                             (:file "editor" :depends-on ("projection"))
                             (:file "graphics" :depends-on ("suite"))
                             (:file "navigation" :depends-on ("package"))
                             (:file "package")
                             (:file "printer" :depends-on ("suite"))
                             (:file "projection" :depends-on ("document"))
                             (:file "suite" :depends-on ("package"))
                             (:file "text" :depends-on ("suite"))))))
