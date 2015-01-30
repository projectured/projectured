;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:projectured
               :hu.dwim.computed-class+hu.dwim.logger
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "document" :depends-on ("suite"))
                             (:file "package")
                             (:file "printer" :depends-on ("suite"))
                             (:file "projection" :depends-on ("document"))
                             (:file "editor" :depends-on ("projection"))
                             (:file "selection" :depends-on ("package"))
                             (:file "suite" :depends-on ("package"))
                             (:file "text" :depends-on ("suite"))))))
