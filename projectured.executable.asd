;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.executable
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :licence "BSD"
  :author "Levente Mészáros"
  :description "Generic purpose projectional editor."
  :depends-on (:command-line-arguments
               :projectured.sdl)
  :components ((:module "source"
                :components ((:module "executable"
                              :components ((:file "document")
                                           (:file "projection")
                                           (:file "toplevel" :depends-on ("document" "projection"))))))))
