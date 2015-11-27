;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.executable
  :description "Generic purpose projectional editor."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :entry-point "projectured::executable-toplevel"
  :build-pathname "projectured" ;; the base name of the executable (unless it's overridden, e.g. in bin/build.sh)
  :package-name :projectured
  :depends-on (:command-line-arguments
               :projectured.sdl)
  :components ((:module "source"
                :components ((:module "executable"
                              :components ((:file "document")
                                           (:file "projection")
                                           (:file "toplevel" :depends-on ("document" "projection"))))))))
