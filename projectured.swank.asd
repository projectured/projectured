;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.swank
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :licence "BSD"
  :author "Levente Mészáros"
  :description "SLIME support."
  :depends-on (:projectured.editor
               :swank)
  :components ((:module "source"
                :components ((:module "util"
                              :components ((:file "swank")))))))
