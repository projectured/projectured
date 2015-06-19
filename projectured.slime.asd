;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.slime
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :licence "BSD"
  :author "Levente Mészáros"
  :description "The SLIME backend for the generic purpose projectional editor."
  :depends-on (:projectured
               :swank)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:module "slime"
                                            :components ((:file "device")
                                                         (:file "editor")
                                                         (:file "graphics")
                                                         (:file "printer" :depends-on ("device"))))))))))
