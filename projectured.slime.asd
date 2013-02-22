;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :projectured.slime
  :class hu.dwim.system
  :package-name :projectured
  :description "The SLIME backend for the generic purpose projectional editor."
  :depends-on (:projectured
               :swank)
  :components ((:module "source"
                :components ((:module "slime"
                              :components ((:file "device")
                                           (:file "editor")
                                           (:file "projection" :depends-on ("device"))))))))
