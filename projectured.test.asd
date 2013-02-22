;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :projectured.test
  :class hu.dwim.test-system
  :depends-on (:projectured
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "source"
                :components ((:module "projection"
                             :components ((:module "primitive"
                                           :components ((:file "test-to-test-result")
                                                        (:file "test-result-to-table")))))))
               (:module "test"
                :depends-on ("source")
                :components ((:file "content" :depends-on ("suite"))
                             (:file "document" :depends-on ("content"))
                             (:file "package")
                             (:file "projection" :depends-on ("document"))
                             (:file "editor" :depends-on ("projection"))
                             (:file "suite" :depends-on ("package"))))))
