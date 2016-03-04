;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.document
  :description "Collection of various documents."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :depends-on (:cl-json
               :parse-number
               :projectured.editor
               :s-xml)
  :components ((:module "source"
                :components ((:module "document"
                              :components ((:file "book" :depends-on ("sequence"))
                                           (:file "clipboard")
                                           (:file "color")
                                           (:file "common-lisp" :depends-on ("lisp-form"))
                                           (:file "document")
                                           (:file "evaluator" :depends-on ("sequence"))
                                           (:file "font")
                                           (:file "graphics")
                                           (:file "help")
                                           (:file "image")
                                           (:file "json" :depends-on ("sequence"))
                                           (:file "lisp-form")
                                           (:file "output")
                                           (:file "primitive")
                                           (:file "searching")
                                           (:file "sequence")
                                           (:file "syntax")
                                           (:file "t")
                                           (:file "text")
                                           (:file "tree")
                                           (:file "widget")
                                           (:file "xml" :depends-on ("sequence"))
                                           (:file "versioning")
                                           (:file "workbench")))))))
