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
                              :components ((:file "book")
                                           (:file "color")
                                           (:file "document")
                                           (:file "font")
                                           (:file "graphics")
                                           (:file "image")
                                           (:file "json")
                                           (:file "number")
                                           (:file "sequence")
                                           (:file "string")
                                           (:file "t")
                                           (:file "text")
                                           (:file "tree")
                                           (:file "widget")
                                           (:file "xml")))))))
