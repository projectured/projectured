;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.editor
  :description "Editor core components."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :depends-on (:hu.dwim.common
               :hu.dwim.defclass-star
               :hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.serializer
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "package")
                             (:module "util"
                              :depends-on ("package")
                              :components ((:file "computed" :depends-on ("util"))
                                           (:file "logger")
                                           (:file "util")))
                             (:module "editor"
                              :depends-on ("util")
                              :components ((:file "backend")
                                           (:file "command")
                                           (:file "device")
                                           (:file "document")
                                           (:file "editor" :depends-on ("printer" "reader"))
                                           (:file "event")
                                           (:file "gesture" :depends-on ("event"))
                                           (:file "iomap")
                                           (:file "loader")
                                           (:file "maker")
                                           (:file "operation")
                                           (:file "printer")
                                           (:file "projection")
                                           (:file "reader")
                                           (:file "reference")
                                           (:file "saver")))))))
