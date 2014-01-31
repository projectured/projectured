;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "A generic purpose projectional editor."
  :depends-on (:hu.dwim.common
               :hu.dwim.computed-class+hu.dwim.defclass-star
               :hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.serializer
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :parse-number)
  :components ((:module "source"
                :components ((:file "package")
                             (:module "util"
                              :depends-on ("package")
                              :components ((:file "util")))
                             (:module "device"
                              :depends-on ("util")
                              :components ((:file "display")
                                           (:file "file")
                                           (:file "keyboard")
                                           (:file "mouse")
                                           (:file "timer")))
                             (:module "editor"
                              :depends-on ("util")
                              :components ((:file "command")
                                           (:file "device")
                                           (:file "discriminator")
                                           (:file "document")
                                           (:file "editor" :depends-on ("printer" "reader"))
                                           (:file "event")
                                           (:file "gesture" :depends-on ("event"))
                                           (:file "iomap")
                                           (:file "operation" :depends-on ("gesture" "projection" "selection"))
                                           (:file "printer" :depends-on ("device"))
                                           (:file "projection")
                                           (:file "reader" :depends-on ("device" "event" "gesture" "operation"))
                                           (:file "reference")
                                           (:file "selection")))
                             (:module "style"
                              :depends-on ("editor")
                              :components ((:file "color")
                                           (:file "font")
                                           (:file "image")))
                             (:module "domain"
                              :depends-on ("util" "style" "editor")
                              :components ((:file "book")
                                           (:file "boolean")
                                           (:file "file-system")
                                           (:file "graph")
                                           (:file "graphics")
                                           (:file "inspector")
                                           (:file "java")
                                           (:file "javascript")
                                           (:file "json")
                                           (:file "list")
                                           (:file "lisp-form")
                                           (:file "number")
                                           (:file "sequence")
                                           (:file "state-machine")
                                           (:file "string")
                                           (:file "table")
                                           (:file "text")
                                           (:file "tree")
                                           (:file "common-lisp")
                                           (:file "widget")
                                           (:file "xml")))
                             (:module "projection"
                              :depends-on ("device" "style" "domain")
                              :components ((:module "generic"
                                            :components ((:file "copying")
                                                         (:file "focusing")
                                                         (:file "preserving")
                                                         (:file "removing")
                                                         (:file "reversing")
                                                         (:file "sorting")))
                                           (:module "higher-order"
                                            :components ((:file "alternative")
                                                         (:file "iterating")
                                                         (:file "nesting")
                                                         (:file "parallel")
                                                         (:file "predicate-dispatching")
                                                         (:file "recursive")
                                                         (:file "reference-dispatching")
                                                         (:file "sequential")
                                                         (:file "type-dispatching")))
                                           (:module "primitive"
                                            :depends-on ("generic")
                                            :components ((:file "book-to-tree")
                                                         (:file "command-to-text")
                                                         (:file "common-lisp-to-lisp-form")
                                                         (:file "document-to-document")
                                                         (:file "document-to-graphics")
                                                         (:file "evaluator")
                                                         (:file "file-system-to-tree")
                                                         (:file "graph-to-graphics")
                                                         (:file "inliner")
                                                         (:file "inspector-to-table")
                                                         (:file "java-to-tree")
                                                         (:file "javascript-to-tree")
                                                         (:file "json-to-tree")
                                                         (:file "lisp-form-to-tree")
                                                         (:file "list-to-text")
                                                         (:file "object-to-object")
                                                         (:file "reference-to-text")
                                                         (:file "sequence-to-list")
                                                         (:file "sequence-to-text")
                                                         (:file "state-machine-to-graph")
                                                         (:file "state-machine-to-tree")
                                                         (:file "string-to-delimited-string")
                                                         (:file "string-to-leaf")
                                                         (:file "string-to-string")
                                                         (:file "string-to-text")
                                                         (:file "t-to-class-name")
                                                         (:file "t-to-table")
                                                         (:file "table-to-text")
                                                         (:file "text-to-graphics")
                                                         (:file "line-numbering")
                                                         (:file "text-to-string")
                                                         (:file "text-to-tree")
                                                         (:file "tree-to-graphics")
                                                         (:file "tree-to-text")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "xml-to-tree")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")))))))))
