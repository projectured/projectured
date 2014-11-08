;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "A generic purpose projectional editor."
  :depends-on (:cl-json
               :hu.dwim.common
               :hu.dwim.computed-class+hu.dwim.defclass-star
               :hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.serializer
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :parse-number
               :s-xml)
  :components ((:module "source"
                :components ((:file "package")
                             (:module "util"
                              :depends-on ("package")
                              :components ((:file "util")
                                           (:file "ll" :depends-on ("util"))))
                             (:module "editor"
                              :depends-on ("util")
                              :components ((:module "device"
                                            :components ((:file "display")
                                                         (:file "file")
                                                         (:file "keyboard")
                                                         (:file "mouse")
                                                         (:file "timer")))
                                           (:module "core"
                                            :components ((:file "command")
                                                         (:file "device")
                                                         (:file "document")
                                                         (:file "editor" :depends-on ("printer" "reader"))
                                                         (:file "event")
                                                         (:file "gesture" :depends-on ("event"))
                                                         (:file "iomap")
                                                         (:file "operation" :depends-on ("gesture" "projection"))
                                                         (:file "printer" :depends-on ("device"))
                                                         (:file "projection")
                                                         (:file "reader" :depends-on ("device" "event" "gesture" "operation"))
                                                         (:file "reference")))
                                           (:module "style"
                                            :depends-on ("core")
                                            :components ((:file "color")
                                                         (:file "font")
                                                         (:file "image")))))
                             (:module "domain"
                              :depends-on ("editor")
                              :components ((:file "book")
                                           (:file "boolean")
                                           (:file "css")
                                           (:file "document")
                                           (:file "evaluator")
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
                                           (:file "test")
                                           (:file "text")
                                           (:file "tree")
                                           (:file "common-lisp")
                                           (:file "widget")
                                           (:file "xml")))
                             (:module "projection"
                              :depends-on ("editor")
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
                                                         (:file "css-to-common-lisp")
                                                         (:file "css-to-tree")
                                                         (:file "command-to-text")
                                                         (:file "common-lisp-to-lisp-form")
                                                         (:file "document-to-text")
                                                         (:file "evaluator")
                                                         (:file "evaluator-to-tree")
                                                         (:file "file-system-to-tree")
                                                         (:file "graph-to-graphics")
                                                         (:file "graphics-to-graphics")
                                                         (:file "image-to-tree")
                                                         (:file "inliner")
                                                         (:file "inspector-to-table")
                                                         (:file "java-to-tree")
                                                         (:file "javascript-to-common-lisp")
                                                         (:file "javascript-to-tree")
                                                         (:file "json-to-common-lisp")
                                                         (:file "json-to-tree")
                                                         (:file "lisp-form-to-form")
                                                         (:file "lisp-form-to-tree")
                                                         (:file "list-to-text")
                                                         (:file "reference-to-text")
                                                         (:file "sequence-to-list")
                                                         (:file "sequence-to-text")
                                                         (:file "state-machine-to-graph")
                                                         (:file "state-machine-to-tree")
                                                         (:file "string-to-delimited-string")
                                                         (:file "string-to-text")
                                                         (:file "t-to-class-name")
                                                         (:file "t-to-table")
                                                         (:file "t-to-text")
                                                         (:file "t-to-tree")
                                                         (:file "table-to-text")
                                                         (:file "text-to-graphics")
                                                         (:file "text-to-text")
                                                         (:file "line-numbering")
                                                         (:file "test-to-common-lisp")
                                                         (:file "test-to-tree")
                                                         (:file "text-to-string")
                                                         (:file "text-to-tree")
                                                         (:file "tree-to-graphics")
                                                         (:file "tree-to-text")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "xml-to-common-lisp")
                                                         (:file "xml-to-tree")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-common-lisp" :depends-on ("t-to-t"))
                                                         (:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")
                                                         (:file "t-to-tree" :depends-on ("t-to-t"))))))
                             (:module "executable"
                              :depends-on ("projection")
                              :components ((:file "document")
                                           (:file "projection")
                                           (:file "top-level" :depends-on ("document" "projection"))))))))
