;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :projectured
  :class hu.dwim.system
  :description "A generic purpose projectional editor."
  :depends-on (:hu.dwim.common
               :hu.dwim.computed-class+hu.dwim.defclass-star
               :hu.dwim.def
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
                              :components ((:file "device")
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
                                           (:file "graph")
                                           (:file "graphics")
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
                                                         (:file "document-to-document")
                                                         (:file "document-to-graphics")
                                                         (:file "evaluator")
                                                         (:file "graph-to-graphics")
                                                         (:file "inliner")
                                                         (:file "java-to-tree")
                                                         (:file "javascript-to-tree")
                                                         (:file "json-to-tree")
                                                         (:file "lisp-form-to-tree")
                                                         (:file "list-to-string")
                                                         (:file "object-to-object")
                                                         (:file "reference-to-string")
                                                         (:file "sequence-to-list")
                                                         (:file "sequence-to-string")
                                                         (:file "state-machine-to-tree")
                                                         (:file "state-machine-to-graph")
                                                         (:file "string-to-delimited-string")
                                                         (:file "string-to-leaf")
                                                         (:file "string-to-line-numbered-string")
                                                         (:file "string-to-string")
                                                         (:file "string-to-styled-string")
                                                         (:file "styled-string-to-graphics")
                                                         (:file "styled-string-to-line-numbered-styled-string")
                                                         (:file "styled-string-to-string")
                                                         (:file "styled-string-to-tree")
                                                         (:file "t-to-class-name")
                                                         (:file "t-to-table")
                                                         (:file "table-to-string")
                                                         (:file "text-to-string")
                                                         (:file "text-to-tree")
                                                         (:file "tree-to-graphics")
                                                         (:file "tree-to-string")
                                                         (:file "tree-to-styled-string")
                                                         (:file "common-lisp-to-lisp-form")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "xml-to-tree")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")))))))))
