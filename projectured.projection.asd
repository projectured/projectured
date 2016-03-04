;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.projection
  :description "Collection of various projections."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :depends-on (:projectured.editor
               :projectured.document)
  :components ((:module "source"
                :components ((:module "projection"
                              :components ((:module "generic"
                                            :components ((:file "copying")
                                                         (:file "focusing")
                                                         (:file "invariably")
                                                         (:file "preserving")
                                                         (:file "removing")
                                                         (:file "reversing")
                                                         (:file "sorting")))
                                           (:module "higher-order"
                                            :components ((:file "alternative")
                                                         (:file "identity-dispatching")
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
                                                         (:file "chapter-numbering")
                                                         (:file "clipboard-to-t")
                                                         (:file "common-lisp-to-lisp-form")
                                                         (:file "document-to-tree")
                                                         (:file "evaluator-to-tree")
                                                         (:file "help-to-text")
                                                         (:file "json-to-tree")
                                                         (:file "line-numbering")
                                                         (:file "lisp-form-to-form")
                                                         (:file "lisp-form-to-tree")
                                                         (:file "primitive-to-t")
                                                         (:file "primitive-to-text")
                                                         (:file "reference-to-text")
                                                         (:file "syntax-to-text")
                                                         (:file "t-to-tree")
                                                         (:file "text-aligning")
                                                         (:file "text-to-graphics")
                                                         (:file "text-to-string")
                                                         (:file "tree-to-text")
                                                         (:file "searching-to-tree")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "workbench-to-t")
                                                         (:file "workbench-to-widget")
                                                         (:file "xml-to-tree")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")))))))))
