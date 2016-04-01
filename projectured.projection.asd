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
                                            :components ((:file "book-to-syntax")
                                                         (:file "chapter-numbering")
                                                         (:file "clipboard-to-t")
                                                         (:file "common-lisp-to-s-expression")
                                                         (:file "document-to-syntax")
                                                         (:file "evaluator-to-syntax")
                                                         (:file "file-system-to-syntax")
                                                         (:file "help-to-text")
                                                         (:file "inlining")
                                                         (:file "json-to-syntax")
                                                         (:file "line-numbering")
                                                         (:file "s-expression-to-form")
                                                         (:file "s-expression-to-syntax")
                                                         (:file "primitive-to-t")
                                                         (:file "primitive-to-text")
                                                         (:file "reference-to-text")
                                                         (:file "t-to-syntax")
                                                         (:file "text-aligning")
                                                         (:file "text-to-graphics")
                                                         (:file "text-to-string")
                                                         (:file "syntax-to-text")
                                                         (:file "searching-to-syntax")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "workbench-to-widget")
                                                         (:file "xml-to-syntax")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")))))))))
