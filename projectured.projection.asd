;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.projection
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :licence "BSD"
  :author "Levente Mészáros"
  :description "Collection of various projections."
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
                                                         (:file "document-to-t")
                                                         (:file "document-to-tree")
                                                         (:file "json-to-tree")
                                                         (:file "line-numbering")
                                                         (:file "reference-to-text")
                                                         (:file "t-to-tree")
                                                         (:file "text-aligning")
                                                         (:file "text-to-graphics")
                                                         (:file "text-to-string")
                                                         (:file "tree-to-text")
                                                         (:file "widget-to-graphics")
                                                         (:file "word-wrapping")
                                                         (:file "xml-to-tree")))
                                           (:module "compound"
                                            :depends-on ("primitive" "higher-order")
                                            :components ((:file "t-to-string" :depends-on ("t-to-t"))
                                                         (:file "t-to-t")))))))))
