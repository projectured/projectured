;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.sdl
  :description "SDL (Simple DirectMedial Layer) backend."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :projectured
  :depends-on (:projectured.document
               :projectured.editor
               :projectured.projection
               :hu.dwim.sdl
               :hu.dwim.sdl/gfx
               :hu.dwim.sdl/image
               :hu.dwim.sdl/ttf)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:module "sdl"
                                            :serial t
                                            :components ((:file "package")
                                                         (:file "backend")
                                                         (:file "graphics")
                                                         (:file "input")
                                                         (:file "output")))))))))
