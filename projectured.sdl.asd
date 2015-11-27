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
               :sdl2
               :sdl2-gfx
               :sdl2-image
               :sdl2-ttf)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:module "sdl"
                                            :components ((:file "backend")
                                                         (:file "graphics")
                                                         (:file "input")
                                                         (:file "output")))))))))
