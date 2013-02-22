;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :projectured.web
  :class hu.dwim.system
  :package-name :projectured
  :description "The web backend for the generic purpose projectional editor."
  :depends-on (:hu.dwim.util.production+swank
               :hu.dwim.web-server.application
               :lispbuilder-sdl-ttf
               :projectured)
  :components ((:module "source"
                :components ((:module "web"
                              :components ((:file "device" :depends-on ("server"))
                                           (:file "editor" :depends-on ("package"))
                                           (:file "entry-point" :depends-on ("device"))
                                           (:file "graphics")
                                           (:file "package")
                                           (:file "projection" :depends-on ("device"))
                                           (:file "server" :depends-on ("package"))))))))

(defmethod perform :after ((o develop-op) (c (eql (find-system :projectured.web))))
  (eval (let ((*package* (find-package :projectured)))
          (read-from-string
           "(progn
              (setf *debug-on-error* t)
              (startup-server *projectional-editor-server*))")))
  (warn "Made sideffects on the following global variables: *database*, *compiled-query-cache*, *debug-on-error*."))
