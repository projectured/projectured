;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.test.demo
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.web-server.application
               :projectured.test)
  :components ((:module "test"
                :components ((:file "demo")))))

(defmethod perform :after ((o hu.dwim.asdf:develop-op) (c (eql (find-system :projectured.test.demo))))
  (eval (let ((*package* (find-package :projectured.test)))
          (read-from-string
           "(progn
              (setf *debug-on-error* t)
              (startup-server *demo-server*))")))
  (warn "Made sideffects on the following global variables: *debug-on-error*."))
