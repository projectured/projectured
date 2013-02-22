;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Application

(def (class* e) projectional-editor-application (application-with-home-package)
  ())

(def (special-variable e) *projectional-editor-application* (make-instance 'projectional-editor-application))

;;;;;;
;;; Server

(def (class* e) projectional-editor-server (broker-based-server)
  ())

(def (special-variable e) *projectional-editor-server* (make-instance 'projectional-editor-server
                                                                      :host iolib.sockets:+any-host+
                                                                      :port +default-http-server-port+
                                                                      :brokers (list *projectional-editor-application*)))
