;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Application

(def class projectional-editor-application (application-with-home-package)
  ())

(def special-variable *projectional-editor-application* (make-instance 'projectional-editor-application))

;;;;;;
;;; Server

(def class projectional-editor-server (broker-based-server)
  ())

(def special-variable *projectional-editor-server* (make-instance 'projectional-editor-server
                                                                      :host iolib.sockets:+any-host+
                                                                      :port +default-http-server-port+
                                                                      :brokers (list *projectional-editor-application*)))
