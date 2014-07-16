;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;; TODO: move to web-server?
(def function serve-http-request (thunk)
  (bind ((html-string (with-output-to-string (*standard-output*)
                        (funcall thunk hu.dwim.web-server::*request*)))
         (html-bytes (babel:string-to-octets html-string :encoding hu.dwim.web-server::+default-external-format+))
         (http-headers (list (cons hu.dwim.web-server:+header/status+ hu.dwim.web-server:+http-ok+) (cons hu.dwim.web-server:+header/content-type+ "text/html; charset=utf-8"))))
    (hu.dwim.web-server::serve-sequence html-bytes :headers http-headers)))

(def (special-variable e) *demo-server* (make-instance 'hu.dwim.web-server::server
                                                       :host iolib.sockets:+any-host+
                                                       :port 8080
                                                       :handler (lambda () (serve-http-request 'process-http-request))))
