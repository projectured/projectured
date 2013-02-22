;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.documentation)

;;;;;;
;;; Projectional editor project

(def project :projectured :description "A generic purpose projectional editor with documentation and examples.")

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :projectured))))
  (append (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide)))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Install guide"))
                  (make-value-inspector (find-book 'install-guide)))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Developer guide"))
                  (make-value-inspector (find-book 'developer-guide))))
          (call-next-method)
          (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Package"))
                  (make-value-inspector (find-package :projectured))))))
