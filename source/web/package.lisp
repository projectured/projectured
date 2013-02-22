;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(use-package '(:hu.dwim.web-server :hu.dwim.quasi-quote.xml))

(setf (hu.dwim.def::readtable-setup-form-of (find-extended-package (string :projectured))) '(hu.dwim.web-server::setup-readtable))

(hu.dwim.def::call-extended-package-definition-hooks (find-extended-package (string :projectured)))
