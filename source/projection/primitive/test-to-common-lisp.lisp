;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection test/check->common-lisp/application ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/test/check->common-lisp/application ()
  (make-projection 'test/check->common-lisp/application))

;;;;;;
;;; Construction

(def (macro e) test/check->common-lisp/application ()
  '(make-projection/test/check->common-lisp/application))

;;;;;;
;;; Printer

(def printer test/check->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol "IS" "HU.DWIM.STEFIL")
                                               (list (output-of (recurse-printer recursion (content-of input) `((content-of (the test/check document))
                                                                                                                ,@(typed-reference (form-type input) input-reference))))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader test/check->common-lisp/application (projection recursion input printer-iomap)
  input)
