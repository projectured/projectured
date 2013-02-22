;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) test->test-result ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/test->test-result ()
  (make-projection 'test->test-result))

;;;;;;
;;; Construction

(def (macro e) test->test-result ()
  '(make-projection/test->test-result))

;;;;;;
;;; Printer

(def printer test->test-result (projection recursion input input-reference output-reference)
  (bind ((name (hu.dwim.stefil::name-of input))
         (test-result (multiple-value-list
                       (handler-bind ((condition (lambda (c) (continue c))))
                         (hu.dwim.stefil::with-new-global-context* ()
                           (hu.dwim.stefil::run-test-body input (fdefinition name) nil t nil)))))
         (output (last-elt test-result)))
    (make-iomap/recursive projection recursion input input-reference output output-reference nil)))

;;;;;;
;;; Reader

(def reader test->test-result (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
