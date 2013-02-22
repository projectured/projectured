;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) test-result->table ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/test-result->table ()
  (make-projection 'test-result->table))

;;;;;;
;;; Construction

(def (macro e) test-result->table ()
  '(make-projection/test-result->table))

;;;;;;
;;; Printer

(def printer test-result->table (projection recursion input input-reference output-reference)
  (bind ((output (make-table/table (list* (make-table/row (list (make-table/cell "Name")
                                                                (make-table/cell "Failures")))
                                          (iter (for (test test-run) :in-hashtable (hu.dwim.stefil::run-tests-of input))
                                                (collect (make-table/row (list (make-table/cell (string-downcase (symbol-name (hu.dwim.stefil::name-of test))))
                                                                               (make-table/cell (write-to-string (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run)))))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference nil)))

;;;;;;
;;; Reader

(def reader test-result->table (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
