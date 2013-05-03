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

(def printer test-result->table (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-table/table (list* (make-table/row (list (make-table/cell (make-text/text (list (make-text/string "Name" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
                                                                (make-table/cell (make-text/text (list (make-text/string "Failures" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))))
                                          (iter (for (test test-run) :in-hashtable (hu.dwim.stefil::run-tests-of input))
                                                (collect (make-table/row (list (make-table/cell (make-text/text (list (make-text/string (string-downcase (symbol-name (hu.dwim.stefil::name-of test))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
                                                                               (make-table/cell (make-text/text (list (make-text/string (write-to-string (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run)) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference nil)))

;;;;;;
;;; Reader

(def reader test-result->table (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
