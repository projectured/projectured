;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Toplevel

(def function executable-toplevel ()
  (run-read-evaluate-print-loop (make-editor) (make-initial-document) (make-initial-projection) :measure-printer #t))
