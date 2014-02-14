;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t->class-name ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/t->class-name ()
  (make-projection 't->class-name))

;;;;;;
;;; Construction

(def (macro e) t->class-name ()
  '(make-projection/t->class-name))

;;;;;;
;;; Printer

(def printer t->class-name (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (string-downcase (class-name (class-of input)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader t->class-name (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)
