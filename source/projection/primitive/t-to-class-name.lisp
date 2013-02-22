;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) t->class-name ()
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

(def printer t->class-name (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (string-downcase (class-name (class-of input)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (string-downcase (the symbol (class-name (the class (class-of ,typed-input-reference)))))) 0
                                                    output `(the string ,output-reference) 0 (length output))))))

;;;;;;
;;; Reader

(def reader t->class-name (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
