;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection inliner ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/inliner ()
  (make-projection 'inliner))

;;;;;;
;;; Construction

(def (macro e) inliner ()
  '(make-projection/inliner))

;;;;;;
;;; Printer

(def printer inliner (projection recursion input input-reference)
  (bind ((operator (operator-of input))
         (output (if (typep operator 'common-lisp/function-definition)
                     (make-common-lisp/let (iter (for argument :in (arguments-of input))
                                                 (for binding :in (bindings-of operator))
                                                 (collect (make-common-lisp/lexical-variable-binding (name-of binding) argument)))
                                           (body-of operator))
                     (make-common-lisp/application operator
                                                   (iter (for index :from 0)
                                                         (for argument :in (arguments-of input))
                                                         (for argument-iomap = (recurse-printer recursion argument `((elt (the sequence document) ,index)
                                                                                                                     (the sequence (arguments-of (the ,(form-type input) document)))
                                                                                                                     ,@(typed-reference (form-type input) input-reference))))
                                                         (collect (output-of argument-iomap)))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader inliner (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))
