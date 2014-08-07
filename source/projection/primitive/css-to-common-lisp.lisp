;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection css/attribute->common-lisp/application ()
  ())

(def projection css/rule->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def function make-projection/css/attribute->common-lisp/application ()
  (make-projection 'css/attribute->common-lisp/application))

(def function make-projection/css/rule->common-lisp/progn ()
  (make-projection 'css/rule->common-lisp/progn))

;;;;;;
;;; Printer

(def printer css/attribute->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ (name-of input) ": " (value-of input) ";"))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer css/rule->common-lisp/progn (projection recursion input input-reference)
  (bind ((attribute-iomaps (iter (for attribute :in-sequence (attributes-of input))
                                 (for attribute-index :from 0)
                                 (collect (recurse-printer recursion attribute
                                                           `((elt (the sequence document) ,attribute-index)
                                                             (the sequence (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ (selector-of input) " {")))))
                                                 (iter (for attribute-iomap :in attribute-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant " ")))))
                                                       (collect (output-of attribute-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "}"))))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader css/attribute->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader css/rule->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
