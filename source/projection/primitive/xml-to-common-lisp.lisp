;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection xml/text->common-lisp/application ()
  ())

(def projection xml/attribute->common-lisp/progn ()
  ())

(def projection xml/element->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/xml/text->common-lisp/application ()
  (make-projection 'xml/text->common-lisp/application))

(def (function e) make-projection/xml/attribute->common-lisp/progn ()
  (make-projection 'xml/attribute->common-lisp/progn))

(def (function e) make-projection/xml/element->common-lisp/progn ()
  (make-projection 'xml/element->common-lisp/progn))

;;;;;;
;;; Construction

(def (macro e) xml/text->common-lisp/application ()
  '(make-projection/xml/text->common-lisp/application))

(def (macro e) xml/attribute->common-lisp/progn ()
  '(make-projection/xml/attribute->common-lisp/progn))

(def (macro e) xml/element->common-lisp/progn ()
  '(make-projection/xml/element->common-lisp/progn))

;;;;;;
;;; Printer

(def printer xml/text->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                               (list (make-common-lisp/constant (value-of input))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer xml/attribute->common-lisp/progn (projection recursion input input-reference)
  (bind ((output (make-common-lisp/progn (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ (name-of input) "=\"" (value-of input) "\""))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer xml/element->common-lisp/progn (projection recursion input input-reference)
  (bind ((attribute-iomaps (iter (for attribute :in-sequence (attributes-of input))
                                 (for attribute-index :from 0)
                                 (collect (recurse-printer recursion attribute
                                                           `((elt (the sequence document) ,attribute-index)
                                                             (the sequence (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (child-iomaps (iter (for child :in-sequence (children-of input))
                             (for child-index :from 0)
                             (for child-iomap = (recurse-printer recursion child
                                                                 `((elt (the sequence document) ,child-index)
                                                                   (the sequence (children-of document))
                                                                   ,@(typed-reference (form-type input) input-reference))))
                             (collect child-iomap)))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ "<" (name-of input))))))
                                                 (iter (for attribute-iomap :in attribute-iomaps)
                                                       (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant " "))))
                                                       (collect (output-of attribute-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant ">"))))
                                                 (iter (for child-iomap :in child-iomaps)
                                                       (collect (output-of child-iomap))
                                                       #+nil(collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string #\NewLine))))))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ "</" (name-of input) ">")))))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader xml/text->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader xml/attribute->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader xml/element->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
