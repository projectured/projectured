;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text/text->tree/node ()
  ())

(def projection text/string->tree/leaf ()
  ())

(def projection image/image->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text/text->tree/node ()
  (make-projection 'text/text->tree/node))

(def (function e) make-projection/text/string->tree/leaf ()
  (make-projection 'text/string->tree/leaf))

(def (function e) make-projection/image/image->tree/leaf ()
  (make-projection 'image/image->tree/leaf))

;;;;;;
;;; Printer

(def printer text/text->tree/node (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion element
                                                                             `(elt (the sequence (elements-of ,typed-input-reference)) ,index)))
                                       (collect (output-of element-iomap))))))
    (make-iomap/compound projection recursion input input-reference output
                          (list (make-iomap/object projection recursion input input-reference output)))))

(def printer text/string->tree/leaf (projection recursion input input-reference)
  (bind ((output (make-tree/leaf input)))
    (make-iomap/compound projection recursion input input-reference output
                          (list (make-iomap/object projection recursion input input-reference output)))))

(def printer image/image->tree/leaf (projection recursion input input-reference)
  (bind ((output (make-tree/leaf input)))
    (make-iomap/compound projection recursion input input-reference output
                          (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader text/text->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader text/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader image/image->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
