;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) removing ()
  ((element :type t)
   (predicate :type function)
   (key :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/removing (element predicate key)
  (make-projection 'removing
                   :element element
                   :predicate predicate
                   :key key))

;;;;;;
;;; Construction

(def (macro e) removing (element predicate key)
  `(make-projection/removing ,element ,predicate ,key))

;;;;;;
;;; Printer

(def printer removing (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (element (element-of projection))
         (key (key-of projection))
         (predicate (predicate-of projection))
         (child-iomaps nil)
         (output (coerce (iter (with output-index = 0)
                               (for input-index :from 0)
                               (for input-element :in-sequence input)
                               (unless (funcall predicate element (funcall key input-element))
                                 (push (if (stringp input)
                                           ;; TODO:
                                           (make-iomap/string input input-reference input-index "TODO XXX" output-reference output-index 1)
                                           (make-iomap/object projection recursion
                                                              input-element `(elt ,typed-input-reference ,input-index)
                                                              input-element `(elt (the list ,output-reference) ,output-index)))
                                       child-iomaps)
                                 (collect input-element)
                                 (incf output-index)))
                         (cond ((stringp input)
                                'string)
                               ((listp input)
                                'list)
                               ((vectorp input)
                                'vector)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader removing (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
