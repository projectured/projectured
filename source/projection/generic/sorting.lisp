;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sorting ()
  ((key :type function)
   (predicate :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/sorting (key predicate)
  (make-projection 'sorting :key key :predicate predicate))

;;;;;;
;;; Construction

(def (macro e) sorting (key predicate)
  `(make-projection/sorting ,key ,predicate))

;;;;;;
;;; Printer

(def printer sorting (projection recursion input input-reference)
  (bind ((key (key-of projection))
         (predicate (predicate-of projection))
         (indices (iter (for index :from 0 :below (length input))
                        (collect index)))
         (sorted-indices (stable-sort indices predicate :key (lambda (index) (funcall key (elt input index)))))
         (output (coerce (iter (for output-index :from 0)
                               (for input-index :in sorted-indices)
                               (for element = (elt input input-index))
                               (collect element))
                         (cond ((stringp input)
                                'string)
                               ((listp input)
                                'list)
                               ((vectorp input)
                                'vector)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) nil))))

;;;;;;
;;; Reader

(def reader sorting (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion gesture-queue))
  operation)
