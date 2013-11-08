;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) sorting ()
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

(def printer sorting (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (key (key-of projection))
         (predicate (predicate-of projection))
         (type (form-type input))
         (indices (iter (for index :from 0 :below (length input))
                        (collect index)))
         (sorted-indices (stable-sort indices predicate :key (lambda (index) (funcall key (elt input index)))))
         (output (coerce (iter (for output-index :from 0)
                               (for input-index :in sorted-indices)
                               (for element = (elt input input-index))
                               (push (if (stringp input)
                                         (make-iomap/string input input-reference input-index
                                                            input output-reference output-index
                                                            1)
                                         (make-iomap/recursive projection recursion
                                                               element `(elt (the ,type ,input-reference) ,input-index)
                                                               element `(elt (the ,type ,output-reference) ,output-index)))
                                     child-iomaps)
                               (collect element))
                         (cond ((stringp input)
                                'string)
                               ((listp input)
                                'list)
                               ((vectorp input)
                                'vector)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader sorting (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap gesture-queue))
  (operation/read-backward operation projection-iomap document-iomap))
