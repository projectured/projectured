;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection predicate-dispatching ()
  ((predicate-projection-pairs :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/predicate-dispatching (predicate-projection-pairs)
  (make-projection 'predicate-dispatching :predicate-projection-pairs predicate-projection-pairs))

;;;;;;
;;; Construction

(def (macro e) predicate-dispatching (&body predicate-projection-pairs)
  `(make-projection/predicate-dispatching (list ,@(iter (for (predicate projection) :in predicate-projection-pairs)
                                                        (collect `(list ',predicate ,projection))))))
;;;;;;
;;; Printer

(def printer predicate-dispatching (projection recursion iomap input input-reference output-reference)
  (declare (ignore recursion))
  (iter (with predicate-projection-pairs = (predicate-projection-pairs-of projection))
        (for (predicate projection) :in-sequence predicate-projection-pairs)
        (when (funcall predicate input)
          (return (funcall (printer-of projection) projection projection iomap input input-reference output-reference)))
        (finally (error "No predicate matched input"))))

;;;;;;
;;; Reader

(def reader predicate-dispatching (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
