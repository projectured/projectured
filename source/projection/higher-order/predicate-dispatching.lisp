;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection predicate-dispatching ()
  ((predicate-projection-pairs :type list)))

;;;;;;
;;; Construction

(def function make-projection/predicate-dispatching (predicate-projection-pairs)
  (make-projection 'predicate-dispatching :predicate-projection-pairs predicate-projection-pairs))

;;;;;;
;;; Construction

(def macro predicate-dispatching (&body predicate-projection-pairs)
  `(make-projection/predicate-dispatching (list ,@(iter (for (predicate projection) :in predicate-projection-pairs)
                                                        (collect `(list ',predicate ,projection))))))
;;;;;;
;;; Printer

(def printer predicate-dispatching ()
  (iter (with predicate-projection-pairs = (predicate-projection-pairs-of -projection-))
        (for (predicate projection) :in-sequence predicate-projection-pairs)
        (when (funcall predicate -input-)
          (return (call-printer -projection- -projection- -input- -input-reference-)))
        (finally (error "No predicate matched input"))))

;;;;;;
;;; Reader

(def reader predicate-dispatching ()
  -input-)
