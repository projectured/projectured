;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection type-dispatching ()
  ((type-projection-pairs :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/type-dispatching (type-projection-pairs)
  (make-projection 'type-dispatching :type-projection-pairs type-projection-pairs))

;;;;;;
;;; Construction

(def (macro e) type-dispatching (&body forms)
  `(make-projection/type-dispatching (list ,@(iter (for (type projection) :in forms)
                                                   (collect `(list ',type ,projection))))))

;;;;;;
;;; Printer

(def printer type-dispatching (projection recursion input input-reference)
  (iter (with type-projection-pairs = (type-projection-pairs-of projection))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep input type)
          (return (call-printer projection recursion input input-reference)))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))

;;;;;;
;;; Reader

(def reader type-dispatching (projection recursion input printer-iomap)
  (iter (with type-projection-pairs = (type-projection-pairs-of projection))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep (input-of printer-iomap) type)
          (return (call-reader projection recursion input printer-iomap)))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))
