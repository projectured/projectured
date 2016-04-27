;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection type-dispatching ()
  ((type-projection-pairs :type list)))

;;;;;;
;;; Construction

(def function make-projection/type-dispatching (type-projection-pairs)
  (make-projection 'type-dispatching :type-projection-pairs type-projection-pairs))

;;;;;;
;;; Construction

(def macro type-dispatching (&body forms)
  `(make-projection/type-dispatching (list ,@(iter (for (type projection) :in forms)
                                                   (collect `(list ',type ,projection))))))

;;;;;;
;;; Forward mapper

(def forward-mapper type-dispatching ()
  (iter (with type-projection-pairs = (type-projection-pairs-of -projection-))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep -printer-input- type)
          (return (funcall (forward-mapper-of projection) (content-iomap-of -printer-iomap-) -reference-)))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))

;;;;;;
;;; Backward mapper

(def backward-mapper type-dispatching ()
  (iter (with type-projection-pairs = (type-projection-pairs-of -projection-))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep -printer-input- type)
          (return (funcall (backward-mapper-of projection) (content-iomap-of -printer-iomap-) -reference-)))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))

;;;;;;
;;; Printer

(def printer type-dispatching ()
  (iter (with type-projection-pairs = (type-projection-pairs-of -projection-))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep -input- type)
          (return (bind ((content-iomap (as (call-printer projection -recursion- -input- -input-reference-))))
                    (make-iomap/content -projection- -recursion- -input- -input-reference- (as (output-of (va content-iomap))) content-iomap))))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))

;;;;;;
;;; Reader

(def reader type-dispatching ()
  (iter (with type-projection-pairs = (type-projection-pairs-of -projection-))
        (for (type projection) :in-sequence type-projection-pairs)
        (when (typep (input-of -printer-iomap-) type)
          (return (call-reader projection -recursion- -input- (content-iomap-of -printer-iomap-))))
        (finally (error "Input is not an instance of the given types ~A" (mapcar 'first type-projection-pairs)))))
