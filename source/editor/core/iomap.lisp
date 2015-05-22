;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def (definer :available-flags "e") iomap (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'iomap) (member 'iomap supers))
                     supers
                     (append supers '(iomap)))))
    `(progn
       (def class* ,name ,supers ,slots ,@options (:metaclass computed-class))
       ,@(when (getf -options- :export) `((export ',name))))))

(def iomap iomap ()
  ((projection :type projection)
   (recursion :type projection)
   (input :type t)
   (input-reference :type reference)
   (output :type t))
  (:documentation "An IOMAP provides a bidirectional mapping between INPUT and OUTPUT."))

(def function make-iomap (type &rest args &key &allow-other-keys)
  (apply #'make-instance type args))

(def function make-iomap/object (projection recursion input input-reference output)
  (make-iomap 'iomap
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference (typed-reference (form-type input) input-reference)) :output output))
