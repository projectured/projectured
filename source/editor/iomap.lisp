;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def definer iomap (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'iomap) (member 'iomap supers))
                     supers
                     (append supers '(iomap)))))
    `(def class* ,name ,supers ,slots ,@options (:metaclass computed-class))))

;;;;;;
;;; Class

(def iomap iomap ()
  ((projection :type projection)
   (recursion :type projection)
   (input :type t)
   (input-reference :type reference)
   (output :type t))
  (:documentation "An IOMAP provides a bidirectional mapping between INPUT and OUTPUT."))

;;;;;;
;;; Construction

(def function make-iomap (projection recursion input input-reference output)
  (make-instance 'iomap
                 :projection projection :recursion recursion
                 :input input :input-reference (when input-reference (typed-reference (document-type input) input-reference))
                 :output output))
