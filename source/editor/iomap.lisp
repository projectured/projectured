;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def (definer :available-flags "e") iomap (name supers slots &rest options)
  (if *use-computed-class*
      `(def computed-class* ,name ,supers
         ,(iter (for slot :in slots)
                (collect (append slot (list :computed-in 'projectured))))
         ,@options)
      `(def class* ,name ,supers ,slots ,@options)))

(def iomap iomap ()
  ((projection :type projection)
   (recursion :type projection)
   (input :type t)
   (input-reference :type reference)
   (output :type t))
  (:documentation "An IOMAP provides a bidirectional mapping between INPUT and OUTPUT."))

(def (function e) make-iomap (type &rest args &key &allow-other-keys)
  (apply #'make-instance type args))

(def (function e) make-iomap/object (projection recursion input input-reference output)
  (make-iomap 'iomap
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference (typed-reference (form-type input) input-reference)) :output output))
