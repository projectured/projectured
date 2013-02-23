;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def (definer :available-flags "e") iomap (name supers slots &rest options)
  `(def computed-class* ,name ,supers
     ,(iter (for slot :in slots)
            (collect (append slot (list :computed-in 'projectured))))
     ,@options))

(def iomap iomap ()
  ((projection :type projection)
   (recursion :type projection)
   (input :type t)
   (output :type t)
   (reference-applier :type function)
   (forward-mapper :type function)
   (backward-mapper :type function))
  (:documentation "An IOMAP provides a bidirectional mapping between INPUT and OUTPUT."))

(def (function e) make-iomap (type &rest args &key reference-applier forward-mapper backward-mapper &allow-other-keys)
  (apply #'make-instance type
         :reference-applier (or reference-applier (find-reference-applier type))
         :forward-mapper (or forward-mapper (find-forward-mapper type))
         :backward-mapper (or backward-mapper (find-backward-mapper type))
         args))

;;;;;;
;;; Forward mapper

(def (namespace e) forward-mapper)

(def (definer e) forward-mapper (name arguments &body forms)
  `(setf (find-forward-mapper ',name) (lambda ,arguments ,@forms)))

(def (function e) map-forward (iomap input-reference function)
  (funcall (forward-mapper-of iomap) iomap input-reference function))

;;;;;;
;;; Backward mapper

(def (namespace e) backward-mapper)

(def (definer e) backward-mapper (name arguments &body forms)
  `(setf (find-backward-mapper ',name) (lambda ,arguments ,@forms)))

(def (function e) map-backward (iomap output-reference function)
  (funcall (backward-mapper-of iomap) iomap output-reference function))