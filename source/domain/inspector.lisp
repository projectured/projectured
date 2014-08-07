;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document inspector/base ()
  ())

(def document inspector/object (inspector/base)
  ((instance :type t)
   (expanded :type boolean)
   (slot-values :type sequence)))

(def document inspector/object-slot (inspector/base)
  ((instance :type t)
   (slot :type standard-effective-slot-definition)
   (value :type t)))

;;;;;;
;;; Construction

(def function make-inspector/object (instance &key (expanded #t))
  (make-instance 'inspector/object
                 :instance instance
                 :expanded expanded
                 :slot-values (iter (for slot :in (class-slots (class-of instance)))
                                    (collect (make-inspector/object-slot instance slot)))))

(def function make-inspector/object-slot (instance slot-or-name)
  (bind ((slot (if (symbolp slot-or-name)
                   (find-slot (class-of instance) slot-or-name)
                   slot-or-name)))
    (make-instance 'inspector/object-slot
                   :instance instance
                   :slot slot
                   :value (bind ((value (slot-value-using-class (class-of instance) instance slot)))
                            (make-inspector/object value :expanded #f)))))

;;;;;;
;;; Construction

(def macro inspector/object ((&key) &body instance)
  `(make-inspector/object ,(first instance)))

(def macro inspector/object-slot ((&key) &body instance-slot-pair)
  `(make-inspector/object ,(first instance-slot-pair) ,(second instance-slot-pair)))
