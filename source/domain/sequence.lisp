;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Sequence domain provides:
;;;;  - sequence

;;;;;;
;;; Sequence document classes
;;;
;;; The sequence document classes are provided by the Common Lisp implementation.

;;;;;;
;;; Sequence document constructors
;;;
;;; The sequence document constructores are provided by the Common Lisp implementation.

;;;;;;
;;; Sequence operation classes

(def operation operation/sequence/replace-element-range (operation)
  ((document :type t)
   (target :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Sequence operation constructors

(def (function e) make-operation/sequence/replace-element-range (document target replacement)
  (make-instance 'operation/sequence/replace-element-range
                 :document document
                 :target target
                 :replacement replacement))

;;;;;;
;;; Sequence operation API implementation

(def method redo-operation ((operation operation/sequence/replace-element-range))
  (pattern-case (target-of operation)
    ((the sequence-position (pos (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b))
     (bind ((document (document-of operation))
            (old-sequence (eval-reference document ?a))
            (new-sequence (concatenate (form-type old-sequence)
                                       (subseq old-sequence 0 ?b) (replacement-of operation) (subseq old-sequence ?b))))
       (setf (eval-reference document ?a) new-sequence)))
    (?a
     (not-yet-implemented))))
