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
  (bind (((:values reference start end)
          (pattern-case (target-of operation)
            ((the sequence-position (pos (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b))
             (values ?a ?b ?b))
            ((the sequence (subseq (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b ?c))
             (values ?a ?b ?c))
            ((elt (the list ?a) ?b)
             (values ?a ?b (1+ ?b)))
            (?a
             (not-yet-implemented))))
         (document (document-of operation))
         (old-sequence (eval-reference document reference))
         (new-sequence (concatenate (form-type old-sequence)
                                    (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end))))
    ;; KLUDGE: somewhat kludgie to keep the original identity of the string
    (if (adjustable-array-p old-sequence)
        (progn
          (adjust-array old-sequence (length new-sequence))
          (replace old-sequence new-sequence))
        (setf (eval-reference document reference) new-sequence))
    (when *use-computed-class*
      ;; KLUDGE: forece recomputation
      (invalidate-computed-slot (document-of operation) 'content))))
