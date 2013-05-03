;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Number domain provides:
;;;;  - number classes provided by the Common Lisp implementation

;;;;;;
;;; Number document classes
;;;
;;; The number document classes are provided by the Common Lisp implementation.

;;;;;;
;;; Number document constructors
;;;
;;; The number document constructores are provided by the Common Lisp implementation.

;;;;;;
;;; Number operation classes

(def operation operation/number/replace-range (operation)
  ((document :type document)
   (target :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Number operation constructors

(def (function e) make-operation/number/replace-range (document target replacement)
  (make-instance 'operation/number/replace-range
                 :document document
                 :target target
                 :replacement replacement))

;;;;;;
;;; Number operation API implementation

(def method redo-operation ((operation operation/number/replace-range))
  (pattern-case (target-of operation)
    ((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b))
     (bind ((old-sequence (write-to-string (eval-reference (document-of operation) ?a)))
            (new-sequence (concatenate (form-type old-sequence)
                                       (subseq old-sequence 0 ?b) (replacement-of operation) (subseq old-sequence ?b))))
       (setf (eval-reference (document-of operation) ?a) (parse-number:parse-number new-sequence))
       ;; KLUDGE: forece recomputation
       (invalidate-computed-slot (document-of operation) 'content)))
    (?a
     (not-yet-implemented))))
