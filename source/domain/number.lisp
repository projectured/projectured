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
  (bind (((:values reference start end)
          (pattern-case (target-of operation)
            ((the sequence (subseq (the ?type (?if (subtypep ?type 'sequence)) (write-to-string (the number ?a))) ?b ?c))
             (values ?a ?b ?c)))))
    (bind ((old-sequence (write-to-string (eval-reference (document-of operation) reference)))
           (new-sequence (concatenate (form-type old-sequence)
                                      (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end))))
      (setf (eval-reference (document-of operation) reference) (parse-number:parse-number new-sequence))
      (when *use-computed-class*
        ;; KLUDGE: forece recomputation
        (invalidate-computed-slot (document-of operation) 'content)))))
