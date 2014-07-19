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
            (((the string (subseq (the string document) ?b ?c))
              (the string (write-to-string (the number document)))
              . ?rest)
             (values `(,@(reverse ?rest) (the sequence document)) ?b ?c)))))
    (bind ((document (document-of operation))
           (flat-reference (reference/flatten reference))
           (old-sequence (write-to-string (eval-reference document flat-reference)))
           (new-sequence (concatenate (form-type old-sequence)
                                      (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end))))
      (setf (eval-reference document flat-reference) (parse-number:parse-number new-sequence))
      #+nil
      (when *use-computed-class*
        ;; KLUDGE: forece recomputation
        (invalidate-computed-slot document 'content))
      ;; KLUDGE: can't do this in a separate operation
      (bind ((character-index (+ start (length (replacement-of operation)))))
        (redo-operation (make-operation/replace-selection document `((the string (subseq (the string document) ,character-index ,character-index))
                                                                     (the string (write-to-string (the number document)))
                                                                     ,@(rest (reverse reference)))))))))
