;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/number ()
  ((value :type (or null number)))
  (:documentation "This class represents a domain independent plain old number which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-document/number (value &key selection)
  (make-instance 'document/number :value value :selection selection))

;;;;;;
;;; Construction

(def macro document/number ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-document/number ,(first value) :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/number/replace-range ()
  ((document :type document)
   (selection :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Construction

(def function make-operation/number/replace-range (document selection replacement)
  (make-instance 'operation/number/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; API

(def method print-object ((instance document/number) stream)
  (awhen (value-of instance)
    (write it :stream stream)))

(def method run-operation ((operation operation/number/replace-range))
  (bind ((document (document-of operation))
         (selection (selection-of operation))
         (replacement (replacement-of operation)))
    (pattern-case (reverse selection)
      (((the string (subseq (the string document) ?start-index ?end-index))
        (the string (write-to-string (the ?type document)))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (reference/flatten reference))
              (old-document (eval-reference document flat-reference))
              (old-number (if (typep old-document 'document/number)
                              (value-of old-document)
                              old-document))
              (old-sequence (write-number old-number nil))
              (new-sequence (concatenate (form-type old-sequence)
                                         (subseq old-sequence 0 ?start-index) replacement (subseq old-sequence ?end-index)))
              (new-number (unless (string= new-sequence "")
                            (parse-number:parse-number new-sequence)))
              (new-index (+ ?start-index (length replacement)))
              (new-selection (append reference `((the string (write-to-string (the ,?type document)))
                                                 (the string (subseq (the string document) ,new-index ,new-index))))))
         (if (typep old-document 'document/number)
             (setf (value-of (eval-reference document flat-reference)) new-number)
             (setf (eval-reference document flat-reference) new-number))
         (run-operation (make-operation/replace-selection document new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))
