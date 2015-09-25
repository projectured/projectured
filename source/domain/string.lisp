;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/string (sequence)
  ((value :type (or null string)))
  (:documentation "This class represents a domain independent plain old string which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-document/string (value &key selection)
  (make-instance 'document/string :value value :selection selection))

;;;;;;
;;; Construction

(def macro document/string ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-document/string ,(first value) :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/string/replace-range ()
  ((document :type t)
   (selection :type reference)
   (replacement :type string)))

;;;;;;
;;; Construction

(def function make-operation/string/replace-range (document selection replacement)
  (make-instance 'operation/string/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; API

(def method print-object ((instance document/string) stream)
  (awhen (value-of instance)
    (write it :stream stream)))

(def method sb-sequence:length ((instance document/string))
  (length (value-of instance)))

(def method sb-sequence:elt ((instance document/string) index)
  (elt (value-of instance) index))

(def method (setf sb-sequence:elt) (new-value (instance document/string) index)
  (setf (elt (value-of instance) index) new-value))

(def method sb-sequence:adjust-sequence ((instance document/string) length &key (initial-element nil initial-element?) initial-contents)
  (setf (value-of instance) (if initial-element?
                                (make-array (list length) :initial-element initial-element)
                                (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

(def method sb-sequence:subseq ((instance document/string) start &optional end)
  (make-instance (class-of instance) :value (subseq (value-of instance) start end)))

(def method sb-sequence:replace ((instance-1 document/string) (instance-2 document/string) &rest args &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (apply #'replace (value-of instance-1) (value-of instance-2) args)
  instance-1)

(def method sb-sequence:make-sequence-like ((instance document/string) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance (class-of instance) :value (if initial-element?
                                               (make-array (list length) :initial-element initial-element)
                                               (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

(def method run-operation ((operation operation/string/replace-range))
  (bind ((document (document-of operation))
         (selection (selection-of operation))
         (replacement (replacement-of operation)))
    (pattern-case (reverse selection)
      (((the string (subseq (the string document) ?start-index ?end-index))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (reference/flatten reference))
              (old-document (eval-reference document flat-reference))
              (old-string (if (typep old-document 'document/string) (value-of old-document) old-document))
              (new-string (concatenate 'string (subseq old-string 0 ?start-index) replacement (subseq old-string ?end-index)))
              (new-index (+ ?start-index (length replacement)))
              (new-selection (append reference `((the ,(form-type new-string) (subseq (the ,(form-type old-string) document) ,new-index ,new-index))))))
         (if (typep old-document 'document/string)
             (setf (value-of (eval-reference document flat-reference)) new-string)
             (setf (eval-reference document flat-reference) new-string))
         (run-operation (make-operation/replace-selection document new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))
