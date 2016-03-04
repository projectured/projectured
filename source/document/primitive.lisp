;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document primitive/base ()
  ())

(def document primitive/boolean (primitive/base)
  ((value :type boolean))
  (:documentation "This class represents a primitve domain independent boolean which is editable and has selection and identity."))

(def document primitive/number (primitive/base)
  ((value :type (or null number)))
  (:documentation "This class represents a primitve domain independent number which is editable and has selection and identity."))

(def document primitive/string (primitive/base sequence)
  ((value :type (or null string)))
  (:documentation "This class represents a primitve domain independent string which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-primitive/boolean (value &key selection)
  (make-instance 'primitive/boolean :value value :selection selection))

(def function make-primitive/number (value &key selection)
  (make-instance 'primitive/number :value value :selection selection))

(def function make-primitive/string (value &key selection)
  (make-instance 'primitive/string :value value :selection selection))

;;;;;;
;;; Construction

(def macro primitive/boolean ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-primitive/boolean ,(first value) :selection ,selection))

(def macro primitive/number ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-primitive/number ,(first value) :selection ,selection))

(def macro primitive/string ((&key selection) &body value)
  (assert (length= 1 value))
  `(make-primitive/string ,(first value) :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/number/replace-range ()
  ((document :type document)
   (selection :type reference)
   (replacement :type sequence)))

(def operation operation/string/replace-range ()
  ((document :type t)
   (selection :type reference)
   (replacement :type string)))

;;;;;;
;;; Construction

(def function make-operation/number/replace-range (document selection replacement)
  (make-instance 'operation/number/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

(def function make-operation/string/replace-range (document selection replacement)
  (make-instance 'operation/string/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; Evaluator

(def evaluator operation/number/replace-range (operation)
  (bind ((document (document-of operation))
         (selection (selection-of operation))
         (replacement (replacement-of operation)))
    (pattern-case (reverse selection)
      (((the string (subseq (the string document) ?start-index ?end-index))
        (the string (write-to-string (the ?type document)))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (flatten-reference reference))
              (old-document (eval-reference document flat-reference))
              (old-number (if (typep old-document 'primitive/number)
                              (value-of old-document)
                              old-document))
              (old-sequence (write-number old-number nil))
              (new-sequence (concatenate (document-type old-sequence)
                                         (subseq old-sequence 0 ?start-index) replacement (subseq old-sequence ?end-index)))
              (new-number (unless (string= new-sequence "")
                            (parse-number:parse-number new-sequence)))
              (new-index (+ ?start-index (length replacement)))
              (new-selection (append reference `((the string (write-to-string (the ,?type document)))
                                                 (the string (subseq (the string document) ,new-index ,new-index))))))
         (if (typep old-document 'primitive/number)
             (setf (value-of (eval-reference document flat-reference)) new-number)
             (setf (eval-reference document flat-reference) new-number))
         (call-evaluator (make-operation/replace-selection document new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))

(def evaluator operation/string/replace-range (operation)
  (bind ((document (document-of operation))
         (selection (selection-of operation))
         (replacement (replacement-of operation)))
    (pattern-case (reverse selection)
      (((the string (subseq (the string document) ?start-index ?end-index))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (flatten-reference reference))
              (old-document (eval-reference document flat-reference))
              (old-string (if (typep old-document 'primitive/string) (value-of old-document) old-document))
              (new-string (concatenate 'string (subseq old-string 0 ?start-index) replacement (subseq old-string ?end-index)))
              (new-index (+ ?start-index (length replacement)))
              (new-selection (append reference `((the ,(document-type new-string) (subseq (the ,(document-type old-string) document) ,new-index ,new-index))))))
         (if (typep old-document 'primitive/string)
             (setf (value-of (eval-reference document flat-reference)) new-string)
             (setf (eval-reference document flat-reference) new-string))
         (call-evaluator (make-operation/replace-selection document new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))

;;;;;;
;;; API

(def method print-object ((instance primitive/number) stream)
  (awhen (value-of instance)
    (write it :stream stream)))

(def method print-object ((instance primitive/string) stream)
  (awhen (value-of instance)
    (write it :stream stream)))

(def method sb-sequence:length ((instance primitive/string))
  (length (value-of instance)))

(def method sb-sequence:elt ((instance primitive/string) index)
  (elt (value-of instance) index))

(def method (setf sb-sequence:elt) (new-value (instance primitive/string) index)
  (setf (elt (value-of instance) index) new-value))

(def method sb-sequence:adjust-sequence ((instance primitive/string) length &key (initial-element nil initial-element?) initial-contents)
  (setf (value-of instance) (if initial-element?
                                (make-array (list length) :initial-element initial-element)
                                (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

(def method sb-sequence:subseq ((instance primitive/string) start &optional end)
  (make-instance (class-of instance) :value (subseq (value-of instance) start end)))

(def method sb-sequence:replace ((instance-1 primitive/string) (instance-2 primitive/string) &rest args &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (apply #'replace (value-of instance-1) (value-of instance-2) args)
  instance-1)

(def method sb-sequence:make-sequence-like ((instance primitive/string) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance (class-of instance) :value (if initial-element?
                                               (make-array (list length) :initial-element initial-element)
                                               (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))
