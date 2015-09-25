;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/sequence (sequence)
  ((elements :type sequence)
   (key :type function)
   (predicate :type function))
  (:documentation "This class represents a domain independent plain old sequence which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-document/sequence (elements &key key predicate selection)
  (make-instance 'document/sequence
                 :elements elements
                 :key key :predicate predicate
                 :selection selection))

;;;;;;
;;; Construction

(def macro document/sequence ((&key key predicate selection) &body elements)
  `(make-document/sequence (list-ll ,@elements) :key ,key :predicate ,predicate :selection ,selection))

;;;;;;
;;; API

(def method sb-sequence:length ((instance document/sequence))
  (length (elements-of instance)))

(def method sb-sequence:elt ((instance document/sequence) index)
  (elt (elements-of instance) index))

(def method (setf sb-sequence:elt) (new-value (instance document/sequence) index)
  (setf (elt (elements-of instance) index) new-value))

(def method sb-sequence:make-sequence-like ((instance document/sequence) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance 'document/sequence
                 :elements (if initial-element?
                               (make-array (list length) :initial-element initial-element)
                               (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))
                 :selection (when (slot-boundp instance 'selection) (selection-of instance))))

;;;;;;
;;; Operation

(def operation operation/sequence/replace-range ()
  ((document :type t)
   (selection :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Construction

(def function make-operation/sequence/replace-range (document selection replacement)
  (make-instance 'operation/sequence/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; API

(def method hu.dwim.serializer:write-object-slots ((class standard-class) (object document/sequence) context)
  (bind ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (hu.dwim.serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (ignore-errors (closer-mop:slot-definition-allocation slot)) :class)
        (hu.dwim.serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (bind ((value (closer-mop:slot-value-using-class class object slot)))
              (hu.dwim.serializer::serialize-element (if (eq (slot-definition-name slot) 'elements)
                                                         (map 'vector (lambda (element) (if (computed-state-p element) (va element) element)) value)
                                                         value)
                                                     context))
            (hu.dwim.serializer::write-unsigned-byte-8 hu.dwim.serializer::+unbound-slot-code+ context))))))

(def method hu.dwim.serializer:read-object-slots ((class standard-class) (prototype document/sequence) context &key &allow-other-keys)
  (bind ((object (allocate-instance class)))
    (hu.dwim.serializer::announce-identity object context)
    (iter (repeat (the fixnum (hu.dwim.serializer::read-variable-length-positive-integer context)))
          (for slot-name = (hu.dwim.serializer::deserialize-symbol context))
          (if (eq hu.dwim.serializer::+unbound-slot-code+ (hu.dwim.serializer::read-unsigned-byte-8 context))
              (slot-makunbound object slot-name)
              (bind ((value (progn
                              (hu.dwim.serializer::unread-unsigned-byte-8 context)
                              (hu.dwim.serializer::deserialize-element context))))
                (setf (slot-value object slot-name)
                      (if (eq slot-name 'elements)
                          (map 'vector (lambda (element) (as element)) value)
                          value)))))
    object))

(def method run-operation ((operation operation/sequence/replace-range))
  (bind ((document (document-of operation))
         (selection (selection-of operation))
         (replacement (replacement-of operation)))
    (pattern-case (reverse selection)
      (((the sequence (subseq (the sequence document) ?start-index ?end-index))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (reference/flatten reference))
              (old-document (eval-reference document flat-reference))
              (old-sequence (if (typep old-document 'document/sequence)
                                (elements-of old-document)
                                old-document))
              (new-sequence (concatenate
                             ;; KLUDGE: etypecase computed-ll doesn't work for some reason
                             (if (eq (type-of old-sequence) 'computed-ll)
                                 'computed-ll
                                 (etypecase old-sequence
                                   (list 'list)
                                   (vector 'vector)))
                             (subseq old-sequence 0 ?start-index) replacement (subseq old-sequence ?end-index)))
              (new-index (+ ?start-index (length replacement)))
              (new-selection (when (<= 0 ?start-index (length new-sequence))
                               (bind ((index (min ?start-index (1- (length new-sequence))))
                                      (type (form-type (elt new-sequence index))))
                                 (append reference `((the ,type (elt (the sequence document) ,index))
                                                     (the ,type document)))))))
         ;; KLUDGE: to make sure we always end up with a computed-ll sequence
         (if (typep old-document 'document/sequence)
             (setf (elements-of (eval-reference document flat-reference)) (ll new-sequence))
             (setf (eval-reference document flat-reference) (ll new-sequence)))
         (run-operation (make-operation/replace-selection document new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))
