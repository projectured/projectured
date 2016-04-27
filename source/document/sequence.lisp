;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; TODO: rename to primitive/sequence or collection/sequence?

;;;;;;
;;; Document

(def document collection/sequence (sequence)
  ((elements :type sequence)
   (key :type function)
   (predicate :type function))
  (:documentation "This class represents a domain independent plain old sequence which is editable and has selection and identity."))

;;;;;;
;;; Construction

(def function make-collection/sequence (elements &key key predicate selection)
  (make-instance 'collection/sequence
                 :elements elements
                 :key key :predicate predicate
                 :selection selection))

;;;;;;
;;; Construction

(def macro collection/sequence ((&key key predicate selection) &body elements)
  `(make-collection/sequence (list-ll ,@elements) :key ,key :predicate ,predicate :selection ,selection))

;;;;;;
;;; API

(def method sb-sequence:length ((instance collection/sequence))
  (length (elements-of instance)))

(def method sb-sequence:elt ((instance collection/sequence) index)
  (elt (elements-of instance) index))

(def method (setf sb-sequence:elt) (new-value (instance collection/sequence) index)
  (setf (elt (elements-of instance) index) new-value))

(def method sb-sequence:make-sequence-like ((instance collection/sequence) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance 'collection/sequence
                 :elements (if initial-element?
                               (make-array (list length) :initial-element initial-element)
                               (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))
                 :selection (when (slot-boundp instance 'selection) (selection-of instance))))

;;;;;;
;;; Operation

(def operation operation/sequence/replace-range ()
  ((selection :type reference)
   (replacement :type sequence)))

(def operation operation/sequence/swap-ranges ()
  ((selection-1 :type reference)
   (selection-2 :type reference)))

;;;;;;
;;; Construction

(def function make-operation/sequence/replace-range (selection replacement)
  (make-instance 'operation/sequence/replace-range
                 :selection selection
                 :replacement replacement))

(def function make-operation/sequence/swap-ranges (selection-1 selection-2)
  (make-instance 'operation/sequence/swap-ranges
                 :selection-1 selection-1
                 :selection-2 selection-2))

;;;;;;
;;; Evaluator

(def evaluator operation/sequence/replace-range ()
  (bind ((selection (selection-of -operation-))
         (replacement (replacement-of -operation-)))
    (pattern-case (reverse selection)
      (((the sequence (subseq (the sequence document) ?start-index ?end-index))
        . ?rest)
       (bind ((reference (reverse ?rest))
              (flat-reference (flatten-reference reference))
              (old-document (eval-reference -document- flat-reference))
              (old-sequence (if (typep old-document 'collection/sequence)
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
              #+nil
              (new-index (+ ?start-index (length replacement)))
              (new-selection (when (<= 0 ?start-index (length new-sequence))
                               (bind ((index (min ?start-index (1- (length new-sequence))))
                                      (type (document-type (elt new-sequence index))))
                                 (append reference `((the ,type (elt (the sequence document) ,index))
                                                     (the ,type document)))))))
         ;; KLUDGE: to make sure we always end up with a computed-ll sequence
         (if (typep old-document 'collection/sequence)
             (setf (elements-of (eval-reference -document- flat-reference)) (ll new-sequence))
             (setf (eval-reference -document- flat-reference) (ll new-sequence)))
         (call-evaluator -document- (make-operation/replace-selection new-selection))))
      (?a
       (error "Unknown selection ~A" selection)))))

(def evaluator operation/sequence/swap-ranges ()
  (bind ((selection-1 (selection-1-of -operation-))
         (selection-2 (selection-2-of -operation-)))
    (not-yet-implemented)))

;;;;;;
;;; API

(def method hu.dwim.serializer:write-object-slots ((class standard-class) (object collection/sequence) context)
  (bind ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (hu.dwim.serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (ignore-errors (closer-mop:slot-definition-allocation slot)) :class)
        (hu.dwim.serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (bind ((value (closer-mop:slot-value-using-class class object slot)))
              (hu.dwim.serializer::serialize-element (if (eq (slot-definition-name slot) 'elements)
                                                         (coerce value 'list)
                                                         value)
                                                     context))
            (hu.dwim.serializer::write-unsigned-byte-8 hu.dwim.serializer::+unbound-slot-code+ context))))))

(def method hu.dwim.serializer:read-object-slots ((class standard-class) (prototype collection/sequence) context &key &allow-other-keys)
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
                          (ll value)
                          value)))))
    object))
