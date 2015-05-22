;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document document/sequence (computed-sequence)
  ())

;;;;;;
;;; Construction

(def function make-document/sequence (elements &key selection)
  (make-instance 'document/sequence
                 :elements elements
                 :selection selection))

;;;;;;
;;; Construction

(def macro document/sequence ((&key selection) &body elements)
  `(make-document/sequence (list-ll ,@elements) :selection ,selection))

(def method sb-sequence:make-sequence-like ((instance document/sequence) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance 'document/sequence
                 :elements (if initial-element?
                               (make-array (list length) :initial-element initial-element)
                               (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))
                 :selection (when (slot-boundp instance 'selection) (selection-of instance))))

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

(def operation operation/sequence/replace-range ()
  ((document :type t)
   (selection :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Sequence operation constructors

(def function make-operation/sequence/replace-range (document selection replacement)
  (make-instance 'operation/sequence/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; Sequence operation API implementation

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

;; KLUDGE: TODO: this!
(def function reference/flatten (reference &optional (result 'document))
  (if (consp reference)
      (reference/flatten (rest reference) (tree-replace (car reference) 'document result))
      result))

(def method run-operation ((operation operation/sequence/replace-range))
  (bind (((:values reference start end)
          (pattern-case (reverse (selection-of operation))
            (((the string (subseq (the ?type (?if (subtypep ?type 'string)) ?a) ?b ?c)) . ?rest)
             (values `(,@(reverse ?rest) (the string ,?a)) ?b ?c))
            (((the sequence (subseq (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b ?c)) . ?rest)
             (values `(,@(reverse ?rest) (the sequence ,?a)) ?b ?c))
            (?a
             (not-yet-implemented))))
         (document (document-of operation))
         (flat-reference (reference/flatten reference))
         (old-sequence (eval-reference document flat-reference))
         (new-sequence (etypecase old-sequence
                         (document/sequence (make-document/sequence (concatenate 'vector
                                                                                 (subseq (elements-of old-sequence) 0 start)
                                                                                 (replacement-of operation)
                                                                                 (subseq (elements-of old-sequence) end))
                                                                    :selection (selection-of old-sequence)))
                         (sequence (concatenate (form-type old-sequence)
                                                (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end))))))
    ;; KLUDGE: to make sure we always end up with a computed-ll sequence
    (unless (typep new-sequence 'string)
      (setf new-sequence (ll new-sequence)))
    ;; KLUDGE: somewhat kludgie to keep the original identity of the string
    (cond ((and (arrayp old-sequence) (adjustable-array-p old-sequence))
           (progn
             (adjust-array old-sequence (length new-sequence))
             (replace old-sequence new-sequence)))
          (t
           (setf (eval-reference document flat-reference) new-sequence)))
    ;; KLUDGE: can't do this in a separate operation
    (bind ((index (+ start (length (replacement-of operation)))))
      (run-operation (make-operation/replace-selection document
                                                       (if (eq 'sequence (form-type new-sequence))
                                                           (when (<= 0 start (1- (length new-sequence)))
                                                             (bind ((type (form-type (elt new-sequence start))))
                                                               (append (butlast reference)
                                                                       `(#+nil (the ,type document) ;; TODO: to make delete from sequence work, but breaks insertion
                                                                         (the ,type (elt (the ,(form-type old-sequence) document) ,start))))))
                                                           (append (butlast reference)
                                                                   `((the ,(form-type new-sequence) (subseq (the ,(form-type old-sequence) document) ,index ,index))))))))))
