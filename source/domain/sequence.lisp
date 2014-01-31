;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

;; TODO: rename?
(def document sequence/sequence (document/base sequence)
  ((elements :type list)))

;;;;;;
;;; Construction

(def (function e) make-sequence/sequence (elements &key selection)
  (make-instance 'sequence/sequence :elements elements :selection selection))

;;;;;;
;;; Construction

(def (macro e) sequence/sequence ((&key selection) &body elements)
  `(make-sequence/sequence (list ,@elements) :selection ,selection))

(def method sb-sequence:elt ((instance sequence/sequence) index)
  (elt (elements-of instance) index))

(def method (setf sb-sequence:elt) (new-value (instance sequence/sequence) index)
  (setf (elt (elements-of instance) index) new-value))

(def method sb-sequence:length ((instance sequence/sequence))
  (length (elements-of instance)))

(def method sb-sequence:adjust-sequence ((instance sequence/sequence) length &key initial-element (initial-contents nil initial-contents?))
  (setf (elements-of instance) (if initial-contents?
                                   initial-contents
                                   (make-list length :initial-element initial-element))))

(def method sb-sequence:make-sequence-like ((instance sequence/sequence) length &key initial-element (initial-contents nil initial-contents?))
  (make-instance 'sequence/sequence
                 :elements (if initial-contents?
                               initial-contents
                               (make-list length :initial-element initial-element))
                 :selection (selection-of instance)))

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

;; KLUDGE: TODO: this!
(def (function e) reference/flatten (reference &optional (result 'document))
  (if (consp reference)
      (reference/flatten (rest reference) (tree-replace (car reference) 'document result))
      result))

(def method redo-operation ((operation operation/sequence/replace-element-range))
  (bind (((:values reference start end)
          (pattern-case (target-of operation)
            (((the sequence-position (pos (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b)) . ?rest)
             (values `(,@(reverse ?rest) (the string ,?a)) ?b ?b))
            (((the sequence-position (text/pos (the text/text ?a) ?b)) . ?rest)
             (values `(,@(reverse ?rest) (the text/text ,?a)) ?b ?b))
            (((the sequence (subseq (the ?type (?if (subtypep ?type 'sequence)) ?a) ?b ?c)) . ?rest)
             (values `(,@(reverse ?rest) (the string ,?a)) ?b ?c))
            (((the sequence (text/subseq (the text/text ?a) ?b ?c)) . ?rest)
             (values `(,@(reverse ?rest) (the text/text ,?a)) ?b ?c))
            (((elt (the sequence ?a) ?b) . ?rest)
             (values ?a ?b (1+ ?b)))
            (?a
             (not-yet-implemented))))
         (document (document-of operation))
         (reference (reference/flatten reference))
         (old-sequence (eval-reference document reference))
         (new-sequence (etypecase old-sequence
                         (sequence (concatenate (form-type old-sequence)
                                                (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end)))
                         (text/text (text/concatenate (text/substring* old-sequence 0 start)
                                                      (make-text/text (list (make-text/string (replacement-of operation) :font *font/default* :font-color *color/default*)))
                                                      (text/substring* old-sequence end))))))
    ;; KLUDGE: somewhat kludgie to keep the original identity of the string
    (cond ((and (arrayp old-sequence) (adjustable-array-p old-sequence))
           (progn
             (adjust-array old-sequence (length new-sequence))
             (replace old-sequence new-sequence)))
          ((typep old-sequence 'text/text)
           (setf (elements-of old-sequence) (elements-of new-sequence)))
          (t
           (setf (eval-reference document reference) new-sequence)))
    (when *use-computed-class*
      ;; KLUDGE: forece recomputation
      (invalidate-computed-slot (document-of operation) 'content))))
