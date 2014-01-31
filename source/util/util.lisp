;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Util

(def logger editor ())

(def logger printer ())

(def logger reader ())

(def logger backend ())

(def special-variable *use-computed-class* #f)

(if *use-computed-class*
    (def (computed-universe e) projectured ()
      ()
      (:computed-state-factory-name as))
    (def macro as (&body forms)
      `(progn ,@forms)))

(def (function e) computed-state-value* (computed-state)
  (if *use-computed-class*
      (computed-state-value computed-state)
      computed-state))

(def (class* ea) sequence-position ()
  ((sequence :type sequence)
   (index :type integer)))

(def (function e) pos (sequence index)
  (make-instance 'sequence-position :sequence sequence :index index))

(def (class* ea) sequence-box ()
  ((sequence :type sequence)
   (start :type integer)
   (end :type integer)))

(def (function e) box (sequence start end)
  (make-instance 'sequence-box :sequence sequence :start start :end end))

(def (function e) form-type (form)
  (typecase form
    (null 'sequence)
    (cons 'sequence)
    (number 'number)
    (string 'string)
    (vector 'vector)
    (t (type-of form))))

(def (function e) boolean-to-string (value)
  (if value "true" "false"))

(def (function e) object-class-name (object)
  (form-type object)
  #+nil
  (class-name (class-of object)))

(def (function e) object-class-symbol-name (object)
  (symbol-name (object-class-name object)))

(def (function e) find-slot-reader (class slot)
  (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class))))
    (first (slot-definition-readers direct-slot))))

(def (function e) tree-search (tree element)
  (or (equal tree element)
      (when (listp tree)
        (iter (for tree-element :in tree)
              (thereis (tree-search tree-element element))))))

(def (function e) tree-replace (tree element replacement)
  (cond ((equal tree element)
         replacement)
        ((listp tree)
         (iter (for tree-element :in tree)
               (collect (tree-replace tree-element element replacement))))
        (t tree)))

(def (function e) rempend (l1 l2)
  (bind ((end (- (length l1) (length l2))))
    (if (equal (subseq l1 end) l2)
        (subseq l1 0 end)
        #+nil
        (error "Not found"))))

(def (class* ea) alternative-function ()
  ((alternatives :type sequence)
   (selection :type positive-integer))
  (:metaclass funcallable-standard-class))

(def (function e) make-alternative-function (alternatives &optional (selection 0))
  (bind ((instance (make-instance 'alternative-function :alternatives alternatives :selection selection)))
    (set-funcallable-instance-function instance (lambda (&rest args)
                                                  (apply (elt (alternatives-of instance) (selection-of instance)) args)))
    instance))
