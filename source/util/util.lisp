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

(def special-variable *use-computed-class* #t)

(if *use-computed-class*
    (def computed-universe projectured ()
      ()
      (:computed-state-factory-name as))
    (def macro as (&body forms)
      `(progn ,@forms)))

(def function va (computed-state)
  (if *use-computed-class*
      (computed-state-value computed-state)
      computed-state))

(def (class* ea) sequence-box ()
  ((sequence :type sequence)
   (start :type integer)
   (end :type integer)))

(def function box (sequence start end)
  (make-instance 'sequence-box :sequence sequence :start start :end end))

(def function resource-pathname (name)
  (if (probe-file (asdf/component:component-pathname (asdf/system:find-system :projectured)))
      (asdf/system:system-relative-pathname :projectured name)
      (pathname (string+ (directory-namestring sb-ext:*runtime-pathname*) name))))

(def function form-type (form)
  (typecase form
    (null 'sequence)
    (cons 'sequence)
    (number 'number)
    (string 'string)
    (vector 'vector)
    (sequence/sequence 'sequence)
    (t (type-of form))))

(def function boolean-to-string (value)
  (if value "true" "false"))

(def function object-class-name (object)
  (form-type object)
  #+nil
  (class-name (class-of object)))

(def function object-class-symbol-name (object)
  (symbol-name (object-class-name object)))

(def function find-slot-reader (class slot)
  (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class))))
    (first (slot-definition-readers direct-slot))))

(def function tree-search (tree element)
  (or (equal tree element)
      (when (listp tree)
        (iter (for tree-element :in tree)
              (thereis (tree-search tree-element element))))))

(def function tree-replace (tree element replacement)
  (cond ((equal tree element)
         replacement)
        ((listp tree)
         (iter (for tree-element :in tree)
               (collect (tree-replace tree-element element replacement))))
        (t tree)))

(def function char=ignorecase (c1 c2)
  (char= (char-downcase c1) (char-downcase c2)))

(def function search-ignorecase (sequence1 sequence2)
  (search sequence1 sequence2 :test 'char=ignorecase))

(def function search-parts (root test &key (slot-provider (compose 'class-slots 'class-of)))
  (bind ((seen-set (make-hash-table))
         (result nil))
    (labels ((recurse (instance reference)
               (unless (gethash instance seen-set)
                 (setf (gethash instance seen-set) #t)
                 (when (funcall test instance)
                   (push (typed-reference (form-type instance) reference) result))
                 (typecase instance
                   (sequence
                    (iter (for index :from 0)
                          (for element :in-sequence instance)
                          (recurse element `((elt (the sequence document) ,index)
                                             ,@(typed-reference (form-type instance) reference)))))
                   (standard-object
                    (iter (with class = (class-of instance))
                          (for slot :in (funcall slot-provider instance))
                          (for slot-value = (slot-value-using-class class instance slot))
                          (for slot-reader = (find-slot-reader class slot))
                          (recurse slot-value `((,slot-reader (the ,(form-type instance) document))
                                                ,@(typed-reference (form-type instance) reference)))))))))
      (recurse root nil))
    (nreverse result)))

(def function longest-common-prefix (string-1 string-2)
  (iter (for index :from 0)
        (while (and (< index (length string-1))
                    (< index (length string-2))
                    (string= (subseq string-1 0 (1+ index))
                             (subseq string-2 0 (1+ index)))))
        (finally (return (subseq string-1 0 index)))))

(def (class* ea) alternative-function ()
  ((alternatives :type sequence)
   (selection :type positive-integer))
  (:metaclass funcallable-standard-class))

(def function make-alternative-function (alternatives &optional (selection 0))
  (bind ((instance (make-instance 'alternative-function :alternatives alternatives :selection selection)))
    (set-funcallable-instance-function instance (lambda (&rest args)
                                                  (apply (elt (alternatives-of instance) (selection-of instance)) args)))
    instance))

(def function deep-copy (instance)
  (labels ((recurse (instance)
             (etypecase instance
               ((or number string symbol pathname function sb-sys:system-area-pointer)
                instance)
               (sequence/sequence
                (make-sequence/sequence (iter (for element :in-sequence instance)
                                              (collect (recurse element)))
                                        :selection (recurse (selection-of instance))))
               (sequence
                (coerce (iter (for element :in-sequence instance)
                              (collect (recurse element)))
                        (type-of instance)))
               (standard-object
                (bind ((class (class-of instance))
                       (copy (allocate-instance class))
                       (slots (class-slots class)))
                  (iter (for slot :in slots)
                        (when (slot-boundp-using-class class instance slot)
                          (setf (slot-value-using-class class copy slot) (recurse (slot-value-using-class class instance slot)))))
                  copy)))))
    (recurse instance)))
