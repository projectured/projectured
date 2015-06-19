;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Meta classes

(def class computed-class (standard-class)
  ())

(def class computed-object (standard-object)
  ())

(def class computed-slot-definition (standard-slot-definition)
  ())

(def class computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(def class computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(def method validate-superclass ((subclass computed-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

;;; make sure computed-object is among the supers (thanks to Pascal Constanza)
(def method initialize-instance :around ((class computed-class) &rest initargs &key direct-superclasses)
  (if (loop for class in direct-superclasses
            thereis (ignore-errors (subtypep class (find-class 'computed-object))))
      (call-next-method)
      (apply #'call-next-method class :direct-superclasses (append direct-superclasses (list (find-class 'computed-object))) initargs)))

(def method reinitialize-instance :around ((class computed-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed this is exactly like above
      (if (loop for class in direct-superclasses
                thereis (ignore-errors (subtypep class (find-class 'computed-object))))
          (call-next-method)
          (apply #'call-next-method class :direct-superclasses (append direct-superclasses (list (find-class 'computed-object))) initargs))
      ;; if direct superclasses are not explicitly passed we _must_ not change anything
      (call-next-method)))

(def method shared-initialize :around ((computed-slot-definition computed-direct-slot-definition) slot-names &rest args &key (initform nil initform-p) &allow-other-keys)
     (apply #'call-next-method computed-slot-definition slot-names
            (append (when initform-p
                      (list :initform initform
                            :initfunction (compile nil `(lambda () ,initform))))
             args)))

(def method direct-slot-definition-class ((class computed-class) &rest slot-initargs &key &allow-other-keys)
  (find-class (if (eq :class (getf slot-initargs :allocation))
                  'standard-direct-slot-definition
                  'computed-direct-slot-definition)))

(def method effective-slot-definition-class ((class computed-class) &key &allow-other-keys)
  (declare (special %effective-slot-definition-class%))
  (aif %effective-slot-definition-class%
       (find-class it)
       (call-next-method)))

(def method compute-effective-slot-definition ((class computed-class) name direct-slot-definitions)
  (declare (type list direct-slot-definitions))
  (let ((%effective-slot-definition-class%
         (when (find-if (lambda (direct-slot-definition)
                          (typep direct-slot-definition 'computed-direct-slot-definition))
                        direct-slot-definitions)
           'computed-effective-slot-definition)))
    (declare (special %effective-slot-definition-class%))
    (call-next-method)))

(def macro slot-value-using-class-body (object slot)
  (declare (type (or symbol effective-slot-definition) slot))
  `(let ((slot-value (standard-instance-access-form ,object ,slot)))
     (when (unbound-slot-marker? slot-value)
       (error 'unbound-slot
              :name ,(if (symbolp slot)
                         `(slot-definition-name ,slot)
                         `(quote ,(slot-definition-name slot)))
              :instance ,object))
     (if (computed-state-p slot-value)
         (computed-state-value slot-value)
         slot-value)))

(def macro setf-slot-value-using-class-body (new-value object slot)
  (declare (type (or symbol effective-slot-definition) slot))
  `(let ((slot-value (standard-instance-access-form ,object ,slot)))
     ;; an equivalent cond is compiled into considerably slower code on sbcl (?!).
     (if (computed-state-p ,new-value)
         (progn
           (if (computed-state-p slot-value)
               (copy-place-independent-slots-of-computed-state ,new-value slot-value)
               (progn
                 (setf slot-value new-value)
                 (setf (cs-object ,new-value) ,object)
                 (debug-only
                   (setf (cs-place-descriptor ,new-value) ,slot))
                 (setf-standard-instance-access-form slot-value ,object ,slot)))
           slot-value)
         (if (computed-state-p slot-value)
             (setf (computed-state-value slot-value) ,new-value)
             (setf-standard-instance-access-form (make-computed-state :recomputation-mode :on-demand :compute-as nil :object ,object :valid #t :value ,new-value
                                                                      #+debug :form #+debug ,new-value #+debug :place-descriptor #+debug ,slot)
                                                 ,object ,slot)))))

(def method slot-value-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (slot-value-using-class-body object slot))

(def method (setf slot-value-using-class) (new-value (class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-slot-value-using-class-body new-value object slot))

(def method slot-boundp-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (not (unbound-slot-marker? (standard-instance-access-form object slot))))

(def method slot-makunbound-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-standard-instance-access-form (unbound-slot-marker) object slot)
  object)

;;;;;;
;;; Computed state

(def type recomputation-mode ()
  `(member :always :on-demand))

(def structure (computed-state (:conc-name cs-) (:print-object print-computed-state))
  (valid
   #f
   :type boolean)
  (depends-on-me
   (make-array 0 :adjustable #t :fill-pointer 0)
   :type (or null vector))
  ;; direct values have nil compute-as
  (compute-as
   nil
   :type (or null symbol function))
  (recomputation-mode
   :on-demand
   :type recomputation-mode)
  ;; these two are used for object slots
  (object
   nil
   :type (or null computed-object))
  (value
   nil
   :type t)
  #+debug
  (place-descriptor ; contains the name of the computed variable, or the slot definition
   nil
   :type (or null symbol computed-effective-slot-definition))
  #+debug
  (form
   nil
   :type (or atom list))
  #+debug
  (debug-invalidation
   #f
   :type boolean))

(define-dynamic-context recompute-state-contex
  ((computed-state nil
    :type computed-state))
  :chain-parents #t
  :create-struct #t
  :struct-options ((:conc-name rsc-)))

(def (function io) copy-place-independent-slots-of-computed-state (from into)
  "Copy the slots of FROM into INTO that are not dependent on the place this computed slot has been assigned to."
  (declare (type computed-state from into))
  (macrolet ((x (&rest names)
               ;; due to inlining it may get expanded in a different package than *package*, so bind it explicitly
               (bind ((*package* (find-package :projectured)))
                 `(progn
                    ,@(loop for name :in names
                            for accessor = (symbolicate '#:cs- name)
                            collect `(setf (,accessor into) (,accessor from)))))))
    (x valid
       depends-on-me
       compute-as
       #+debug form
       value)
    (values)))

(def (function o) invalidate-computed-slot (object slot)
  (invalidate-computed-state (standard-instance-access-form object (find-slot (class-of object) slot))))

(def (function o) purge-computed-state-depends-on-me (computed-state)
  (bind ((original-depends-on-me (cs-depends-on-me computed-state)))
    (when (find-if-not #'sb-ext:weak-pointer-value original-depends-on-me)
      (bind ((purged-depends-on-me (make-array 0 :adjustable #t :fill-pointer 0)))
        (loop for element :across (cs-depends-on-me computed-state) :do
              (when (sb-ext:weak-pointer-value element)
                (vector-push-extend element purged-depends-on-me)))
        ;;(format t "~%Purged from ~A to ~A" (length (cs-depends-on-me computed-state)) (length depends-on-me))
        (setf (cs-depends-on-me computed-state) purged-depends-on-me)))))

(def (function io) computed-state-value (computed-state)
  "Read the value, recalculate when needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex?)
    (bind ((computed-state-being-recomputed (rsc-computed-state *recompute-state-contex*)))
      (unless (find computed-state-being-recomputed (cs-depends-on-me computed-state) :key #'sb-ext:weak-pointer-value)
        (vector-push-extend (sb-ext:make-weak-pointer computed-state-being-recomputed) (cs-depends-on-me computed-state))
        (purge-computed-state-depends-on-me computed-state))))
  (ensure-computed-state-is-valid computed-state)
  (cs-value computed-state))

(def (function io) (setf computed-state-value) (new-value computed-state)
  "Set the value, invalidate and recalculate as needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (invalidate-computed-state computed-state)
  (setf (cs-valid computed-state) #t)
  (setf (cs-compute-as computed-state) nil)
  (setf (cs-value computed-state) new-value))

(def (function io) ensure-computed-state-is-valid (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (unless (computed-state-valid-p computed-state)
    #+nil(log.dribble "About to recompute ~A" computed-state)
    #+debug
    (check-circularity computed-state)
    (recompute-computed-state computed-state))
  (values))

(def (function o) recompute-computed-state (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (with-new-recompute-state-contex (:computed-state computed-state)
    #+debug
    (when *profile-context*
      (incf (recomputation-count-of *profile-context*)))
    #+nil(log.dribble "Recomputing slot ~A" computed-state)
    (bind ((old-value (cs-value computed-state))
           (new-value (aif (cs-compute-as computed-state)
                           (funcall it (cs-object computed-state) old-value)
                           (cs-value computed-state)))
           (store-new-value-p (if (and (primitive-p old-value)
                                       (primitive-p new-value))
                                  (not (equal old-value new-value))
                                  #t)))
      (when (or store-new-value-p
                (not (cs-valid computed-state)))
        (setf (cs-valid computed-state) #t))
      (if store-new-value-p
          (setf (cs-value computed-state) new-value)
          #+nil(log.dribble "Not storing fresh recomputed value for ~A because it was equal to the cached value." computed-state))
      new-value)))

(def (function io) primitive-p (object)
  (or (numberp object)
      (stringp object)
      (symbolp object)
      (characterp object)))

(def (function io) computed-state-valid-p (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  #+debug
  (when *profile-context*
    (incf (validation-count-of *profile-context*)))
  (and (not (eq :always (cs-recomputation-mode computed-state)))
       (cs-valid computed-state)))

#+debug
(def (function o) check-circularity (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex?)
    (bind ((context *recompute-state-contex*))
      (loop for parent-context = context :then (rsc-parent-context parent-context)
            while parent-context
            do (let ((parent-computed-state (rsc-computed-state parent-context)))
                 (when (eq computed-state parent-computed-state)
                   (error "Circularity detected among the computed slots/variables ~A"
                          (append (list computed-state)
                                  (loop for parent-context = context :then (rsc-parent-context parent-context)
                                        while parent-context
                                        collect (rsc-computed-state parent-context)))))))))
  (values))

(def (function o) invalidate-computed-state (computed-state)
  (declare (type computed-state computed-state))
  (labels ((recurse (instance)
             #+debug
             (when (cs-debug-invalidation instance)
               (break "Computed state ~A is being invalidated" instance))
             (setf (cs-valid instance) #f)
             (loop for element :across (cs-depends-on-me instance) :do
                   (awhen (sb-ext:weak-pointer-value element)
                     (recurse it)))
             #+debug
             (assert (not (has-recompute-state-contex?)))
             (adjust-array (cs-depends-on-me instance) 0 :fill-pointer 0)))
    (recurse computed-state))
  (values))

(def function print-computed-state (computed-state stream)
  (declare (type computed-state computed-state))
  #*((:debug
      (bind ((name (cs-place-descriptor computed-state)))
        (typecase name
          (computed-effective-slot-definition
           (setf name (slot-definition-name name))))
        (format stream "~A / " (cs-object computed-state))
        (format stream "<#~A ~A :value ~A>"
                name (if (cs-valid computed-state) "valid" "invalid") (cs-value computed-state))))
     (t
      (format stream "~A / " (cs-object computed-state))
      (format stream "<#computed-state ~A :value ~A>"
              (if (cs-valid computed-state) "valid" "invalid") (cs-value computed-state)))))

(def macro as* ((&key (recomputation-mode ':on-demand) #+debug (debug-invalidation #f)) &body form)
  (bind ((self-variable-name '-self-))
    `(make-computed-state :recomputation-mode ',recomputation-mode #+debug :form #+debug ',form #+debug :debug-invalidation #+debug ,debug-invalidation
                          :compute-as (named-lambda as/compute-as*/a-computation-body
                                          (,self-variable-name -current-value-)
                                        (declare (ignorable ,self-variable-name -current-value-))
                                        ,@form))))

(def macro as (&body body)
  `(as* ,nil ,@body))

(def macro asd (&body body)
  `(as* (:debug-invalidation #t) ,@body))

(def macro va (computed-state)
  `(computed-state-value ,computed-state))

;;;;;;
;;; Profiling

(def special-variable *profile-context* nil)

(def class* profile-context ()
  ((validation-count 0 :type integer)
   (recomputation-count 0 :type integer)))

(def macro with-profiling-computation (&body forms)
  (with-unique-names (validation-count recomputation-count)
    `(bind ((*profile-context* (make-instance 'profile-context))
            (,validation-count (validation-count-of *profile-context*))
            (,recomputation-count (recomputation-count-of *profile-context*)))
       (values (progn ,@forms)
               (- (validation-count-of *profile-context*) ,validation-count)
               (- (recomputation-count-of *profile-context*) ,recomputation-count)))))

;;;;;;
;;; Computed sequence

(def class* computed-sequence (sequence)
  ((elements :type sequence))
  (:metaclass computed-class))

(def function make-computed-sequence (&rest elements)
  (make-instance 'computed-sequence :elements elements))

#+nil
(def method sb-sequence:emptyp ((sequence computed-sequence))
  (emptyp (elements-of sequence)))

(def method sb-sequence:length ((sequence computed-sequence))
  (length (elements-of sequence)))

(def method sb-sequence:elt ((sequence computed-sequence) index)
  (bind ((element (elt (elements-of sequence) index)))
    (if (computed-state-p element)
        (computed-state-value element)
        element)))

(def method (setf sb-sequence:elt) (new-value (sequence computed-sequence) index)
  (bind ((element (elt (elements-of sequence) index)))
    (if (computed-state-p element)
        (setf (computed-state-value element) new-value)
        (setf (elt (elements-of sequence) index) new-value))))

(def method sb-sequence:adjust-sequence ((sequence computed-sequence) length &key (initial-element nil initial-element?) initial-contents)
  (setf (elements-of sequence) (if initial-element?
                                   (make-array (list length) :initial-element initial-element)
                                   (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

(def method sb-sequence:subseq ((sequence computed-sequence) start &optional end)
  (make-instance (class-of sequence) :elements (subseq (elements-of sequence) start end)))

(def method sb-sequence:replace ((sequence-1 computed-sequence) (sequence-2 computed-sequence) &rest args &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (apply #'replace (elements-of sequence-1) (elements-of sequence-2) args)
  sequence-1)

(def method sb-sequence:make-sequence-like ((sequence computed-sequence) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance (class-of sequence) :elements (if initial-element?
                                                   (make-array (list length) :initial-element initial-element)
                                                   (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

;;;;;;
;;; Computed linked list

(def class* computed-ll (sequence)
  ((value :type t)
   (previous-element :type computed-ll)
   (next-element :type computed-ll))
  (:metaclass computed-class))

(def function make-computed-ll (value previous-element next-element)
  (make-instance 'computed-ll :value value :previous-element previous-element :next-element next-element))

(def function elt-ll (sequence index)
  (cond ((zerop index)
         sequence)
        ((> index 0)
         (elt-ll (next-element-of sequence) (1- index)))
        (t
         (elt-ll (previous-element-of sequence) (1+ index)))))

(def method sb-sequence:length ((sequence computed-ll))
  (bind ((length 0))
    (iter (for element :initially sequence :then (previous-element-of element))
          (while element)
          (incf length))
    (iter (for element :initially sequence :then (next-element-of element))
          (while element)
          (incf length))
    (1- length)))

(def method sb-sequence:elt ((sequence computed-ll) index)
  (bind ((first-element (iter (for element :initially sequence :then (previous-element-of element))
                              (for previous-element :previous element)
                              (while element)
                              (finally (return previous-element)))))
    (iter (for element :initially first-element :then (next-element-of element))
          (until (zerop index))
          (decf index)
          (finally (return (value-of element))))))

(def method (setf sb-sequence:elt) (new-value (sequence computed-ll) index)
  (bind ((first-element (iter (for element :initially sequence :then (previous-element-of element))
                              (for previous-element :previous element)
                              (while element)
                              (finally (return previous-element)))))
    (iter (for element :initially first-element :then (next-element-of element))
          (until (zerop index))
          (decf index)
          (finally (return (setf (value-of element) new-value))))))

(def special-variable *check-ll* nil)

(def function ll (sequence &optional (index 0))
  (if (typep sequence 'computed-ll)
      sequence
      (unless (emptyp sequence)
        (labels ((make (index previous next)
                   (make-computed-ll (as (elt sequence index))
                                     (as (when (> index 0)
                                           (or previous (make (1- index) nil -self-))))
                                     (as (when (< index (1- (length sequence)))
                                           (or next (make (1+ index) -self- nil)))))))
          (elt-ll (make 0 nil nil) index)))))

(def function make-ll (length &key (initial-element nil initial-element?) initial-contents)
  (if initial-element?
      (ll (make-list length :initial-element initial-element))
      (list-ll initial-contents)))

(def function list-ll (&rest args)
  (ll args))

(def method sb-sequence:make-sequence-like ((sequence computed-ll) length &key (initial-element nil initial-element?) initial-contents)
  (ll (if initial-element?
          (make-list length :initial-element initial-element)
          (concatenate 'list initial-contents (make-list (- length (length initial-contents)))))))

(def function last-element (sequence)
  (iter (for e :initially sequence :then (next-element-of e))
        (for previous-e :previous e)
        (while e)
        (finally (return previous-e))))

(def function first-element (sequence)
  (iter (for e :initially sequence :then (previous-element-of e))
        (for previous-e :previous e)
        (while e)
        (finally (return previous-e))))

#+debug
(def function check-ll (sequence)
  (when *check-ll*
    (assert (eq (first-element sequence) (first-element (last-element sequence))))
    (assert (eq (last-element (first-element sequence)) (last-element sequence)))
    (assert (equal (iter (for element :initially (first-element sequence) :then (next-element-of element))
                         (while element)
                         (collect element))
                   (reverse (iter (for element :initially (last-element sequence) :then (previous-element-of element))
                                  (while element)
                                  (collect element)))))))

(def function map-ll (sequence function)
  #+debug
  (check-ll sequence)
  (when sequence
    (labels ((recurse (element previous-element next-element)
               (make-computed-ll (as (funcall function (value-of element)))
                                 (as (or previous-element
                                         (awhen (previous-element-of element)
                                           (recurse it nil -self-))))
                                 (as (or next-element
                                         (awhen (next-element-of element)
                                           (recurse it -self- nil)))))))
      (recurse sequence nil nil))))

(def function map-ll* (sequence function)
  #+debug
  (check-ll sequence)
  (when sequence
    (labels ((recurse (element previous-element next-element index)
               (make-computed-ll (as (funcall function element index))
                                 (as (or previous-element
                                         (awhen (previous-element-of element)
                                           (recurse it nil -self- (1- index)))))
                                 (as (or next-element
                                         (awhen (next-element-of element)
                                           (recurse it -self- nil (1+ index))))))))
      (recurse sequence nil nil 0))))


(def function subseq-ll (sequence start end)
  #+debug
  (check-ll sequence)
  (if (zerop start)
      (unless (zerop end)
        (make-computed-ll (as (value-of sequence))
                          (as (not-yet-implemented))
                          (as (subseq-ll (next-element-of sequence) 0 (1- end)))))
      (subseq-ll (next-element-of sequence) (1- start) (1- end))))

(def function concatenate-ll (index &rest sequences)
  (append-ll (ll (mapcar 'll (remove-if 'null sequences)) index)))

(def function append-ll (sequences)
  (when sequences
    (labels ((recurse (sequence sequence-element previous-sequence-element next-sequence-element previous-element next-element)
               #+debug
               (check-ll sequence)
               (when sequence
                 (make-computed-ll (as (value-of sequence))
                                   (as (or previous-element
                                           (aif (previous-element-of sequence)
                                                (recurse it sequence-element previous-sequence-element next-sequence-element nil -self-)
                                                (when previous-sequence-element
                                                  (recurse (last-element (value-of previous-sequence-element)) previous-sequence-element
                                                           (previous-element-of previous-sequence-element) sequence-element
                                                           nil -self-)))))
                                   (as (or next-element
                                           (aif (next-element-of sequence)
                                                (recurse it sequence-element previous-sequence-element next-sequence-element -self- nil)
                                                (when next-sequence-element
                                                  (recurse (first-element (value-of next-sequence-element)) next-sequence-element
                                                           sequence-element (next-element-of next-sequence-element)
                                                           -self- nil)))))))))
      (recurse (value-of sequences) sequences (previous-element-of sequences) (next-element-of sequences) nil nil))))

(def function separate-elements-ll (sequence separator)
  #+debug
  (check-ll sequence)
  (when sequence
    (labels ((recurse (sequence previous-element next-element)
               (make-computed-ll (as (value-of sequence))
                                 (as (when (previous-element-of sequence)
                                       (make-computed-ll (as separator)
                                                         (as (recurse (previous-element-of sequence) nil -self-))
                                                         (as next-element))))
                                 (as (when (next-element-of sequence)
                                       (make-computed-ll (as separator)
                                                         (as previous-element)
                                                         (as (recurse (next-element-of sequence) -self- nil))))))))
      (recurse sequence nil nil))))
