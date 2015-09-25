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

(def function resource-pathname (name)
  (flet ((try (path)
           (awhen (probe-file path)
             (return-from resource-pathname it))))
    (try (asdf:system-relative-pathname :projectured name))
    (awhen (uiop:argv0)
      (try (uiop:merge-pathnames* (directory-namestring it) name)))))

(def function form-type (form)
  (typecase form
    (null 'sequence)
    (cons 'sequence)
    (number 'number)
    (string 'string)
    (vector 'vector)
    (computed-ll 'sequence)
    (document/sequence 'sequence)
    (document/string 'document/string)
    (sequence 'sequence)
    (t (type-of form))))

(def function object-class-name (object)
  (form-type object)
  #+nil
  (class-name (class-of object)))

(def function object-class-symbol-name (object)
  (symbol-name (object-class-name object)))

(def function find-slot-reader (class slot)
  (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class))))
    (first (slot-definition-readers direct-slot))))

(def function write-number (object &optional (stream *standard-output*))
  ;; TODO: format stream doesn't work with hu.dwim.web-server network streams for some reason
  (if (eq stream *standard-output*)
      (write-string (format nil "~A" (or object "")) stream)
      (format stream "~A" (or object ""))))

(def function reference/flatten (reference &optional (result 'document))
  (if (consp reference)
      (reference/flatten (rest reference) (tree-replace (car reference) 'document result))
      result))

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
                          (when (slot-boundp-using-class class instance slot)
                            (for slot-value = (slot-value-using-class class instance slot))
                            (for slot-reader = (find-slot-reader class slot))
                            (recurse slot-value `((,slot-reader (the ,(form-type instance) document))
                                                  ,@(typed-reference (form-type instance) reference))))))))))
      (recurse root nil))
    (nreverse result)))

(def function longest-common-prefix (string-1 string-2)
  (iter (for index :from 0)
        (while (and (< index (length string-1))
                    (< index (length string-2))
                    (string= (subseq string-1 0 (1+ index))
                             (subseq string-2 0 (1+ index)))))
        (finally (return (subseq string-1 0 index)))))

(def macro completion-prefix-switch (prefix &body cases)
  `(switch (,prefix :test 'string=)
     ,@cases
     (t (values nil
                (bind ((matching-prefixes (remove-if-not (curry 'starts-with-subseq ,prefix) (list ,@(mapcar 'first cases))))
                       (common-prefix (reduce 'longest-common-prefix matching-prefixes :initial-value (first matching-prefixes))))
                  (subseq common-prefix (min (length common-prefix) (length ,prefix))))))))

(def function deep-copy (instance)
  (labels ((recurse (instance)
             (etypecase instance
               ((or number string symbol pathname function #+sbcl sb-sys:system-area-pointer)
                instance)
               (document/sequence
                (make-document/sequence (iter (for element :in-sequence instance)
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

(def macro with-measuring ((name type) &body forms)
  `(case ,type
     (:profile
      (with-profiling ()
        ,@forms))
     (:measure
      (format t "MEASURING ~A~%" ,name)
      (time
       (bind (((:values result validation-count recomputation-count)
               (with-profiling-computation ,@forms)))
         (format t "~%Validation count: ~A, recomputation count: ~A" validation-count recomputation-count)
         result)))
     (t
      ,@forms)))

(def (function io) unbound-slot-marker ()
  #*((:sbcl
      ;; without LOAD-TIME-VALUE the compiler dies
      (load-time-value sb-pcl::+slot-unbound+))
     (:ccl
      (ccl::%slot-unbound-marker))
     (t
      #.(not-yet-implemented/crucial-api 'unbound-slot-marker?))))

(def (function io) unbound-slot-marker? (value)
  (eq value (unbound-slot-marker)))

;; TODO '(inline standard-instance-access (setf standard-instance-access))

(def macro standard-instance-access-form (object slot)
  `(standard-instance-access ,object
    ,(if (typep slot 'effective-slot-definition)
         (slot-definition-location slot)
         `(slot-definition-location ,slot))))

(def macro setf-standard-instance-access-form (new-value object slot)
  #*((:sbcl `(setf (sb-pcl::clos-slots-ref (sb-pcl::std-instance-slots ,object)
                                           ,(if (typep slot 'effective-slot-definition)
                                                (slot-definition-location slot)
                                                `(slot-definition-location ,slot)))
                   ,new-value))
     (t `(setf (standard-instance-access ,object
                                         ,(if (typep slot 'effective-slot-definition)
                                              (slot-definition-location slot)
                                              `(slot-definition-location ,slot)))
               ,new-value))))
