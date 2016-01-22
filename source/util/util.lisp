;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Util

(def function resource-pathname (name)
  (flet ((try (path)
           (awhen (probe-file path)
             (return-from resource-pathname it))))
    (try (asdf:system-relative-pathname :projectured.executable name))
    (awhen (uiop:argv0)
      (try (uiop:merge-pathnames* (directory-namestring it) name)))))

(def function find-slot-reader (class slot)
  (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class))))
    (first (slot-definition-readers direct-slot))))

(def function write-number (object &optional (stream *standard-output*))
  ;; TODO: format stream doesn't work with hu.dwim.web-server network streams for some reason
  (if (eq stream *standard-output*)
      (write-string (format nil "~A" (or object "")) stream)
      (format stream "~A" (or object ""))))

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

(def macro completion-prefix-merge (&body cases)
  (bind ((result-vars (iter (repeat (length cases))
                            (collect (gensym "RESULT"))))
         (completion-vars (iter (repeat (length cases))
                                (collect (gensym "COMPLETION")))))
    `(bind (,@(iter (for case :in cases)
                    (for result-var :in result-vars)
                    (for completion-var :in completion-vars)
                    (collect `((:values ,result-var ,completion-var) ,case))))
       (or ,@result-vars
           (values nil
                   (bind ((matching-prefixes (remove-if 'null (list ,@completion-vars))))
                     (reduce 'longest-common-prefix matching-prefixes :initial-value (first matching-prefixes))))))))

(def function deep-copy (instance)
  (labels ((recurse (instance)
             (etypecase instance
               ((or number string symbol pathname function #+sbcl sb-sys:system-area-pointer)
                instance)
               (style/base
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
