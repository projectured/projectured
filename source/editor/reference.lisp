;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Type

(def type reference ()
  '(or number symbol string cons))

;;;;;;
;;; Evaluation

(def function eval-reference (document reference)
  "Returns the value of REFERENCE within DOCUMENT. Purely functional."
  (bind (#+sbcl(sb-ext::*evaluator-mode* :interpret))
    (eval `(bind ((document ',document))
             (declare (ignorable document))
             ,reference))))

(def function (setf eval-reference) (new-value document reference)
  "Sets the value of REFERENCE within DOCUMENT to NEW-VALUE. Has side effects on DOCUMENT."
  (bind (#+sbcl(sb-ext::*evaluator-mode* :interpret))
    (eval `(bind ((document ,document))
             (declare (ignorable document))
             (setf ,reference ',new-value)))))

;;;;;;
;;; API

(def function typed-reference (type reference)
  (assert (symbolp type))
  (assert (or (not (consp (first reference)))
              (not (eq 'the (first (first reference))))))
  (when reference
    `((the ,type ,(first reference)) ,@(rest reference))))

(def function flatten-reference (reference &optional (result 'document))
  (if (consp reference)
      (flatten-reference (rest reference) (tree-replace (car reference) 'document result))
      result))
