;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace evaluator)

(def definer operation (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'operation) (member 'operation supers))
                     supers
                     (append supers '(operation)))))
    `(def class* ,name ,supers ,slots ,@options)))

(def definer evaluator (name &body forms)
  (bind ((function-name (format-symbol :projectured "EVALUATOR/~A" name)))
    `(progn
       (def function ,function-name (-document- -operation-)
         (declare (ignorable -document- -operation-))
         ,@forms)
       (setf (find-evaluator ',name) ',function-name))))

;;;;;;
;;; Class

(def operation operation ()
  ()
  (:documentation "Base class for operations."))

(def operation operation/quit ()
  ()
  (:documentation "An operation that quits the editor."))

(def operation operation/functional ()
  ((thunk :type function))
  (:documentation "An operation that calls a function."))

(def operation operation/compound ()
  ((elements :type sequence))
  (:documentation "A sequence of operations carried out in the order they appear in elements."))

;;;;;;
;;; Construction

(def function make-operation/quit ()
  (make-instance 'operation/quit))

(def function make-operation/functional (thunk)
  (make-instance 'operation/functional :thunk thunk))

(def function make-operation/compound (elements)
  (make-instance 'operation/compound :elements elements))

;;;;;;
;;; Evaluator

(def evaluator operation/quit ()
  (throw :quit-editor nil))

(def evaluator operation/functional ()
  (funcall (thunk-of -operation-)))

(def evaluator operation/compound ()
  (iter (for element :in-sequence (elements-of -operation-))
        (call-evaluator -document- element)))

;;;;;;
;;; API

(def function call-evaluator (document operation)
  (funcall (find-evaluator (class-name (class-of operation))) document operation))
