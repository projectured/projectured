;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document evaluator/base ()
  ())

(def document evaluator/toplevel (evaluator/base)
  ((elements :type sequence)))

(def document evaluator/form (evaluator/base)
  ((form :type document)
   (result :type document)))

;;;;;;
;;; Construction

(def function make-evaluator/toplevel (elements &key selection)
  (make-instance 'evaluator/toplevel :elements elements :selection selection))

(def function make-evaluator/form (form &key result selection)
  (make-instance 'evaluator/form :form form :result result :selection selection))

;;;;;;
;;; Construction

(def macro evaluator/toplevel ((&key selection) &body elements)
  `(make-evaluator/toplevel (document/sequence (:selection (rest ,selection)) ,@elements) :selection ,selection))

(def macro evaluator/form ((&key selection) &body body)
  `(make-evaluator/form ,(first body) :result ,(second body) :selection ,selection))

;;;;;;
;;; API

(def class* evaluator/completion ()
  ((toplevel :type evaluator/toplevel))
  (:metaclass funcallable-standard-class))

(def method initialize-instance :after ((instance evaluator/completion) &key &allow-other-keys)
  (set-funcallable-instance-function
   instance
   (lambda (factory printer-input reader-input name)
     (bind ((toplevel (toplevel-of factory)))
       (completion-prefix-merge
         (completion-prefix-switch* name
                                    (iter (for element :in-sequence (elements-of toplevel))
                                          (for form = (form-of element))
                                          (when (typep form 'common-lisp/function-definition)
                                            (for name = (string-downcase (name-of (name-of form))))
                                            (collect (cons name
                                                           (bind ((name-length (length name)))
                                                             (make-common-lisp/application (make-common-lisp/function-reference form :selection `((the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
                                                                                                                                                  (the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
                                                                                                                                                  (the string (name-of (the lisp-form/symbol document)))
                                                                                                                                                  (the string (subseq (the string document) ,name-length ,name-length))))
                                                                                           nil
                                                                                           :factory factory
                                                                                           :selection `((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
                                                                                                        (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
                                                                                                        (the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
                                                                                                        (the string (name-of (the lisp-form/symbol document)))
                                                                                                        (the string (subseq (the string document) ,name-length ,name-length))))))))))
         (common-lisp/complete-document factory printer-input reader-input name))))))
