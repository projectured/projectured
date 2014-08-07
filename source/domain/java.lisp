;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Java document classes

(def document java/base ()
  ())

;;;;;;
;;; Java statement classes

(def document java/statement (java/base)
  ())

(def document java/statement/block (java/statement)
  ((elements :type sequence)))

(def document java/statement/if (java/statement)
  ((condition :type java/expression)
   (then :type java/statement)
   (else :type java/statement)))

(def document java/statement/return (java/statement)
  ((value :type java/base)))

(def document java/statement/for (java/statement)
  ())

(def document java/statement/while (java/statement)
  ())

(def document java/statement/break (java/statement)
  ())

(def document java/statement/continue (java/statement)
  ())

(def document java/statement/switch (java/statement)
  ())

(def document java/statement/try (java/statement)
  ())

(def document java/statement/throw (java/statement)
  ())

(def document java/statement/variable-definition (java/statement)
  ((name :type string)
   (type :type java/definition/type :accessor nil)))

;;;;;;
;;; Java expression classes

(def document java/expression (java/base)
  ())

(def document java/expression/this (java/expression)
  ())

(def document java/expression/method-invocation (java/expression)
  ((method :type java/definition/method)
   (arguments :type sequence)))

(def document java/expression/infix-operator (java/expression)
  ((operator :type string)
   (arguments :type sequence)))

(def document java/expression/variable-reference (java/expression)
  ((name :type string)))

;;;;;;
;;; Java literal classes

(def document java/literal (java/expression)
  ())

(def document java/literal/null (java/literal)
  ())

(def document java/literal/boolean (java/literal)
  ((value :type (member #t #f))))

(def document java/literal/number (java/literal)
  ((value :type number)))

(def document java/literal/character (java/literal)
  ((value :type character)))

(def document java/literal/string (java/literal)
  ((value :type string)))

;;;;;;
;;; Java definition classes

(def document java/definition (java/base)
  ())

(def document java/definition/qualifier (java/base)
  ((name :type string)))

(def document java/definition/type (java/base)
  ((name :type string)))

(def document java/definition/class (java/definition)
  ((name :type string)
   (extends :type java/definition/class)
   (fields :type sequence)
   (methods :type sequence)))

(def document java/definition/interface (java/definition)
  ((name :type string)
   (methods :type sequence)))

(def document java/definition/argument (java/definition)
  ((name :type string)
   (type :type java/definition/type :accessor nil)))

(def document java/definition/method (java/definition)
  ((qualifier :type java/definition/qualifier)
   (return-type :type java/definition/type)
   (name :type string)
   (arguments :type sequence)
   (body :type java/base)))

;;;;;;
;;; Java statement constructors

(def function make-java/statement/block (elements)
  (make-instance 'java/statement/block :elements elements))

(def function make-java/statement/if (condition then &optional else)
  (make-instance 'java/statement/if :condition condition :then then :else else))

(def function make-java/statement/return (value)
  (make-instance 'java/statement/return :value value))

;;;;;;
;;; Java expression constructors

(def function make-java/expression/variable-reference (name)
  (make-instance 'java/expression/variable-reference :name name))

(def function make-java/expression/method-invocation (method arguments)
  (make-instance 'java/expression/method-invocation :method method :arguments arguments))

(def function make-java/expression/infix-operator (operator arguments)
  (make-instance 'java/expression/infix-operator :operator operator :arguments arguments))

;;;;;;
;;; Java literal constructors

(def function make-java/literal/boolean (value)
  (make-instance 'java/literal/boolean :value value))

(def function make-java/literal/number (value)
  (make-instance 'java/literal/number :value value))

(def function make-java/literal/character (value)
  (make-instance 'java/literal/character :value value))

(def function make-java/literal/string (value)
  (make-instance 'java/literal/string :value value))

;;;;;;
;;; Java definition constructors

(def function make-java/definition/qualifier (name)
  (make-instance 'java/definition/qualifier :name name))

(def function make-java/definition/argument (name type)
  (make-instance 'java/definition/argument :name name :type type))

(def function make-java/definition/type (name)
  (make-instance 'java/definition/type :name name))

(def function make-java/definition/method (qualifier return-type name arguments body)
  (make-instance 'java/definition/method
                 :qualifier qualifier
                 :return-type return-type
                 :name name
                 :arguments arguments
                 :body body))
