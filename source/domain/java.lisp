;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Java document classes

(def document java/base (selection/base)
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

(def document java/statement/variable-declaration (java/statement)
  ((name :type string)
   (type :type java/declaration/type :accessor nil)))

;;;;;;
;;; Java expression classes

(def document java/expression (java/base)
  ())

(def document java/expression/this (java/expression)
  ())

(def document java/expression/method-invocation (java/expression)
  ((method :type java/declaration/method)
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
;;; Java declaration classes

(def document java/declaration (java/base)
  ())

(def document java/declaration/qualifier (java/base)
  ((name :type string)))

(def document java/declaration/type (java/base)
  ((name :type string)))

(def document java/declaration/class (java/declaration)
  ((name :type string)
   (extends :type java/declaration/class)
   (fields :type sequence)
   (methods :type sequence)))

(def document java/declaration/interface (java/declaration)
  ((name :type string)
   (methods :type sequence)))

(def document java/declaration/argument (java/declaration)
  ((name :type string)
   (type :type java/declaration/type :accessor nil)))

(def document java/declaration/method (java/declaration)
  ((qualifier :type java/declaration/qualifier)
   (return-type :type java/declaration/type)
   (name :type string)
   (arguments :type sequence)
   (body :type java/base)))

;;;;;;
;;; Java statement constructors

(def (function e) make-java/statement/block (elements)
  (make-instance 'java/statement/block :elements elements))

(def (function e) make-java/statement/if (condition then &optional else)
  (make-instance 'java/statement/if :condition condition :then then :else else))

(def (function e) make-java/statement/return (value)
  (make-instance 'java/statement/return :value value))

;;;;;;
;;; Java expression constructors

(def (function e) make-java/expression/variable-reference (name)
  (make-instance 'java/expression/variable-reference :name name))

(def (function e) make-java/expression/method-invocation (method arguments)
  (make-instance 'java/expression/method-invocation :method method :arguments arguments))

(def (function e) make-java/expression/infix-operator (operator arguments)
  (make-instance 'java/expression/infix-operator :operator operator :arguments arguments))

;;;;;;
;;; Java literal constructors

(def (function e) make-java/literal/boolean (value)
  (make-instance 'java/literal/boolean :value value))

(def (function e) make-java/literal/number (value)
  (make-instance 'java/literal/number :value value))

(def (function e) make-java/literal/character (value)
  (make-instance 'java/literal/character :value value))

(def (function e) make-java/literal/string (value)
  (make-instance 'java/literal/string :value value))

;;;;;;
;;; Java declaration constructors

(def (function e) make-java/declaration/qualifier (name)
  (make-instance 'java/declaration/qualifier :name name))

(def (function e) make-java/declaration/argument (name type)
  (make-instance 'java/declaration/argument :name name :type type))

(def (function e) make-java/declaration/type (name)
  (make-instance 'java/declaration/type :name name))

(def (function e) make-java/declaration/method (qualifier return-type name arguments body)
  (make-instance 'java/declaration/method
                 :qualifier qualifier
                 :return-type return-type
                 :name name
                 :arguments arguments
                 :body body))
