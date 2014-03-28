;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Javascript document classes

(def document javascript/base ()
  ())

;;;;;;
;;; Javascript statement classes

(def document javascript/statement (javascript/base)
  ())

(def document javascript/statement/block (javascript/statement)
  ((elements :type sequence)))

(def document javascript/statement/top-level (javascript/statement)
  ((elements :type sequence)))

;;;;;;
;;; Javascript expression classes

(def document javascript/expression (javascript/base)
  ())

(def document javascript/expression/variable-reference (javascript/expression)
  ((name :type string)))

(def document javascript/expression/property-access (javascript/expression)
  ((object :type javascript/expression)
   (property :type string)))

(def document javascript/expression/constructor-invocation (javascript/expression)
  ((object :type javascript/expression)
   (arguments :type sequence)))

(def document javascript/expression/method-invocation (javascript/expression)
  ((object :type javascript/expression)
   (method :type javascript/definition/method)
   (arguments :type sequence)))

;;;;;;
;;; Javascript literal classes

(def document javascript/literal (javascript/expression)
  ())

(def document javascript/literal/null (javascript/literal)
  ())

(def document javascript/literal/undefined (javascript/literal)
  ())

(def document javascript/literal/boolean (javascript/literal)
  ((value :type (member #t #f))))

(def document javascript/literal/number (javascript/literal)
  ((value :type number)))

(def document javascript/literal/string (javascript/literal)
  ((value :type string)))

;;;;;;
;;; Javascript definition classes

(def document javascript/definition (javascript/base)
  ())

(def document javascript/definition/variable (javascript/definition)
  ((name :type string)
   (body :type javascript/base)))

(def document javascript/definition/function (javascript/definition)
  ((name :type string)
   (arguments :type sequence)
   (body :type javascript/base)))

;;;;;;
;;; Javascript statement constructors

(def (function e) make-javascript/statement/block (elements)
  (make-instance 'javascript/statement/block :elements elements))

(def (function e) make-javascript/statement/top-level (elements)
  (make-instance 'javascript/statement/top-level :elements elements))

;;;;;;
;;; Javascript expression constructors

(def (function e) make-javascript/expression/variable-reference (name)
  (make-instance 'javascript/expression/variable-reference :name name))

(def (function e) make-javascript/expression/property-access (object property)
  (make-instance 'javascript/expression/property-access :object object :property property))

(def (function e) make-javascript/expression/constuctor-invocation (object arguments)
  (make-instance 'javascript/expression/constructor-invocation :object object :arguments arguments))

(def (function e) make-javascript/expression/method-invocation (object method arguments)
  (make-instance 'javascript/expression/method-invocation :object object :method method :arguments arguments))

;;;;;;
;;; Javascript literal constructors

(def (function e) make-javascript/literal/boolean (value)
  (make-instance 'javascript/literal/boolean :value value))

(def (function e) make-javascript/literal/number (value)
  (make-instance 'javascript/literal/number :value value))

(def (function e) make-javascript/literal/string (value)
  (make-instance 'javascript/literal/string :value value))

;;;;;;
;;; Javascript definition constructors

(def (function e) make-javascript/definition/variable (name body)
  (make-instance 'javascript/definition/variable
                 :name name
                 :body body))

(def (function e) make-javascript/definition/function (name arguments body)
  (make-instance 'javascript/definition/function
                 :name name
                 :arguments arguments
                 :body body))
