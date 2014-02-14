;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document common-lisp/base (selection/base)
  ((indentation :type integer)))

(def document common-lisp/constant (common-lisp/base)
  ((value :type t)))

(def document common-lisp/variable-reference (common-lisp/base)
  ((name :type symbol)))

(def document common-lisp/if (common-lisp/base)
  ((condition :type common-lisp/base)
   (then :type common-lisp/base)
   (else :type common-lisp/base)))

(def document common-lisp/progn (common-lisp/base)
  ((body :type sequence)))

(def document common-lisp/the (common-lisp/base)
  ((declared-type :type t)
   (value :type common-lisp/base)))

(def document common-lisp/lexical-variable-binding (common-lisp/base)
  ((name :type t)
   (initial-value :type common-lisp/base)))

(def document common-lisp/let (common-lisp/base)
  ((bindings :type sequence)
   (body :type sequence)))

(def document common-lisp/application (common-lisp/base)
  ((operator :type symbol)
   (arguments :type sequence)))

(def document common-lisp/function-definition (common-lisp/base)
  ((name :type symbol)
   (bindings :type sequence)
   (allow-other-keys :type boolean)
   (documentation :type string)
   (body :type sequence)))

(def document common-lisp/lambda-function (common-lisp/base)
  ((bindings :type sequence)
   (allow-other-keys :type boolean)
   (body :type sequence)))

(def document common-lisp/function-argument (common-lisp/base)
  ((name :type symbol)))

(def document common-lisp/required-function-argument (common-lisp/function-argument)
  ())

(def document common-lisp/optional-function-argument (common-lisp/function-argument)
  ())

(def document common-lisp/keyword-function-argument (common-lisp/function-argument)
  ())

(def document common-lisp/rest-function-argument (common-lisp/function-argument)
  ())

(def document common-lisp/auxiliary-function-argument (common-lisp/function-argument)
  ())

(def document common-lisp/comment (common-lisp/base)
  ((content :type string)))

(def document common-lisp/top-level (common-lisp/base)
  ((body :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-common-lisp/comment (content)
  (make-instance 'common-lisp/comment :content content))

(def (function e) make-common-lisp/top-level (body)
  (make-instance 'common-lisp/top-level :body body))
