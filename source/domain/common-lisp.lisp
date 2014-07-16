;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document common-lisp/base ()
  ())

(def document common-lisp/constant (common-lisp/base)
  ((value :type t)))

(def document common-lisp/variable-reference (common-lisp/base)
  ((variable :type common-lisp/base)))

(def document common-lisp/function-reference (common-lisp/base)
  ((function :type common-lisp/base)))

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
   (value :type common-lisp/base)))

(def document common-lisp/let (common-lisp/base)
  ((bindings :type sequence)
   (body :type sequence)))

(def document common-lisp/application (common-lisp/base)
  ((operator :type symbol)
   (arguments :type sequence)))

(def document common-lisp/special-variable-definition (common-lisp/base)
  ((name :type lisp-form/symbol)
   (value :type common-lisp/base)))

(def document common-lisp/function-definition (common-lisp/base)
  ((name :type lisp-form/symbol)
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

(def (function e) make-common-lisp/constant (value &key selection)
  (make-instance 'common-lisp/constant :value value :selection selection))

(def (function e) make-common-lisp/variable-reference (variable &key selection)
  (make-instance 'common-lisp/variable-reference :variable variable :selection selection))

(def (function e) make-common-lisp/function-reference (function &key selection)
  (make-instance 'common-lisp/function-reference :function function :selection selection))

(def (function e) make-common-lisp/if (condition then else &key selection)
  (make-instance 'common-lisp/if :condition condition :then then :else else :selection selection))

(def (function e) make-common-lisp/progn (body &key selection)
  (make-instance 'common-lisp/progn :body body :selection selection))

(def (function e) make-common-lisp/lexical-variable-binding (name value &key selection)
  (make-instance 'common-lisp/lexical-variable-binding :name name :value value :selection selection))

(def (function e) make-common-lisp/let (bindings body &key selection)
  (make-instance 'common-lisp/let :bindings bindings :body body :selection selection))

(def (function e) make-common-lisp/required-function-argument (name &key selection)
  (make-instance 'common-lisp/required-function-argument :name name :selection selection))

(def (function e) make-common-lisp/special-variable-definition (name value &key selection)
  (make-instance 'common-lisp/special-variable-definition
                 :name name
                 :value value
                 :selection selection))

(def (function e) make-common-lisp/function-definition (name bindings body &key allow-other-keys documentation selection)
  (make-instance 'common-lisp/function-definition
                 :name name
                 :bindings bindings
                 :allow-other-keys allow-other-keys
                 :documentation documentation
                 :body body
                 :selection selection))

(def (function e) make-common-lisp/lambda-function (bindings body &key allow-other-keys selection)
  (make-instance 'common-lisp/lambda-function
                 :bindings bindings
                 :allow-other-keys allow-other-keys
                 :body body
                 :selection selection))

(def (function e) make-common-lisp/application (operator arguments &key selection)
  (make-instance 'common-lisp/application :operator operator :arguments arguments :selection selection))

(def (function e) make-common-lisp/top-level (body)
  (make-instance 'common-lisp/top-level :body body))
