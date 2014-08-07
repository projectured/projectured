;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; XML

(def function make-projection/xml->common-lisp ()
  (type-dispatching
    (xml/text (make-projection/xml/text->common-lisp/application))
    (xml/attribute (make-projection/xml/attribute->common-lisp/progn))
    (xml/element (make-projection/xml/element->common-lisp/progn))))

(def macro xml->common-lisp ()
  '(make-projection/xml->common-lisp))

;;;;;;
;;; JSON

(def function make-projection/json->common-lisp ()
  (type-dispatching
    (json/null (make-projection/json/null->common-lisp/application))
    (json/boolean (make-projection/json/boolean->common-lisp/application))
    (json/number (make-projection/json/number->common-lisp/application))
    (json/string (make-projection/json/string->common-lisp/application))
    (json/array (make-projection/json/array->common-lisp/progn))
    (json/object-entry (make-projection/json/object-entry->common-lisp/progn))
    (json/object (make-projection/json/object->common-lisp/progn))))

(def macro json->common-lisp ()
  '(make-projection/json->common-lisp))

;;;;;;
;;; Javascript

(def function make-projection/javascript->common-lisp ()
  (type-dispatching
    (javascript/statement/block (make-projection/javascript/statement/block->common-lisp/progn))
    (javascript/statement/top-level (make-projection/javascript/statement/top-level->common-lisp/progn))
    (javascript/expression/variable-reference (make-projection/javascript/expression/variable-reference->common-lisp/application))
    (javascript/expression/property-access (make-projection/javascript/expression/property-access->common-lisp/progn))
    (javascript/expression/constructor-invocation (make-projection/javascript/expression/constructor-invocation->common-lisp/progn))
    (javascript/expression/method-invocation (make-projection/javascript/expression/method-invocation->common-lisp/progn))
    (javascript/literal/string (make-projection/javascript/literal/string->common-lisp/application))
    (javascript/definition/variable (make-projection/javascript/definition/variable->common-lisp/progn))
    (javascript/definition/function (make-projection/javascript/definition/function->common-lisp/progn))))

(def macro javascript->common-lisp ()
  '(make-projection/javascript->common-lisp))

;;;;;;
;;; CSS

(def function make-projection/css->common-lisp ()
  (type-dispatching
    (css/attribute (make-projection/css/attribute->common-lisp/application))
    (css/rule (make-projection/css/rule->common-lisp/progn))))

(def macro css->common-lisp ()
  '(make-projection/css->common-lisp))
;;;;;;
;;; T

(def function make-projection/t->common-lisp ()
  (type-dispatching
    ((or document/insertion tree/base table/base common-lisp/comment)
     (make-instance 'projection
                    :reader (constantly nil)
                    :printer (lambda (projection recursion input input-reference)
                               (make-iomap/object projection recursion input input-reference (make-common-lisp/progn nil)))))
    (xml/base (xml->common-lisp))
    (json/base (json->common-lisp))
    (javascript/base (javascript->common-lisp))
    (css/base (css->common-lisp))
    (test/check (make-projection/test/check->common-lisp/application))
    ((or lisp-form/base common-lisp/constant common-lisp/variable-reference common-lisp/function-reference) (preserving))
    ((or sequence common-lisp/base) (copying))))

(def function make-projection/common-lisp->form ()
  (sequential
    (recursive (make-projection/common-lisp->lisp-form))
    (recursive (make-projection/lisp-form->form))))

(def function make-projection/t->form ()
  (sequential
    (recursive (make-projection/t->common-lisp))
    (recursive (make-projection/common-lisp->form))))

(def macro t->common-lisp ()
  '(make-projection/t->common-lisp))
