;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Widget

(def (function e) make-projection/widget->graphics ()
  (type-dispatching
    (widget/tooltip (make-projection/widget/tooltip->graphics/canvas))
    (widget/composite (make-projection/widget/composite->graphics/canvas))
    (widget/scroll-pane (make-projection/widget/scroll-pane->graphics/canvas))))

(def (macro e) widget->graphics ()
  '(make-projection/widget->graphics))

;;;;;;
;;; Book

(def (function e) make-projection/book->tree ()
  (type-dispatching
    (book/book (make-projection/book/book->tree/node))
    (book/chapter (make-projection/book/chapter->tree/node))
    (string (make-projection/preserving))))

(def (macro e) book->tree ()
  '(make-projection/book->tree))

;;;;;;
;;; Tree

(def (function e) make-projection/tree->list ()
  (type-dispatching
    (string (make-projection/preserving))
    (tree/node (make-projection/tree->list))))

(def (macro e) tree->list ()
  '(make-projection/tree->list))

;;;;;;
;;; Table

(def (function e) make-projection/table->string ()
  (type-dispatching
    (table/table (make-projection/table/table->string))
    (string (preserving))))

(def (function e) make-projection/t->table ()
  (type-dispatching
    (null (make-projection/t/null->string))
    (number (make-projection/t/number->string))
    (string (make-projection/t/string->string))
    (symbol (make-projection/t/symbol->string))
    (sequence (make-projection/t/sequence->table/table))
    (hash-table (make-projection/t/hash-table->table/table))
    (function (make-projection/t/function->table/table))
    ((or structure-object standard-object) (make-projection/t/object->table))))

(def (macro e) table->string ()
  '(make-projection/table->string))

(def (macro e) t->table ()
  '(make-projection/t->table))

;;;;;;
;;; XML

(def (function e) make-projection/xml->tree ()
  (type-dispatching
    (xml/text (make-projection/xml/text->string))
    (xml/attribute (make-projection/xml/attribute->tree/node))
    (xml/element (make-projection/xml/element->tree/node))))

(def (macro e) xml->tree ()
  '(make-projection/xml->tree))

;;;;;;
;;; JSON

(def (function e) make-projection/json->tree ()
  (type-dispatching
    (json/null (make-projection/json/null->string))
    (json/boolean (make-projection/json/boolean->string))
    (json/number (make-projection/json/number->string))
    (json/string (make-projection/json/string->string))
    (json/array (make-projection/json/array->tree/node))
    (json/object (make-projection/json/object->tree/node))))

(def (macro e) json->tree ()
  '(make-projection/json->tree))

;;;;;;
;;; Java

(def (function e) make-projection/java->tree ()
  (type-dispatching
    (java/statement/block (make-projection/java/statement/block->tree/node))
    (java/statement/if (make-projection/java/statement/if->tree/node))
    (java/statement/return (make-projection/java/statement/return->tree/node))
    (java/expression/variable-reference (make-projection/java/expression/variable-reference->string))
    (java/expression/method-invocation (make-projection/java/expression/method-invocation->tree/node))
    (java/expression/infix-operator (make-projection/java/expression/infix-operator->tree/node))
    (java/literal/null (make-projection/java/literal/null->string))
    (java/literal/number (make-projection/java/literal/number->string))
    (java/literal/character (make-projection/java/literal/character->string))
    (java/literal/string (make-projection/java/literal/string->string))
    (java/declaration/method (make-projection/java/declaration/method->tree/node))
    (java/declaration/argument (make-projection/java/declaration/argument->tree/node))
    (java/declaration/qualifier (make-projection/java/declaration/qualifier->string))
    (java/declaration/type (make-projection/java/declaration/type->string))))

(def (macro e) java->tree ()
  '(make-projection/java->tree))

;;;;;;
;;; Lisp form

(def (function e) make-projection/lisp-form->tree ()
  (type-dispatching
    (lisp-form/number (make-projection/lisp-form/number->string))
    (lisp-form/symbol (make-projection/lisp-form/symbol->string))
    (lisp-form/string (make-projection/lisp-form/string->string))
    (lisp-form/list (make-projection/lisp-form/cons->tree/node))
    (lisp-form/object (make-projection/lisp-form/object->string))))

(def (macro e) lisp-form->tree ()
  '(make-projection/lisp-form->tree))

;;;;;;
;;; Walked lisp form

(def (function e) make-projection/walked-lisp-form->lisp-form ()
  (type-dispatching
    (hu.dwim.walker:constant-form (make-projection/walked-lisp-form/constant-form->lisp-form/string))
    (hu.dwim.walker:variable-reference-form (make-projection/walked-lisp-form/variable-reference-form->lisp-form/string))
    (hu.dwim.walker:if-form (make-projection/walked-lisp-form/if-form->lisp-form/list))
    (hu.dwim.walker:the-form (make-projection/walked-lisp-form/the-form->lisp-form/list))
    (hu.dwim.walker:application-form (make-projection/walked-lisp-form/application-form->lisp-form/list))
    (hu.dwim.walker:function-definition-form (make-projection/walked-lisp-form/function-definition-form->lisp-form/list))
    (hu.dwim.walker:lambda-function-form (make-projection/walked-lisp-form/lambda-function-form->lisp-form/list))
    (hu.dwim.walker:function-argument-form (make-projection/walked-lisp-form/function-argument-form->lisp-form/string))))

(def (macro e) walked-lisp-form->lisp-form ()
  '(make-projection/walked-lisp-form->lisp-form))
