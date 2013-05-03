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
;;; Styled string

(def (function e) make-projection/styled-string->tree ()
  (type-dispatching
    (text/text (make-projection/text/text->tree/node))
    (text/string (make-projection/text/string->tree/leaf))))

(def (macro e) styled-string->tree ()
  '(make-projection/styled-string->tree))

;;;;;;
;;; Book

(def (function e) make-projection/book->tree ()
  (type-dispatching
    (book/book (make-projection/book/book->tree/node))
    (book/chapter (make-projection/book/chapter->tree/node))
    (string (make-projection/string->tree/leaf))))

(def (macro e) book->tree ()
  '(make-projection/book->tree))

;;;;;;
;;; Text

(def (function e) make-projection/text->tree ()
  (type-dispatching
    (string (make-projection/string->tree/leaf))
    (text/string (make-projection/text/string->tree/leaf))
    (text/text (make-projection/text/text->tree/node))
    (text/paragraph (make-projection/text/paragraph->tree/node))))

(def (macro e) text->tree ()
  '(make-projection/text->tree))

;;;;;;
;;; Tree

(def (function e) make-projection/tree->styled-string (&key delimiter-provider separator-provider indentation-provider)
  (type-dispatching
    (tree/leaf (make-projection/tree/leaf->styled-string :indentation-provider indentation-provider :delimiter-provider delimiter-provider))
    (tree/node (make-projection/tree/node->styled-string :indentation-provider indentation-provider :delimiter-provider delimiter-provider :separator-provider separator-provider))
    (text/base (preserving))
    (image/image (preserving))
    (string (preserving))))

(def (macro e) tree->styled-string (&key delimiter-provider separator-provider indentation-provider)
  `(make-projection/tree->styled-string :delimiter-provider ,delimiter-provider :separator-provider ,separator-provider :indentation-provider ,indentation-provider))

(def (function e) make-projection/tree->list ()
  (type-dispatching
    (string (make-projection/preserving))
    (tree/node (make-projection/tree->list))))

(def (macro e) tree->list ()
  '(make-projection/tree->list))

;;;;;;
;;; Graph

(def (function e) make-projection/graph->tree ()
  (type-dispatching
    ))

(def (macro e) graph->tree ()
  '(make-projection/graph->tree))

;;;;;;
;;; Statae machine

(def (function e) make-projection/state-machine->tree ()
  (type-dispatching
    (state-machine/state-machine (make-projection/state-machine/state-machine->tree/node))
    (state-machine/state (make-projection/state-machine/state->tree/node))
    (state-machine/transition (make-projection/state-machine/transition->tree/node))))

(def (macro e) state-machine->tree ()
  '(make-projection/state-machine->tree))

;;;;;;
;;; Table

(def (function e) make-projection/table->string ()
  (type-dispatching
    (table/table (make-projection/table/table->string))
    (t (preserving))))

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
    (xml/text (make-projection/xml/text->tree/leaf))
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
    (json/object-entry (make-projection/json/object-entry->tree/node))
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
;;; Javascript

(def (function e) make-projection/javascript->tree ()
  (type-dispatching
    (javascript/statement/block (make-projection/javascript/statement/block->tree/node))
    (javascript/statement/top-level (make-projection/javascript/statement/top-level->tree/node))
    (javascript/expression/variable-reference (make-projection/javascript/expression/variable-reference->tree/leaf))
    (javascript/expression/property-access (make-projection/javascript/expression/property-access->tree/node))
    (javascript/expression/constructor-invocation (make-projection/javascript/expression/constructor-invocation->tree/node))
    (javascript/expression/method-invocation (make-projection/javascript/expression/method-invocation->tree/node))
    (javascript/literal/string (make-projection/javascript/literal/string->tree/leaf))
    (javascript/declaration/variable (make-projection/javascript/declaration/variable->tree/node))
    (javascript/declaration/function (make-projection/javascript/declaration/function->tree/node))))

(def (macro e) javascript->tree ()
  '(make-projection/javascript->tree))

;;;;;;
;;; Lisp form

(def (function e) make-projection/lisp-form->tree ()
  (type-dispatching
    (lisp-form/comment (make-projection/lisp-form/comment->string))
    (lisp-form/number (make-projection/lisp-form/number->string))
    (lisp-form/symbol (make-projection/lisp-form/symbol->string))
    (lisp-form/string (make-projection/lisp-form/string->string))
    (lisp-form/list (make-projection/lisp-form/list->tree/node))
    (lisp-form/object (make-projection/lisp-form/object->string))
    (lisp-form/top-level (make-projection/lisp-form/top-level->tree/node))))

(def (macro e) lisp-form->tree ()
  '(make-projection/lisp-form->tree))

;;;;;;
;;; Common lisp

(def (function e) make-projection/common-lisp->lisp-form ()
  (type-dispatching
    (common-lisp/constant (make-projection/common-lisp/constant-form->lisp-form/string))
    (common-lisp/variable-reference (make-projection/common-lisp/variable-reference-form->lisp-form/string))
    (common-lisp/if (make-projection/common-lisp/if-form->lisp-form/list))
    (common-lisp/the (make-projection/common-lisp/the-form->lisp-form/list))
    (common-lisp/progn (make-projection/common-lisp/progn-form->lisp-form/list))
    (common-lisp/lexical-variable-binding (make-projection/common-lisp/lexical-variable-binding-form->lisp-form/list))
    (common-lisp/let (make-projection/common-lisp/let-form->lisp-form/list))
    (common-lisp/application (make-projection/common-lisp/application-form->lisp-form/list))
    (common-lisp/function-definition (make-projection/common-lisp/function-definition-form->lisp-form/list))
    (common-lisp/lambda-function (make-projection/common-lisp/lambda-function-form->lisp-form/list))
    (common-lisp/function-argument (make-projection/common-lisp/function-argument-form->lisp-form/string))
    (common-lisp/comment (make-projection/common-lisp/comment->lisp-form/comment))
    (common-lisp/top-level (make-projection/common-lisp/top-level->lisp-form/top-level))))

(def (macro e) common-lisp->lisp-form ()
  '(make-projection/common-lisp->lisp-form))
