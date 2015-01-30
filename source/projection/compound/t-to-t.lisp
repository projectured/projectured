;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Graphics

(def function make-projection/graphics->graphics@cache-graphics (&key debug)
  (type-dispatching
    (graphics/canvas (graphics/canvas->graphics/image@cache-graphics :debug debug))
    (graphics/viewport (graphics/viewport->graphics/image@cache-graphics :debug debug))
    (graphics/base (preserving))))

(def macro graphics->graphics@cache-graphics (&key debug)
  `(make-projection/graphics->graphics@cache-graphics :debug ,debug))

(def function make-projection/graphics->graphics@mark-changes ()
  (type-dispatching
    (graphics/canvas (graphics/canvas->graphics/canvas@mark-changes))
    (graphics/viewport (graphics/viewport->graphics/viewport@mark-changes))
    (graphics/base (preserving))))

(def macro graphics->graphics@mark-changes ()
  '(make-projection/graphics->graphics@mark-changes))

;;;;;;
;;; Text

(def function make-projection/text->graphics ()
  (make-projection/text/text->graphics/canvas))

(def macro text->graphics ()
  '(make-projection/text/text->graphics/canvas))

;;;;;;
;;; Widget

(def function make-projection/document->t (factory)
  (type-dispatching
    (document/document (document/document->t))
    (document/nothing (document/nothing->tree/leaf))
    (document/insertion (document/insertion->tree/leaf factory))
    (document/search (document/search->text/text))
    (document/clipboard (document/clipboard->t))))

(def macro document->t (factory)
  `(make-projection/document->t ,factory))

;;;;;;
;;; Widget

(def function make-projection/widget->graphics ()
  (type-dispatching
    (widget/label (make-projection/widget/label->graphics/canvas))
    (widget/text (make-projection/widget/text->graphics/canvas))
    (widget/tooltip (make-projection/widget/tooltip->graphics/canvas))
    (widget/menu (make-projection/widget/menu->graphics/canvas))
    (widget/menu-item (make-projection/widget/menu-item->graphics/canvas))
    (widget/composite (make-projection/widget/composite->graphics/canvas))
    (widget/shell (make-projection/widget/shell->graphics/canvas))
    (widget/title-pane (make-projection/widget/title-pane->graphics/canvas))
    (widget/split-pane (make-projection/widget/split-pane->graphics/canvas))
    (widget/tabbed-pane (make-projection/widget/tabbed-pane->graphics/canvas))
    (widget/scroll-pane (make-projection/widget/scroll-pane->graphics/canvas))))

(def macro widget->graphics ()
  '(make-projection/widget->graphics))

;;;;;;
;;; Book

(def function make-projection/book->tree ()
  (type-dispatching
    (book/book (book/book->tree/node))
    (book/chapter (book/chapter->tree/node))
    (book/paragraph (book/paragraph->tree/leaf))
    (book/list (book/list->tree/node))
    (book/picture (book/picture->tree/leaf))))

(def macro book->tree ()
  '(make-projection/book->tree))

;;;;;;
;;; Tree

(def function make-projection/tree->text ()
  (type-dispatching
    (tree/leaf (tree/leaf->text/text))
    (tree/node (tree/node->text/text))))

(def macro tree->text ()
  `(make-projection/tree->text))

;;;;;;
;;; Graph

(def function make-projection/graph->tree ()
  (type-dispatching
    ))

(def macro graph->tree ()
  '(make-projection/graph->tree))

;;;;;;
;;; Statae machine

(def function make-projection/state-machine->tree ()
  (type-dispatching
    (state-machine/state-machine (make-projection/state-machine/state-machine->tree/node))
    (state-machine/state (make-projection/state-machine/state->tree/node))
    (state-machine/transition (make-projection/state-machine/transition->tree/node))))

(def macro state-machine->tree ()
  '(make-projection/state-machine->tree))

;;;;;;
;;; List

(def function make-projection/list->text ()
  (make-projection/list/list->text))

(def macro list->text ()
  '(make-projection/list->text))

;;;;;;
;;; Table

(def function make-projection/table->text ()
  (make-projection/table/table->text))

(def macro table->text ()
  '(make-projection/table->text))

;;;;;;
;;; Inspector

(def function make-projection/inspector->table ()
  (type-dispatching
    (inspector/object (make-projection/inspector/object->table/table))
    (inspector/object-slot (make-projection/inspector/object-slot->table/row))))

(def macro inspector->table ()
  '(make-projection/inspector->table))

;;;;;;
;;; XML

(def function make-projection/xml->tree ()
  (type-dispatching
    (xml/text (make-projection/xml/text->tree/leaf))
    (xml/attribute (make-projection/xml/attribute->tree/node))
    (xml/element (make-projection/xml/element->tree/node))))

(def macro xml->tree ()
  '(make-projection/xml->tree))

;;;;;;
;;; JSON

(def function make-projection/json->tree ()
  (type-dispatching
    (json/nothing (make-projection/json/nothing->tree/leaf))
    (json/null (make-projection/json/null->tree/leaf))
    (json/boolean (make-projection/json/boolean->tree/leaf))
    (json/number (make-projection/json/number->tree/leaf))
    (json/string (make-projection/json/string->tree/leaf))
    (json/array (make-projection/json/array->tree/node))
    (json/object-entry (make-projection/json/object-entry->tree/node))
    (json/object (make-projection/json/object->tree/node))))

(def macro json->tree ()
  '(make-projection/json->tree))

;;;;;;
;;; Javascript

(def function make-projection/javascript->tree ()
  (type-dispatching
    (javascript/statement/block (make-projection/javascript/statement/block->tree/node))
    (javascript/statement/top-level (make-projection/javascript/statement/top-level->tree/node))
    (javascript/expression/variable-reference (make-projection/javascript/expression/variable-reference->tree/leaf))
    (javascript/expression/property-access (make-projection/javascript/expression/property-access->tree/node))
    (javascript/expression/constructor-invocation (make-projection/javascript/expression/constructor-invocation->tree/node))
    (javascript/expression/method-invocation (make-projection/javascript/expression/method-invocation->tree/node))
    (javascript/literal/string (make-projection/javascript/literal/string->tree/leaf))
    (javascript/definition/variable (make-projection/javascript/definition/variable->tree/node))
    (javascript/definition/function (make-projection/javascript/definition/function->tree/node))))

(def macro javascript->tree ()
  '(make-projection/javascript->tree))

;;;;;;
;;; CSS

(def function make-projection/css->tree ()
  (type-dispatching
    (css/attribute (make-projection/css/attribute->tree/leaf))
    (css/rule (make-projection/css/rule->tree/node))))

(def macro css->tree ()
  '(make-projection/css->tree))

;;;;;;
;;; T

(def function make-projection/t->tree (&key slot-provider)
  (type-dispatching
    (null (make-projection/t/null->text/text))
    (number (make-projection/t/number->text/text))
    (string (make-projection/t/string->text/text))
    (symbol (make-projection/t/symbol->text/text))
    (pathname (make-projection/t/pathname->text/text))
    (sequence (make-projection/t/sequence->tree/node))
    ((or structure-object standard-object) (make-projection/t/object->tree/node :slot-provider slot-provider))))

(def macro t->tree (&key slot-provider)
  `(make-projection/t->tree :slot-provider ,slot-provider))

(def function make-projection/t->table (&key slot-provider)
  (type-dispatching
    (null (make-projection/t/null->text/text))
    (number (make-projection/t/number->text/text))
    (string (make-projection/t/string->text/text))
    (symbol (make-projection/t/symbol->text/text))
    (pathname (make-projection/t/pathname->text/text))
    (sequence (make-projection/t/sequence->table/table))
    (hash-table (make-projection/t/hash-table->table/table))
    (function (make-projection/t/function->table/table))
    ((or structure-object standard-object) (make-projection/t/object->table/table :slot-provider slot-provider))))

(def macro t->table (&key slot-provider)
  `(make-projection/t->table :slot-provider ,slot-provider))

;;;;;;
;;; File system

(def function make-projection/file-system->tree ()
  (type-dispatching
    (file-system/file (make-projection/file-system/file->tree/leaf))
    (file-system/directory (make-projection/file-system/directory->tree/node))))

(def macro file-system->tree ()
  '(make-projection/file-system->tree))

;;;;;;
;;; Java

(def function make-projection/java->tree ()
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
    (java/definition/method (make-projection/java/definition/method->tree/node))
    (java/definition/argument (make-projection/java/definition/argument->tree/node))
    (java/definition/qualifier (make-projection/java/definition/qualifier->string))
    (java/definition/type (make-projection/java/definition/type->string))))

(def macro java->tree ()
  '(make-projection/java->tree))

;;;;;;
;;; Lisp form

(def function make-projection/lisp-form->tree ()
  (type-dispatching
    (lisp-form/comment (make-projection/lisp-form/comment->tree/node))
    (lisp-form/number (make-projection/lisp-form/number->tree/leaf))
    (lisp-form/symbol (make-projection/lisp-form/symbol->tree/leaf))
    (lisp-form/string (make-projection/lisp-form/string->tree/leaf))
    (lisp-form/quote (make-projection/lisp-form/quote->tree/node))
    (lisp-form/list (make-projection/lisp-form/list->tree/node))
    (lisp-form/object (make-projection/lisp-form/object->tree/leaf))
    (lisp-form/top-level (make-projection/lisp-form/top-level->tree/node))))

(def macro lisp-form->tree ()
  '(make-projection/lisp-form->tree))

(def function make-projection/lisp-form->form ()
  (type-dispatching
    (lisp-form/number (make-projection/lisp-form/number->number))
    (lisp-form/string (make-projection/lisp-form/string->string))
    (lisp-form/symbol (make-projection/lisp-form/symbol->symbol))
    (lisp-form/quote (make-projection/lisp-form/quote->list))
    (lisp-form/list (make-projection/lisp-form/list->list))))

(def macro lisp-form->form ()
  '(make-projection/lisp-form->form))

;;;;;;
;;; Common lisp

(def function make-projection/common-lisp->lisp-form ()
  (type-dispatching
    (common-lisp/constant (make-projection/common-lisp/constant->lisp-form/string))
    (common-lisp/variable-reference (make-projection/common-lisp/variable-reference->lisp-form/symbol))
    (common-lisp/function-reference (make-projection/common-lisp/function-reference->lisp-form/symbol))
    (common-lisp/if (make-projection/common-lisp/if->lisp-form/list))
    (common-lisp/the (make-projection/common-lisp/the->lisp-form/list))
    (common-lisp/progn (make-projection/common-lisp/progn->lisp-form/list))
    (common-lisp/lexical-variable-binding (make-projection/common-lisp/lexical-variable-binding->lisp-form/list))
    (common-lisp/let (make-projection/common-lisp/let->lisp-form/list))
    (common-lisp/application (make-projection/common-lisp/application->lisp-form/list))
    (common-lisp/special-variable-definition (make-projection/common-lisp/special-variable-definition->lisp-form/list))
    (common-lisp/function-definition (make-projection/common-lisp/function-definition->lisp-form/list))
    (common-lisp/lambda-function (make-projection/common-lisp/lambda-function->lisp-form/list))
    (common-lisp/function-argument (make-projection/common-lisp/function-argument->lisp-form/string))
    (common-lisp/comment (make-projection/common-lisp/comment->lisp-form/comment))
    (common-lisp/top-level (make-projection/common-lisp/top-level->lisp-form/top-level))))

(def macro common-lisp->lisp-form ()
  '(make-projection/common-lisp->lisp-form))

;;;;;;
;;; Test

(def function make-projection/test->tree ()
  (type-dispatching
    (test/check (test/check->tree/node))
    (test/result (test/result->tree/leaf))))

(def macro test->tree ()
  '(make-projection/test->tree))

;;;;;;
;;; Workbench

(def function make-projection/workbench->widget ()
  (type-dispatching
    (workbench/workbench (workbench/workbench->widget/shell))
    (workbench/navigator (workbench/navigator->widget/title-pane))
    (workbench/console (workbench/console->widget/title-pane))
    (workbench/editor (workbench/editor->widget/tabbed-pane))
    (workbench/document (workbench/document->widget/scroll-pane))))

(def macro workbench->widget ()
  '(make-projection/workbench->widget))
