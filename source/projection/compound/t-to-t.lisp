;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Text to graphics

(def function make-projection/text->graphics ()
  (make-projection/text/text->graphics/canvas))

(def macro text->graphics ()
  '(make-projection/text/text->graphics/canvas))

;;;;;;
;;; Widget to graphics

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
;;; Tree to text

(def function make-projection/tree->text ()
  (type-dispatching
    (tree/leaf (tree/leaf->text/text))
    (tree/node (tree/node->text/text))))

(def macro tree->text ()
  `(make-projection/tree->text))

;;;;;;
;;; T

(def function make-projection/t->tree (slot-provider)
  (type-dispatching
    (null (make-projection/t/null->tree/leaf))
    ((or number document/number) (make-projection/t/number->tree/leaf))
    ((or string document/string) (make-projection/t/string->tree/leaf))
    (symbol (make-projection/t/symbol->tree/leaf))
    (pathname (make-projection/t/pathname->tree/leaf))
    (sequence (make-projection/t/sequence->tree/node))
    ((or structure-object standard-object) (make-projection/t/object->tree/node slot-provider))))

(def macro t->tree (slot-provider)
  `(make-projection/t->tree ,slot-provider))

;;;;;;
;;; Document to tree

(def function make-projection/document->tree (factory searcher)
  (type-dispatching
    (document/document (document/document->t))
    (document/nothing (document/nothing->tree/leaf))
    (document/insertion (document/insertion->tree/leaf factory))
    (document/clipboard (document/clipboard->t))
    (document/search (document/search->document/search-result searcher))))

(def macro document->tree (factory searcher)
  `(make-projection/document->tree ,factory ,searcher))

;;;;;;
;;; Book to tree

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
;;; JSON to tree

(def function make-projection/json->tree ()
  (type-dispatching
    (json/insertion (make-projection/json/insertion->tree/leaf))
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
;;; XML to tree

(def function make-projection/xml->tree ()
  (type-dispatching
    (xml/insertion (make-projection/xml/insertion->tree/leaf))
    (xml/text (make-projection/xml/text->tree/leaf))
    (xml/attribute (make-projection/xml/attribute->tree/node))
    (xml/element (make-projection/xml/element->tree/node))))

(def macro xml->tree ()
  '(make-projection/xml->tree))

;;;;;;
;;; Lisp form to tree

(def function make-projection/lisp-form->tree ()
  (type-dispatching
    (lisp-form/insertion (make-projection/lisp-form/insertion->tree/leaf))
    (lisp-form/comment (make-projection/lisp-form/comment->tree/node))
    (lisp-form/number (make-projection/lisp-form/number->tree/leaf))
    (lisp-form/symbol (make-projection/lisp-form/symbol->tree/leaf))
    (lisp-form/string (make-projection/lisp-form/string->tree/leaf))
    (lisp-form/quote (make-projection/lisp-form/quote->tree/node))
    (lisp-form/list (make-projection/lisp-form/list->tree/node))
    (lisp-form/object (make-projection/lisp-form/object->tree/leaf))
    (lisp-form/toplevel (make-projection/lisp-form/toplevel->tree/node))))

(def macro lisp-form->tree ()
  '(make-projection/lisp-form->tree))

;;;;;;
;;; Lisp form to tree

(def function make-projection/lisp-form->form ()
  (type-dispatching
    (lisp-form/number (make-projection/lisp-form/number->number))
    (lisp-form/string (make-projection/lisp-form/string->string))
    (lisp-form/symbol (make-projection/lisp-form/symbol->symbol))
    (lisp-form/quote (make-projection/lisp-form/quote->list))
    (lisp-form/list (make-projection/lisp-form/list->list))
    (lisp-form/toplevel (make-projection/lisp-form/toplevel->list))
    (document/string (document/string->string))
    (document/number (document/number->number))
    ((or number symbol string) (preserving))))

(def macro lisp-form->form ()
  '(make-projection/lisp-form->form))

;;;;;;
;;; Common lisp to lisp form

(def function make-projection/common-lisp->lisp-form ()
  (type-dispatching
    (common-lisp/insertion (make-projection/common-lisp/insertion->lisp-form/insertion))
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
    (common-lisp/function-argument (make-projection/common-lisp/function-argument->lisp-form/symbol))
    (common-lisp/comment (make-projection/common-lisp/comment->lisp-form/comment))
    (common-lisp/toplevel (make-projection/common-lisp/toplevel->lisp-form/toplevel))))

(def macro common-lisp->lisp-form ()
  '(make-projection/common-lisp->lisp-form))

;;;;;;
;;; Repl to tree

(def function make-projection/evaluator->tree ()
  (type-dispatching
    (evaluator/toplevel (make-projection/evaluator/toplevel->tree/node))
    (evaluator/form (make-projection/evaluator/form->tree/node))))

(def macro evaluator->tree ()
  '(make-projection/evaluator->tree))
