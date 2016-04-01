;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Primitive to t

(def function make-projection/primitive->t ()
  (type-dispatching
    (primitive/boolean (primitive/boolean->boolean))
    (primitive/number (primitive/number->number))
    (primitive/string (primitive/string->string))))

(def macro primitive->t ()
  '(make-projection/primitive->t))

;;;;;;
;;; Primitive to text

(def function make-projection/primitive->text ()
  (type-dispatching
    (primitive/boolean (primitive/boolean->text/text))
    (primitive/number (primitive/number->text/text))
    (primitive/string (primitive/string->text/text))))

(def macro primitive->text ()
  '(make-projection/primitive->text))

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
;;; Syntax to text

(def function make-projection/syntax->text ()
  (type-dispatching
    (syntax/node (syntax/node->text/text))
    (syntax/leaf (syntax/leaf->text/text))
    (syntax/delimitation (syntax/delimitation->text/text))
    (syntax/indentation (syntax/indentation->text/text))
    (syntax/collapsible (syntax/collapsible->text/text))
    (syntax/separation (syntax/separation->text/text))))

(def macro syntax->text ()
  `(make-projection/syntax->text))

;;;;;;
;;; T

(def function make-projection/t->syntax (slot-provider)
  (type-dispatching
    (null (make-projection/t/null->syntax/leaf))
    ((or number primitive/number) (make-projection/t/number->syntax/leaf))
    ((or string primitive/string) (make-projection/t/string->syntax/leaf))
    (symbol (make-projection/t/symbol->syntax/leaf))
    (pathname (make-projection/t/pathname->syntax/leaf))
    (sequence (make-projection/t/sequence->syntax/node))
    ((or structure-object standard-object) (make-projection/t/object->syntax/node slot-provider))))

(def macro t->syntax (slot-provider)
  `(make-projection/t->syntax ,slot-provider))

;;;;;;
;;; Workbench to widget

(def function make-projection/workbench->widget ()
  (type-dispatching
    (workbench/workbench (workbench/workbench->widget/shell))
    (workbench/page (workbench/page->widget/tabbed-pane))
    (workbench/navigator (workbench/navigator->widget/scroll-pane))
    (workbench/console (workbench/console->widget/scroll-pane))
    (workbench/descriptor (workbench/descriptor->widget/scroll-pane))
    (workbench/operator (workbench/operator->widget/scroll-pane))
    (workbench/searcher (workbench/searcher->widget/scroll-pane))
    (workbench/evaluator (workbench/evaluator->widget/scroll-pane))
    (workbench/document (workbench/document->widget/scroll-pane))))

(def macro workbench->widget ()
  '(make-projection/workbench->widget))

;;;;;;
;;; Clipboard to t

(def function make-projection/clipboard->t ()
  (type-dispatching
    (clipboard/slice (clipboard/slice->t))
    (clipboard/collection (clipboard/collection->t))))

(def macro clipboard->t ()
  `(make-projection/clipboard->t))

;;;;;;
;;; Document to t

(def function make-projection/document->t ()
  (type-dispatching))

(def macro document->t ()
  `(make-projection/document->t))

;;;;;;
;;; Document to syntax

(def function make-projection/document->syntax (factory searcher)
  (type-dispatching
    (document/nothing (document/nothing->syntax/leaf))
    (document/insertion (document/insertion->syntax/leaf factory))))

(def macro document->syntax (factory searcher)
  `(make-projection/document->syntax ,factory ,searcher))

;;;;;;
;;; Book to syntax

(def function make-projection/book->syntax ()
  (type-dispatching
    (book/book (book/book->syntax/node))
    (book/chapter (book/chapter->syntax/node))
    (book/paragraph (book/paragraph->syntax/leaf))
    (book/list (book/list->syntax/node))
    (book/picture (book/picture->syntax/leaf))))

(def macro book->syntax ()
  '(make-projection/book->syntax))

;;;;;;
;;; File system

(def function make-projection/file-system->syntax ()
  (type-dispatching
    (file-system/file (file-system/file->syntax/leaf))
    (file-system/directory (file-system/directory->syntax/node))))

(def macro file-system->syntax ()
  '(make-projection/file-system->syntax))

;;;;;;
;;; JSON to syntax

(def function make-projection/json->syntax ()
  (type-dispatching
    (json/insertion (make-projection/json/insertion->syntax/leaf))
    (json/null (make-projection/json/null->syntax/leaf))
    (json/boolean (make-projection/json/boolean->syntax/leaf))
    (json/number (make-projection/json/number->syntax/leaf))
    (json/string (make-projection/json/string->syntax/leaf))
    (json/array (make-projection/json/array->syntax/node))
    (json/object-entry (make-projection/json/object-entry->syntax/node))
    (json/object (make-projection/json/object->syntax/node))))

(def macro json->syntax ()
  '(make-projection/json->syntax))

;;;;;;
;;; XML to syntax

(def function make-projection/xml->syntax ()
  (type-dispatching
    (xml/insertion (make-projection/xml/insertion->syntax/leaf))
    (xml/text (make-projection/xml/text->syntax/leaf))
    (xml/attribute (make-projection/xml/attribute->syntax/node))
    (xml/element (make-projection/xml/element->syntax/node))))

(def macro xml->syntax ()
  '(make-projection/xml->syntax))

;;;;;;
;;; Lisp form to syntax

(def function make-projection/s-expression->syntax ()
  (type-dispatching
    (s-expression/insertion (make-projection/s-expression/insertion->syntax/leaf))
    (s-expression/comment (make-projection/s-expression/comment->syntax/node))
    (s-expression/number (make-projection/s-expression/number->syntax/leaf))
    (s-expression/symbol (make-projection/s-expression/symbol->syntax/leaf))
    (s-expression/string (make-projection/s-expression/string->syntax/leaf))
    (s-expression/quote (make-projection/s-expression/quote->syntax/node))
    (s-expression/list (make-projection/s-expression/list->syntax/node))
    (s-expression/object (make-projection/s-expression/object->syntax/leaf))
    (s-expression/toplevel (make-projection/s-expression/toplevel->syntax/node))))

(def macro s-expression->syntax ()
  '(make-projection/s-expression->syntax))

;;;;;;
;;; Lisp form to syntax

(def function make-projection/s-expression->form ()
  (type-dispatching
    (s-expression/number (make-projection/s-expression/number->number))
    (s-expression/string (make-projection/s-expression/string->string))
    (s-expression/symbol (make-projection/s-expression/symbol->symbol))
    (s-expression/quote (make-projection/s-expression/quote->list))
    (s-expression/list (make-projection/s-expression/list->list))
    (s-expression/toplevel (make-projection/s-expression/toplevel->list))
    (primitive/string (primitive/string->string))
    (primitive/number (primitive/number->number))
    ((or number symbol string) (preserving))))

(def macro s-expression->form ()
  '(make-projection/s-expression->form))

;;;;;;
;;; Common lisp to lisp form

(def function make-projection/common-lisp->s-expression ()
  (type-dispatching
    (common-lisp/insertion (make-projection/common-lisp/insertion->s-expression/insertion))
    (common-lisp/constant (make-projection/common-lisp/constant->s-expression/string))
    (common-lisp/variable-reference (make-projection/common-lisp/variable-reference->s-expression/symbol))
    (common-lisp/function-reference (make-projection/common-lisp/function-reference->s-expression/symbol))
    (common-lisp/if (make-projection/common-lisp/if->s-expression/list))
    (common-lisp/the (make-projection/common-lisp/the->s-expression/list))
    (common-lisp/progn (make-projection/common-lisp/progn->s-expression/list))
    (common-lisp/lexical-variable-binding (make-projection/common-lisp/lexical-variable-binding->s-expression/list))
    (common-lisp/let (make-projection/common-lisp/let->s-expression/list))
    (common-lisp/application (make-projection/common-lisp/application->s-expression/list))
    (common-lisp/special-variable-definition (make-projection/common-lisp/special-variable-definition->s-expression/list))
    (common-lisp/function-definition (make-projection/common-lisp/function-definition->s-expression/list))
    (common-lisp/lambda-function (make-projection/common-lisp/lambda-function->s-expression/list))
    (common-lisp/function-argument (make-projection/common-lisp/function-argument->s-expression/symbol))
    (common-lisp/comment (make-projection/common-lisp/comment->s-expression/comment))
    (common-lisp/toplevel (make-projection/common-lisp/toplevel->s-expression/toplevel))))

(def macro common-lisp->s-expression ()
  '(make-projection/common-lisp->s-expression))

;;;;;;
;;; Evaluator to syntax

(def function make-projection/evaluator->syntax ()
  (type-dispatching
    (evaluator/toplevel (make-projection/evaluator/toplevel->syntax/node))
    (evaluator/form (make-projection/evaluator/form->syntax/node))))

(def macro evaluator->syntax ()
  '(make-projection/evaluator->syntax))

;;;;;;
;;; Searching

(def function make-projection/searching->syntax ()
  (type-dispatching
    (searching/result (searching/result->syntax/node))
    (searching/result-element (searching/result-element->syntax/node))))

(def macro searching->syntax ()
  '(make-projection/searching->syntax))
