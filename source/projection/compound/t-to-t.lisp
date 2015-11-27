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

(def function make-projection/document->tree (factory)
  (type-dispatching
    (document/document (document/document->t))
    (document/nothing (document/nothing->tree/leaf))
    (document/insertion (document/insertion->tree/leaf factory))
    (document/clipboard (document/clipboard->t))))

(def macro document->tree (factory)
  `(make-projection/document->tree ,factory))

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
