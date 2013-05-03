;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Widget

(def function make-test-document (content &key selection)
  (composite ()
    (scroll-pane (:location (make-2d 0 0) :size (make-2d 1024 768) :margin (make-inset :all 5))
      (document (:selection selection)
        content))
    (tooltip (:location (make-2d 100 100) :margin (make-inset :all 5))
      nil)))

(def macro test-document ((&key selection) &body content)
  `(make-test-document ,(first content) :selection ,selection))

;;;;;;
;;; Graphics

(def function make-test-document/graphics/empty ()
  (test-document ()
    (make-test-content/graphics/empty)))

(def function make-test-document/graphics ()
  (test-document ()
    (make-test-content/graphics)))

;;;;;;
;;; String

(def function make-test-document/string/empty ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the document document))) 0)))
    (make-test-content/string/empty)))

(def function make-test-document/string ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the document document))) 4)))
    (make-test-content/string)))

;;;;;;
;;; Styled string

(def function make-test-document/styled-string ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/text (content-of (the document document))))) 0)))) 2)))
    (make-test-content/styled-string)))

;;;;;;
;;; Text

(def function make-test-document/text/empty ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the make-text/string (elt (the list (elements-of (the text/paragraph (elt (the list (elements-of (the text/text (content-of (the document document))))) 0)))) 0)))) 0)))
    (make-test-content/text/empty)))

(def function make-test-document/text ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the text/string (elt (the list (elements-of (the text/paragraph (elt (the list (elements-of (the text/text (content-of (the document document))))) 1)))) 0)))) 4)))
    (make-test-content/text)))

;;;;;;
;;; List

(def function make-test-document/list/empty ()
  (test-document (:selection '(the sequence-position (pos (the list (elements-of (the list/list (content-of (the document document))))) 0)))
    (make-test-content/list/empty)))

(def function make-test-document/list ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the list/element (elt (the list (elements-of (the list/list (content-of (the document document))))) 0)))) 2)))
    (make-test-content/list)))

;;;;;;
;;; Table

(def function make-test-document/table/empty ()
  (test-document (:selection '(the sequence-position (pos (the list (cells-of (the table/row (elt (the list (rows-of (the table/table (content-of (the document document))))) 0)))) 0)))
    (make-test-content/table/empty)))

(def function make-test-document/table ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table (content-of (the document document))))) 1)))) 0)))) 25)))
    (make-test-content/table)))

;;;;;;
;;; Tree

(def function make-test-document/tree/empty ()
  (test-document (:selection '(the null (content-of (the document document))))
    (make-test-content/tree/empty)))

(def function make-test-document/tree/leaf ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the tree/leaf (content-of (the document document))))) 1)))
    (make-test-content/tree/leaf)))

(def function make-test-document/tree/node ()
  (test-document (:selection '(the sequence-position (pos (the string (opening-delimiter (the tree/node (content-of (the document document))) "(")) 1)))
    (make-test-content/tree/node)))

(def function make-test-document/tree ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt (the list (children-of (the tree/node (content-of (the document document))))) 1)))) 1)))) 1)))
    (make-test-content/tree)))

;;;;;;
;;; Graph

(def function make-test-document/graph ()
  (test-document ()
    (make-test-content/graph)))

;;;;;;
;;; State machine

(def function make-test-document/state-machine ()
  (test-document ()
    (make-test-content/state-machine)))

;;;;;;
;;; Book

(def function make-test-document/book/empty ()
  (test-document (:selection '(the sequence-position (pos (the string (title-of (the book/book (content-of (the document document))))) 0)))
    (make-test-content/book/empty)))

(def function make-test-document/book ()
  (test-document (:selection '(the sequence-position (pos (the string (title-of (the book/chapter (elt (the list (elements-of (the book/book (content-of (the document document))))) 0)))) 3)))
    (make-test-content/book)))

;;;;;;
;;; XML

(def function make-test-document/xml/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/xml/empty)))

(def function make-test-document/xml ()
  (test-document (:selection '(the sequence-position (pos (the string (value-of (the xml/attribute (elt (the list (attributes-of (the xml/element (elt (the list (children-of (the xml/element (elt (the list (children-of (the xml/element (content-of (the document document))))) 1)))) 1)))) 1)))) 2)))
    (make-test-content/xml)))

;;;;;;
;;; JSON

(def function make-test-document/json/empty ()
  (test-document (:selection '(the null (content-of (the document document))))
    (make-test-content/json/empty)))

(def function make-test-document/json ()
  (test-document (:selection '(the sequence-position (pos (the string (text-of (the json/string (elt (the list (elements-of (the json/array (content-of (the document document))))) 4)))) 8)))
    (make-test-content/json)))

;;;;;;
;;; Java

(def function make-test-document/java/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/java/empty)))

(def function make-test-document/java ()
  (test-document (:selection '(the sequence-position (pos (the string (name-of (the java/declaration/method (content-of (the document document))))) 2)))
    (make-test-content/java)))

;;;;;;
;;; Javascript

(def function make-test-document/javascript ()
  (test-document ()
    (make-test-content/javascript)))

;;;;;;
;;; Lisp form

(def function make-test-document/lisp-form/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/lisp-form/empty)))

(def function make-test-document/lisp-form ()
  (test-document (:selection '(the sequence-position (pos (the string (elt (the list (content-of (the document document))) 0)) 3)))
    (make-test-content/lisp-form)))

;;;;;;
;;; Common lisp

(def function make-test-document/common-lisp/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/common-lisp/empty)))

(def function make-test-document/common-lisp ()
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the common-lisp/function-definition (content-of (the document document))) 'documentation)) 7)))
    (make-test-content/common-lisp)))

;;;;;
;;; Evaluator

(def function make-test-document/evaluator ()
  (test-document ()
    (make-test-content/evaluator)))

;;;;;;
;;; Test

(def function make-test-document/test ()
  (test-document ()
    (make-test-content/test)))

;;;;;;
;;; T

(def function make-test-document/t/empty ()
  (test-document ()
    (make-test-content/t/empty)))

(def function make-test-document/t ()
  (test-document ()
    (make-test-content/t)))

;;;;;;
;;; Nested

(def function make-test-document/nested ()
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the common-lisp/constant (elt (the list (slot-value (the common-lisp/application (slot-value (the common-lisp/if (elt (the list (slot-value (the common-lisp/lambda-function (content-of (the document document))) 'body)) 0)) 'condition)) 'arguments)) 0)) 'value)) 1)))
    (make-test-content/nested)))

;;;;;;
;;; Complex

(def function make-test-document/complex ()
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the common-lisp/function-definition (content-of (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table (content-of (the document document))))) 1)))) 1))))) 'documentation)) 7)))
    (make-test-content/complex)))

;;;;;;
;;; Demo

;;(the sequence-position (pos (the string (title-of (the book/chapter (elt (the list (elements-of (the book/book (content-of (the document document))))) 0)))) 3))
(def function make-test-document/demo ()
  (test-document (:selection '(the sequence-position (pos (the string (title-of (the book/book (content-of (the document document))))) 2)))
    (make-test-content/demo)))

;;;;;;
;;; Wow

(def function make-test-document/wow ()
  (test-document ()
    (make-test-content/wow)))
