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
    (scroll-pane (:location (make-2d 0 0) :size (make-2d 800 600))
      (document (:selection selection)
        content))
    (tooltip (:location (make-2d 100 100))
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
;;; Text

(def function make-test-document/text/empty ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the make-styled-string/string (elt (the list (elements-of (the text/paragraph (elt (the list (elements-of (the text/document (content-of (the document document))))) 0)))) 0)))) 0)))
    (make-test-content/text/empty)))

(def function make-test-document/text ()
  (test-document (:selection '(the sequence-position (pos (the string (content-of (the styled-string/string (elt (the list (elements-of (the text/paragraph (elt (the list (elements-of (the text/document (content-of (the document document))))) 1)))) 0)))) 4)))
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

(def function make-test-document/tree ()
  (test-document (:selection '(the sequence-position (pos (the string (elt (the list (children-of (the tree/node (elt (the list (children-of (the tree/node (content-of (the document document))))) 1)))) 1)) 1)))
    (make-test-content/tree)))

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
  (test-document (:selection '(the sequence-position (pos (the string (value-of (the xml/attribute (elt (the list (attributes-of (the xml/element (elt (the list (children-of (the xml/element (elt (the list (children-of (the xml/element (content-of (the document document))))) 0)))) 1)))) 1)))) 2)))
    (make-test-content/xml)))

;;;;;;
;;; JSON

(def function make-test-document/json/empty ()
  (test-document (:selection '(the null (content-of (the document document))))
    (make-test-content/json/empty)))

(def function make-test-document/json ()
  (test-document (:selection '(the sequence-position (pos (the string (text-of (the json/string (elt (the list (elements-of (the json/array (content-of (the document document))))) 4)))) 10)))
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
;;; Lisp form

(def function make-test-document/lisp-form/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/lisp-form/empty)))

(def function make-test-document/lisp-form ()
  (test-document (:selection '(the sequence-position (pos (the string (elt (the list (content-of (the document document))) 0)) 3)))
    (make-test-content/lisp-form)))

;;;;;;
;;; Walked lisp form

(def function make-test-document/walked-lisp-form/empty ()
  (test-document (:selection '(content-of (the document document)))
    (make-test-content/walked-lisp-form/empty)))

(def function make-test-document/walked-lisp-form ()
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the hu.dwim.walker:function-definition-form (content-of (the document document))) 'hu.dwim.walker::docstring)) 7)))
    (make-test-content/walked-lisp-form)))

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
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the hu.dwim.walker:constant-form (elt (the list (slot-value (the hu.dwim.walker:free-application-form (slot-value (the hu.dwim.walker:if-form (elt (the list (slot-value (the hu.dwim.walker:lambda-function-form (content-of (the document document))) 'hu.dwim.walker::body)) 0)) 'hu.dwim.walker::condition)) 'hu.dwim.walker::arguments)) 0)) 'hu.dwim.walker::value)) 1)))
    (make-test-content/nested)))

;;;;;;
;;; Complex

(def function make-test-document/complex ()
  (test-document (:selection '(the sequence-position (pos (the string (slot-value (the hu.dwim.walker:function-definition-form (content-of (content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of (the table/table (content-of (the document document))))) 1)))) 1))))) 'hu.dwim.walker::docstring)) 7)))
    (make-test-content/complex)))

;;;;;;
;;; Wow

(def function make-test-document/wow ()
  (test-document ()
    (make-test-content/wow)))
