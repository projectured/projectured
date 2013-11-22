;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; JSON

(def function make-test-selection/json/nothing ()
  '(the json/nothing (content-of (the document document))))

(def function make-test-selection/json/object ()
  '(the json/object (elt (the list (elements-of (the json/array (content-of (the document document))))) 5)))

(def function make-test-selection/json/character ()
  '(the character (elt (the string (value-of (the json/string (elt (the list (elements-of (the json/array (content-of (the document document))))) 4)))) 8)))

(def function make-test-selection/json/character-position ()
  '(the sequence-position (pos (the string (value-of (the json/string (elt (the list (elements-of (the json/array (content-of (the document document))))) 4)))) 8)))
