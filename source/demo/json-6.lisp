;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 6
;;;;
;;;; Goal:
;;;;  - add automatic line numbering
;;;;
;;;; Implementation:
;;;;  - redefine test projection

(in-package :projectured.test)

(def function make-test-projection/json ()
  (make-test-projection
   (nesting
     (widget->graphics)
     (sequential
       (nesting
         (document->document)
         (recursive (json->tree)))
       (nesting
         (document->document)
         (recursive (tree->text)))
       (nesting
         (document->document)
         (text->line-numbered-text))
       (nesting
         (document->graphics)
         (text->graphics))))))
