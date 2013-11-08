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

(def function make-test-projection/jjson ()
  (make-test-projection
   (nesting
     (widget->graphics)
     (sequential
       (nesting
         (document->document)
         (recursive (jjson->tree)))
       (nesting
         (document->document)
         (recursive (tree->styled-string)))
       (nesting
         (document->document)
         (styled-string->line-numbered-styled-string))
       (nesting
         (document->graphics)
         (styled-string->graphics))))))
