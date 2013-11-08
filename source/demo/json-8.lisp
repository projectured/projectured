;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; STEP 8
;;;;
;;;; Goal:
;;;;  - edit through sorting and filtering document
;;;;
;;;; Implementation:

(in-package :projectured.test)

(def function make-test-content/json ()
  (text/text ()
    (text/string "Hello World" :font *font/default* :font-color *color/solarized/blue*)
    (text/newline)
    (json/array
      (json/object
        ("name" (json/string "John Doe"))
        ("age" (json/number 42))
        ("sex" (json/string "Male"))
        ("married" (json/boolean #f)))
      (json/object
        ("name" (json/string "Bloody Mary"))
        ("age" (json/number 26))
        ("sex" (json/string "Female"))
        ("married" (json/boolean #t))))))

(def function make-test-projection/json->graphics ()
  (test-projection
    (nesting
      (widget->graphics)
      (alternative
        (sequential
          (nesting
            (document->document)
            (recursive (type-dispatching
                         (text/base (text->tree))
                         (json/base (json->tree)))))
          (nesting
            (document->document)
            (recursive (tree->styled-string)))
          (nesting
            (document->document)
            (styled-string->line-numbered-styled-string))
          (nesting
            (document->graphics)
            (make-test-projection/styled-string->output)))
        (nesting
          (document->graphics)
          (sequential
            (recursive (json->tree))
            (tree->graphics)))))))
