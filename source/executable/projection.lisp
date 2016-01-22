;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def function default-factory (name)
  (completion-prefix-switch name
    ("book" (book/book (:selection '((the string (title-of (the book/book document)))
                                     (the string (subseq (the string document) 0 0))))))
    ("chapter" (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                           (the string (subseq (the string document) 0 0))))))
    ("paragraph" (book/paragraph (:alignment :justified
                                             :selection '((the text/text (content-of (the book/paragraph document)))
                                                          (the text/text (text/subseq (the text/text document) 0 0))))
                   (text/text () (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))
    ("list" (book/list (:selection '((the sequence (elements-of (the book/list document)))
                                     (the book/paragraph (elt (the sequence document) 0))
                                     (the text/text (content-of (the book/paragraph document)))
                                     (the text/text (text/subseq (the text/text document) 0 0))))
              (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                            (the text/text (text/subseq (the text/text document) 0 0))))
                (text/text () (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*)))))
    ("picture" (book/picture (:selection '((the image/file (content-of (the book/picture document)))
                                           (the string (filename-of (the image/file document)))
                                           (the string (subseq (the string document) 0 0))))
                 (image/file () "")))
    ("text" (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0)))) (text/string "")))
    ("xml element" (xml/element ("" nil :selection '((the string (xml/start-tag (the xml/element document)))
                                                     (the string (subseq (the string document) 0 0))))))
    ("xml attribute" (xml/attribute (:selection '((the string (name-of (the xml/attribute document)))
                                                  (the string (subseq (the string document) 0 0)))) "" ""))
    ("xml text" (xml/text (:selection '((the string (value-of (the xml/text document)))
                                        (the string (subseq (the string document) 0 0)))) ""))
    ("json null" (json/null ()))
    ("json false" (json/boolean () #f))
    ("json true" (json/boolean ()#t))
    ("json number" (json/number () 0))
    ("json string" (json/string () ""))
    ("json array" (json/array (:selection '((the sequence (elements-of (the json/array document)))
                                            (the json/insertion (elt (the sequence document) 0))
                                            (the string (value-of (the json/insertion document)))
                                            (the string (subseq (the string document) 0 0))))
                    (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                  (the string (subseq (the string document) 0 0)))))))
    ("json object entry" (json/object-entry (:selection '((the string (key-of (the json/object-entry document)))
                                                          (the string (subseq (the string document) 0 0))))
                                            ""
                                            (json/insertion ())))
    ("json object" (make-json/object (list (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                                         (the string (subseq (the string document) 0 0))))))
                                     :selection '((the sequence (entries-of (the json/object document)))
                                                  (the json/insertion (elt (the sequence document) 0))
                                                  (the string (value-of (the json/insertion document)))
                                                  (the string (subseq (the string document) 0 0)))))
    ("common lisp function definition" (make-common-lisp/function-definition (make-lisp-form/symbol "" "") nil nil
                                                                             :documentation ""
                                                                             :selection '((the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
                                                                                          (the string (name-of (the lisp-form/symbol document)))
                                                                                          (the string (subseq (the string document) 0 0)))))
    ("common lisp if" (make-common-lisp/if (make-common-lisp/insertion "" 'common-lisp/complete-document) (make-common-lisp/insertion "" 'common-lisp/complete-document) (make-common-lisp/insertion "" 'common-lisp/complete-document)))
    ("common lisp progn" (make-common-lisp/progn nil))
    ("common lisp let" (make-common-lisp/let nil nil))
    ("common lisp lambda" (make-common-lisp/lambda-function nil nil))
    ("evaluator toplevel" (evaluator/toplevel (:selection '((the sequence (elements-of (the evaluator/toplevel document)))
                                                            (the evaluator/form (elt (the sequence document) 0))
                                                            (the common-lisp/insertion (form-of (the evaluator/form document)))
                                                            (the string (value-of (the common-lisp/insertion document)))
                                                            (the string (subseq (the string document) 0 0))))
                            (evaluator/form (:selection '((the common-lisp/insertion (form-of (the evaluator/form document)))
                                                          (the string (value-of (the common-lisp/insertion document)))
                                                          (the string (subseq (the string document) 0 0))))
                              (make-common-lisp/insertion "" 'common-lisp/complete-document
                                                          :default-value "enter form"
                                                          :selection '((the string (value-of (the common-lisp/insertion document)))
                                                                       (the string (subseq (the string document) 0 0)))))))
    ("evaluator form" (evaluator/form (:selection '((the common-lisp/insertion (form-of (the evaluator/form document)))
                                                    (the string (value-of (the common-lisp/insertion document)))
                                                    (the string (subseq (the string document) 0 0))))
                        (make-common-lisp/insertion "" 'common-lisp/complete-document
                                                    :default-value "enter form"
                                                    :selection '((the string (value-of (the common-lisp/insertion document)))
                                                                 (the string (subseq (the string document) 0 0))))))))

(def function default-slot-provider (instance)
  (remove-if (lambda (slot) (member (slot-definition-name slot)
                               '(raw selection annotation reader printer part-evaluator)))
             (class-slots (class-of instance))))

(def function make-default-projection ()
  (recursive
    (type-dispatching
      (widget/base (widget->graphics))
      (document/document (document/document->t))
      (document/clipboard (document/clipboard->t))
      (document/reflection (document/reflection->graphics/canvas))
      (document
       (type-dispatching
         (graphics/base (preserving))
         (text/text (sequential
                      (word-wrapping 1280)
                      (text->graphics)))
         (document/reference (sequential
                               (document/reference->text/text)
                               (text->graphics)))
         (help/context-sensitive (sequential
                                   (help/context-sensitive->text/text)
                                   (text->graphics)))
         (document
          (sequential
            (recursive
              (type-dispatching
                (book/book (copying))
                (book/chapter (chapter-numbering))
                (sequence (copying))
                (t (preserving))))
            (focusing 'document nil)
            (recursive
              (reference-dispatching
                (alternative
                  (type-dispatching
                    (document/base (document->tree 'default-factory))
                    (document/sequence (copying))
                    (book/paragraph (nesting
                                      (book/paragraph->tree/leaf)
                                      (text-aligning 1270)))
                    (book/base (book->tree))
                    (json/base (json->tree))
                    (xml/base (xml->tree))
                    (common-lisp/base (sequential
                                        (common-lisp->lisp-form)
                                        (lisp-form->tree)))
                    (lisp-form/base (lisp-form->tree))
                    (evaluator/base (evaluator->tree))
                    (tree/base (preserving))
                    (text/base (word-wrapping 1270)))
                  (recursive
                    (t->tree 'default-slot-provider)))))
            (recursive
              (type-dispatching
                (tree/base (tree->text))
                (text/text (preserving))))
            (text->graphics))))))))
