;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def function initial-factory (name)
  (completion-prefix-switch name
    ("book" (book/book (:selection '((the string (subseq (the string document) 0 0))
                                     (the string (title-of (the book/book document)))))))
    ("chapter" (book/chapter (:selection '((the string (subseq (the string document) 0 0))
                                           (the string (title-of (the book/chapter document)))))))
    ("paragraph" (book/paragraph (:selection '((the text/text (text/subseq (the text/text document) 0 0))
                                               (the text/text (content-of (the book/paragraph document)))))
                   (text/text ()
                     (text/string "" :font *font/liberation/serif/regular/24* :font-color (color/darken *color/solarized/blue* 0.5)))))
    ("picture" (book/picture (:selection '((the string (subseq (the string document) 0 0))
                                           (the string (filename-of (the image/image document)))
                                           (the image/image (content-of (the book/picture document)))))
                 (image/image () "")))
    ("xml element" (xml/element ("" nil :selection '((the string (subseq (the string document) 0 0))
                                                     (the string (xml/start-tag (the xml/element document)))))))
    ("xml attribute" (xml/attribute (:selection '((the string (subseq (the string document) 0 0))
                                                  (the string (name-of (the xml/attribute document))))) "" ""))
    ("xml text" (xml/text (:selection '((the string (subseq (the string document) 0 0))
                                        (the string (value-of (the xml/text document))))) ""))
    ("css rule" (css/rule ("" :selection '((the string (subseq (the string document) 0 0))
                                           (the string (selector-of (the css/rule document)))))))
    ("css attribute" (css/attribute (:selection '((the string (subseq (the string document) 0 0))
                                                  (the string (name-of (the css/attribute document))))) "" ""))
    ("json null" (json/null ()))
    ("json false" (json/boolean () #f))
    ("json true" (json/boolean ()#t))
    ("json number" (json/number () 0))
    ("json string" (json/string () ""))
    ("json array" (json/array (:selection '((the string (subseq (the string document) 0 0))
                                            (the string (value-of (the json/nothing document)))
                                            (the json/nothing (elt (the sequence document) 0))
                                            (the sequence (elements-of (the json/array document)))))
                    (json/nothing (:selection '((the string (subseq (the string document) 0 0))
                                                (the string (value-of (the json/nothing document))))))))
    ("json object entry" (json/object-entry (:selection '((the string (subseq (the string document) 0 0))
                                                          (the string (key-of (the json/object-entry document))))) ""
                                                          (json/nothing ())))
    ("json object" (make-json/object (make-sequence/sequence (list (json/nothing (:selection '((the string (subseq (the string document) 0 0))
                                                                                               (the string (value-of (the json/nothing document)))))))
                                                             :selection '((the string (subseq (the string document) 0 0))
                                                                          (the string (value-of (the json/nothing document)))
                                                                          (the json/nothing (elt (the sequence document) 0))))
                                     :selection '((the string (subseq (the string document) 0 0))
                                                  (the string (value-of (the json/nothing document)))
                                                  (the json/nothing (elt (the sequence document) 0))
                                                  (the sequence (entries-of (the json/object document))))))))


(def function make-initial-projection ()
  (sequential
    (nesting
      (widget->graphics)
      (type-dispatching
        (widget/tooltip
          (nesting
            (widget->graphics)
            (sequential
              (type-dispatching
                (text/base (preserving))
                (t (reference->text)))
              (text->graphics))))
        (widget/split-pane
          (nesting
            (widget->graphics)
            (type-dispatching
              (widget/tabbed-pane
                (nesting
                  (widget->graphics)
                  (type-dispatching
                    (widget/label
                      (recursive
                        (type-dispatching
                          (widget/base (widget/label->graphics/canvas))
                          (text/base (text->graphics)))))
                    (widget/scroll-pane
                      (nesting
                       (widget->graphics)
                       (document/document->t)
                       (sequential
                         (recursive
                           (type-dispatching
                             (book/base (book->tree))
                             (xml/base (xml->tree))
                             (json/base (json->tree))
                             (document/base (document->t 'initial-factory))
                             (text/text (preserving))))
                         (recursive
                           (type-dispatching
                             (tree/base (tree->text))
                             (text/text (preserving))))
                         (word-wrapping 1075)
                         (text->graphics)))))))
              (widget/scroll-pane
                (nesting
                  (widget->graphics)
                  (sequential
                    (recursive (file-system->tree))
                    (recursive (type-dispatching
                                 (tree/base (tree->text))
                                 (text/text (preserving))))
                    (text->graphics)))))))))
    (recursive
      (graphics->graphics))))
