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
                   (text/text () (text/string "" :font *font/liberation/serif/regular/24* :font-color (color/darken *color/solarized/blue* 0.5)))))
    ("picture" (book/picture (:selection '((the string (subseq (the string document) 0 0))
                                           (the string (filename-of (the image/image document)))
                                           (the image/image (content-of (the book/picture document)))))
                 (image/image () "")))))

(def function make-initial-projection ()
  (sequential
    (nesting
      (widget->graphics)
      (type-dispatching
        (widget/tooltip (nesting
                          (widget->graphics)
                          (sequential
                            (type-dispatching
                              (text/base (preserving))
                              (t (reference->text)))
                            (text->graphics))))
        (widget/scroll-pane (nesting
                              (widget->graphics)
                              (nesting
                                (document->t nil)
                                (document/clipboard->t)
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
                                  (word-wrapping 1280)
                                  (text->graphics)))))))
    (recursive
      (graphics->graphics))))
