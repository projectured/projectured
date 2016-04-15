;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection chapter-numbering ()
  ())

;;;;;;
;;; Construction

(def function make-projection/chapter-numbering ()
  (make-projection 'chapter-numbering))

;;;;;;
;;; Construction

(def macro chapter-numbering ()
  '(make-projection/chapter-numbering))

;;;;;;
;;; Printer

(def printer chapter-numbering ()
  (bind ((element-iomaps (as (map-ll* (ll (elements-of -input-))
                                      (lambda (element index)
                                        (recurse-printer -recursion- (value-of element)
                                                         `((elt (the sequence document) ,index)
                                                           (the sequence (elements-of (the book/chapter document)))
                                                           ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output (make-book/chapter (map-ll (va element-iomaps) 'output-of)
                                    :title (as (title-of -input-))
                                    :collapsed (as (collapsed-p -input-))
                                    :numbering (as (reference-case -input-reference-
                                                     (((elt (the sequence document) ?subchapter-element-index)
                                                       (the sequence (elements-of (the book/chapter document)))
                                                       (the book/chapter (elt (the sequence document) ?chapter-element-index))
                                                       (the sequence (elements-of (the book/chapter document)))
                                                       (the book/chapter (elt (the sequence document) ?book-element-index))
                                                       (the sequence (elements-of (the book/book document)))
                                                       . ?rest)
                                                      (string+ (write-to-string (1+ ?book-element-index)) "." (write-to-string (1+ ?chapter-element-index)) "." (write-to-string (1+ ?subchapter-element-index))))
                                                     (((elt (the sequence document) ?chapter-element-index)
                                                       (the sequence (elements-of (the book/chapter document)))
                                                       (the book/chapter (elt (the sequence document) ?book-element-index))
                                                       (the sequence (elements-of (the book/book document)))
                                                       . ?rest)
                                                      (string+ (write-to-string (1+ ?book-element-index)) "." (write-to-string (1+ ?chapter-element-index))))
                                                     (((elt (the sequence document) ?index)
                                                       (the sequence (elements-of (the book/book document)))
                                                       . ?rest)
                                                      (write-to-string (1+ ?index)))
                                                     (()
                                                      "1")))
                                    :selection (as (get-selection -input-)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader chapter-numbering ()
  -input-)
