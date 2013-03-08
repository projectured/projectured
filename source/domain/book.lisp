;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Document

(def document book/base ()
  ())

(def document book/book (book/base)
  ((title :type string)
   (authors :type sequence)
   (elements :type sequence)))

(def document book/chapter (book/base)
  ((title :type string)
   (elements :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-book/book (elements &key title authors)
  (make-instance 'book/book :title title :authors authors :elements elements))

(def (function e) make-book/chapter (elements &key title)
  (make-instance 'book/chapter :title title :elements elements))

;;;;;;
;;; Construction

(def (macro e) book ((&key title authors) &body elements)
  `(make-book/book (list ,@elements) :title ,title :authors ,authors))

(def (macro e) chapter ((&key title) &body elements)
  `(make-book/chapter (list ,@elements) :title ,title))

;;;;;;
;;; Provider

(def (function e) book-font-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (title-of (the book/book ?a))) ?b))
                     (return-from book-font-color-provider
                       (make-style/color 255 196 0 0)))
                    ((the character (elt (the string (title-of (the book/chapter ?a))) ?b))
                     (return-from book-font-color-provider
                       (make-style/color 255 0 0 196)))
                    ((the character (elt (the string (elt (the list (elements-of (the book/chapter ?a))) ?b)) ?c))
                     (return-from book-font-color-provider
                       (make-style/color 255 0 0 0)))))))

(def (function e) book-font-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (title-of (the book/book ?a))) ?b))
                     (return-from book-font-provider
                       *font/ubuntu/bold/36*))
                    ((the character (elt (the string (title-of (the book/chapter ?a))) ?b))
                     (return-from book-font-provider
                       *font/ubuntu/bold/24*))
                    ((the character (elt (the string (elt (the list (elements-of (the book/chapter ?a))) ?b)) ?c))
                     (return-from book-font-provider
                       *font/ubuntu/regular/18*))))))

(def (function e) book-delimiter-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the string (elt (the list (elements-of (the book/chapter ?a))) ?b))
                     (return-from book-delimiter-provider ""))))))

(def (function e) book-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore next-child-reference))
  (map-backward iomap previous-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the string (elt (the list (elements-of (the book/chapter ?a))) ?b))
                     (return-from book-separator-provider ""))))))

(def (function e) book-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore next-child-reference parent-node))
  (map-backward iomap previous-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ;; new line after title
                    ((the string (title-of (the (?or book/book book/chapter) ?a)))
                     (return-from book-indentation-provider 1))
                    ;; new line after paragraph
                    ((the ?a (elt (the list (elements-of (the book/chapter ?b))) ?c))
                     (return-from book-indentation-provider 1))))))
