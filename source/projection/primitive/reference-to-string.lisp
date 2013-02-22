;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) reference->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/reference->string ()
  (make-projection 'reference->string))

;;;;;;
;;; Construction

(def (macro e) reference->string ()
  '(make-projection/reference->string))

;;;;;;
;;; Printer

(def printer reference->string (projection recursion iomap input input-reference output-reference)
  (labels ((recurse (input)
             (pattern-case input
               ((the ?a document)
                (format nil "The ~A is the document being edited." ?a))
               ((the ?a (elt (the ?b ?c) ?d))
                (string+ (format nil "The ~A is the ~Ath element of a ~A." ?a ?d ?b) (string #\NewLine) (recurse `(the ,?b ,?c))))
               ((the ?a (printer-output (the ?b ?c) ?d ?e))
                (string+ (format nil "The ~A is the printer output of a ~A." ?a ?B) (string #\NewLine) (recurse `(the ,?b ,?c))))
               ((the ?a (?b (the ?c ?d)))
                (string+ (format nil "The ~A is the ~A of a ~A." ?a ?b ?c) (string #\NewLine) (recurse `(the ,?c ,?d))))
               ((the ?a (slot-value (the ?b ?c) (quote ?d)))
                (string+ (format nil "The ~A is the ~A slot value of a ~A." ?a ?d ?b) (string #\NewLine) (recurse `(the ,?b ,?c))))
               ((the string (?a (the ?b ?c) ?d))
                (string+ (format nil "The STRING is the ~A of a ~A." ?a ?b) (string #\NewLine) (recurse `(the ,?b ,?c))))
               (?a
                (warn "Unknown reference part ~A" input)))))
    (bind ((output (recurse input)))
      (make-iomap/object projection recursion input input-reference output output-reference))))

;;;;;;
;;; Reader

(def reader reference->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
