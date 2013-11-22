;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection reference->text ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/reference->text ()
  (make-projection 'reference->text))

;;;;;;
;;; Construction

(def (macro e) reference->text ()
  '(make-projection/reference->text))

;;;;;;
;;; Printer

(def printer reference->text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (labels ((recurse (input)
             (pattern-case input
               ((the ?a document)
                (list (text/string "The " :font *font/default* :font-color *color/black*)
                      (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                      (text/string " is the document that is being edited." :font *font/default* :font-color *color/black*)))
               ((the ?a ((?or text/elt elt) (the ?b ?c) ?d))
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (write-to-string ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string "th element of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline)
                       (recurse `(the ,?b ,?c))))
               ((the ?a (printer-output (the ?b ?c) ?d ?e))
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the printer output of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse `(the ,?b ,?c))))
               ((the ?a (?b (the ?c ?d)))
                (list* (text/string "The ":font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/red*)
                       (text/string " of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?c) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse `(the ,?c ,?d))))
               ((the ?a (slot-value (the ?b ?c) (quote ?d)))
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string " slot value of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline)
                       (recurse `(the ,?b ,?c))))
               ((the string (?a (the ?b ?c) ?d))
                (list* (text/string "The STRING is the " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/red*)
                       (text/string " of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline)
                       (recurse `(the ,?b ,?c))))
               (?a
                (warn "Unknown reference part ~A" input)
                (list (text/string "Unknown" :font *font/default* :font-color *color/black*))))))
    (bind ((output (make-text/text (recurse input))))
      (make-iomap/object projection recursion input input-reference output output-reference))))

;;;;;;
;;; Reader

(def reader reference->text (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
