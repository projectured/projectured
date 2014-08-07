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

(def function make-projection/reference->text ()
  (make-projection 'reference->text))

;;;;;;
;;; Construction

(def macro reference->text ()
  '(make-projection/reference->text))

;;;;;;
;;; Printer

(def printer reference->text (projection recursion input input-reference)
  (labels ((recurse (input)
             (pattern-case input
               (()
                (list (text/string "")))
               (((the ?a document) . ?rest)
                (list* (text/string "The whole " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?d)) . ?rest)
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string "POSITION" :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (write-to-string ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string "th position of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?e (?if (= ?d (1- ?e))))) . ?rest)
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string "CHARACTER" :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (write-to-string ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string "th element of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a ((?or text/elt elt) (the ?b ?c) ?d)) . ?rest)
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (write-to-string ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string "th element of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a (printer-output (the ?b ?c) ?d ?e)) . ?rest)
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the printer output of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a (?b (the ?c ?d))) . ?rest)
                (list* (text/string "The ":font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (bind ((name (symbol-name ?b)))
                         (text/string (if (ends-with-subseq "-OF" name)
                                          (subseq name 0 (- (length name) 3))
                                          name)
                                      :font *font/default* :font-color *color/solarized/red*))
                       (text/string " of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?c) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the ?a (slot-value (the ?b ?c) (quote ?d))) . ?rest)
                (list* (text/string "The " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string " is the " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?d) :font *font/default* :font-color *color/solarized/red*)
                       (text/string " slot value of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (((the string (?a (the ?b ?c) ?d)) . ?rest)
                (list* (text/string "The STRING is the " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?a) :font *font/default* :font-color *color/solarized/red*)
                       (text/string " of a " :font *font/default* :font-color *color/black*)
                       (text/string (symbol-name ?b) :font *font/default* :font-color *color/solarized/blue*)
                       (text/string "." :font *font/default* :font-color *color/black*)
                       (text/newline) (recurse ?rest)))
               (?a
                (warn "Unknown reference part ~A" input)
                (list (text/string "Unknown reference part: " :font *font/default* :font-color *color/black*)
                      (text/string (write-to-string input) :font *font/default* :font-color *color/solarized/blue*))))))
    (bind ((output (make-text/text (recurse input))))
      (make-iomap/object projection recursion input input-reference output))))

;;;;;;
;;; Reader

(def reader reference->text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
