;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/reference->text/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/reference->text/text ()
  (make-projection 'document/reference->text/text))

;;;;;;
;;; Construction

(def macro document/reference->text/text ()
  '(make-projection/document/reference->text/text))

;;;;;;
;;; Printer

(def printer document/reference->text/text (projection recursion input input-reference)
  (bind ((font *font/ubuntu/monospace/regular/18*))
    (labels ((recurse (path)
               (pattern-case path
                 (()
                  nil)
                 (((the ?a document) . ?rest)
                  (list* (text/string "The whole " :font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?d)) . ?rest)
                  (list* (text/string "The " :font font :font-color *color/black*)
                         (text/string "POSITION" :font font :font-color *color/solarized/blue*)
                         (text/string " is the " :font font :font-color *color/black*)
                         (text/string (write-to-string ?d) :font font :font-color *color/solarized/red*)
                         (text/string "th position of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?e (?if (= ?d (1- ?e))))) . ?rest)
                  (list* (text/string "The " :font font :font-color *color/black*)
                         (text/string "CHARACTER" :font font :font-color *color/solarized/blue*)
                         (text/string " is the " :font font :font-color *color/black*)
                         (text/string (write-to-string ?d) :font font :font-color *color/solarized/red*)
                         (text/string "th element of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/elt elt) (the ?b ?c) ?d)) . ?rest)
                  (list* (text/string "The " :font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/blue*)
                         (text/string " is the " :font font :font-color *color/black*)
                         (text/string (write-to-string ?d) :font font :font-color *color/solarized/red*)
                         (text/string "th element of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (printer-output (the ?b ?c) ?d ?e)) . ?rest)
                  (list* (text/string "The " :font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/blue*)
                         (text/string " is the printer output of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (?b (the ?c ?d))) . ?rest)
                  (list* (text/string "The ":font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/blue*)
                         (text/string " is the " :font font :font-color *color/black*)
                         (bind ((name (symbol-name ?b)))
                           (text/string (if (ends-with-subseq "-OF" name)
                                            (subseq name 0 (- (length name) 3))
                                            name)
                                        :font font :font-color *color/solarized/red*))
                         (text/string " of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?c) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (slot-value (the ?b ?c) (quote ?d))) . ?rest)
                  (list* (text/string "The " :font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/blue*)
                         (text/string " is the " :font font :font-color *color/black*)
                         (text/string (symbol-name ?d) :font font :font-color *color/solarized/red*)
                         (text/string " slot value of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (((the string (?a (the ?b ?c) ?d)) . ?rest)
                  (list* (text/string "The STRING is the " :font font :font-color *color/black*)
                         (text/string (symbol-name ?a) :font font :font-color *color/solarized/red*)
                         (text/string " of a " :font font :font-color *color/black*)
                         (text/string (symbol-name ?b) :font font :font-color *color/solarized/blue*)
                         (text/string "." :font font :font-color *color/black*)
                         (text/newline :font font) (recurse ?rest)))
                 (?a
                  (warn "Unknown reference part ~A" path)
                  (list (text/string "Unknown reference part: " :font font :font-color *color/black*)
                        (text/string (write-to-string path) :font font :font-color *color/solarized/blue*)
                        (text/newline :font font))))))
      (bind ((output (text/make-text (butlast (recurse (reverse (path-of input)))))))
        (make-iomap projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader document/reference->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
