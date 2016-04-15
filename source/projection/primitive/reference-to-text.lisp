;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/reference->text/text ()
  ((path-color :type style/color)
   (type-color :type style/color)
   (static-color :type style/color)))

;;;;;;
;;; Construction

(def function make-projection/document/reference->text/text (&key path-color type-color static-color)
  (make-projection 'document/reference->text/text
                   :path-color (or path-color *color/solarized/red*)
                   :type-color (or type-color *color/solarized/blue*)
                   :static-color (or static-color *color/black*)))

;;;;;;
;;; Construction

(def macro document/reference->text/text (&key path-color type-color static-color)
  `(make-projection/document/reference->text/text :path-color ,path-color :type-color ,type-color :static-color ,static-color))

;;;;;;
;;; Printer

(def printer document/reference->text/text ()
  (bind ((font *font/ubuntu/monospace/regular/18*)
         (static-font *font/ubuntu/regular/18*)
         (path-color (path-color-of -projection-))
         (type-color (type-color-of -projection-))
         (static-color (static-color-of -projection-)))
    (labels ((recurse (path)
               (reference-case path
                 (()
                  nil)
                 (((the ?a document) . ?rest)
                  (list* (text/string "The whole " :font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?d)) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string "POSITION" :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (text/string (write-to-string ?d) :font font :font-color path-color)
                         (text/string ". position of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/subseq subseq) (the ?b ?c) ?d ?e (?if (= ?d (1- ?e))))) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string "CHARACTER" :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (text/string (write-to-string ?d) :font font :font-color path-color)
                         (text/string ". element of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a ((?or text/elt elt) (the ?b ?c) ?d)) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (text/string (write-to-string ?d) :font font :font-color path-color)
                         (text/string ". element of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (printer-output (the ?b ?c) ?d ?e)) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color type-color)
                         (text/string " is the printer output of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (?b (the ?c ?d))) . ?rest)
                  (list* (text/string "The ":font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (bind ((name (symbol-name ?b)))
                           (text/string (if (ends-with-subseq "-OF" name)
                                            (subseq name 0 (- (length name) 3))
                                            name)
                                        :font font :font-color path-color))
                         (text/string " of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?c) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the ?a (slot-value (the ?b ?c) (quote ?d))) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (text/string (symbol-name ?d) :font font :font-color path-color)
                         (text/string " slot value of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (((the string (?a (the ?b ?c) ?d)) . ?rest)
                  (list* (text/string "The " :font static-font :font-color static-color)
                         (text/string "STRING" :font font :font-color type-color)
                         (text/string " is the " :font static-font :font-color static-color)
                         (text/string (symbol-name ?a) :font font :font-color path-color)
                         (text/string " of a " :font static-font :font-color static-color)
                         (text/string (symbol-name ?b) :font font :font-color type-color)
                         (text/string "." :font static-font :font-color static-color)
                         (text/newline :font font) (recurse ?rest)))
                 (?a
                  (warn "Unknown reference part ~A" path)
                  (list (text/string "Unknown reference part: " :font static-font :font-color static-color)
                        (text/string (write-to-string path) :font font :font-color type-color)
                        (text/newline :font font))))))
      (bind ((output (if (path-of -input-)
                         (text/make-text (butlast (recurse (reverse (coerce (path-of -input-) 'list)))))
                         (text/text ()
                           (text/string "The " :font static-font :font-color static-color)
                           (text/string (symbol-name (document-type -input-)) :font font :font-color type-color)
                           (text/string " document." :font static-font :font-color static-color)))))
        (make-iomap -projection- -recursion- -input- -input-reference- output)))))

;;;;;;
;;; Reader

(def reader document/reference->text/text ()
  -input-)
