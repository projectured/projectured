;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/null->tree/leaf ()
  ())

(def projection t/number->tree/leaf ()
  ())

(def projection t/string->tree/leaf ()
  ())

(def projection t/symbol->tree/leaf ()
  ())

(def projection t/pathname->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def function make-projection/t/null->tree/leaf ()
  (make-projection 't/null->tree/leaf))

(def function make-projection/t/number->tree/leaf ()
  (make-projection 't/number->tree/leaf))

(def function make-projection/t/string->tree/leaf ()
  (make-projection 't/string->tree/leaf))

(def function make-projection/t/symbol->tree/leaf ()
  (make-projection 't/symbol->tree/leaf))

(def function make-projection/t/pathname->tree/leaf ()
  (make-projection 't/pathname->tree/leaf))

;;;;;;
;;; Construction

(def macro t/null->tree/leaf ()
  '(make-projection/t/null->tree/leaf))

(def macro t/number->tree/leaf ()
  '(make-projection/t/number->tree/leaf))

(def macro t/string->tree/leaf ()
  '(make-projection/t/string->tree/leaf))

(def macro t/symbol->tree/leaf ()
  '(make-projection/t/symbol->tree/leaf))

(def macro t/pathname->tree/leaf ()
  '(make-projection/t/pathname->tree/leaf))

;;;;;;
;;; Printer

(def printer t/null->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf ()
                   (text/make-simple-text "NIL" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/number->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf ()
                   (text/make-default-text (write-to-string input) "enter number" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/string->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf (:opening-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                             :closing-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                   (text/make-default-text input "enter string" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer t/symbol->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf ()
                   (text/make-default-text (write-to-string input) "enter symbol" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/pathname->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf (:opening-delimiter (text/make-simple-text "#P\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                             :closing-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                   (text/make-default-text (princ-to-string input) "enter path" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader t/null->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/number->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/symbol->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/pathname->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))
