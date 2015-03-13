;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/null->text/text ()
  ())

(def projection t/number->text/text ()
  ())

(def projection t/string->text/text ()
  ())

(def projection t/symbol->text/text ()
  ())

(def projection t/pathname->text/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/t/null->text/text ()
  (make-projection 't/null->text/text))

(def function make-projection/t/number->text/text ()
  (make-projection 't/number->text/text))

(def function make-projection/t/string->text/text ()
  (make-projection 't/string->text/text))

(def function make-projection/t/symbol->text/text ()
  (make-projection 't/symbol->text/text))

(def function make-projection/t/pathname->text/text ()
  (make-projection 't/pathname->text/text))

;;;;;;
;;; Construction

(def macro t/null->text/text ()
  '(make-projection/t/null->text/text))

(def macro t/number->text/text ()
  '(make-projection/t/number->text/text))

(def macro t/string->text/text ()
  '(make-projection/t/string->text/text))

(def macro t/symbol->text/text ()
  '(make-projection/t/symbol->text/text))

(def macro t/pathname->text/text ()
  '(make-projection/t/pathname->text/text))

;;;;;;
;;; Printer

(def printer t/null->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string "NIL" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/number->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string (write-to-string input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/string->text/text (projection recursion input input-reference)
  (bind ((output (text/text ()
                   (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                   (text/string input :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)
                   (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer t/symbol->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string (symbol-name input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/pathname->text/text (projection recursion input input-reference)
  (bind ((output (text/text ()
                   (text/string "#P\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                   (text/string (princ-to-string input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)
                   (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader t/null->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/number->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/string->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/symbol->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))

(def reader t/pathname->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  (make-command/nothing (gesture-of input)))
