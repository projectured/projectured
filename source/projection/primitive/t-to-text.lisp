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

(def (function e) make-projection/t/null->text/text ()
  (make-projection 't/null->text/text))

(def (function e) make-projection/t/number->text/text ()
  (make-projection 't/number->text/text))

(def (function e) make-projection/t/string->text/text ()
  (make-projection 't/string->text/text))

(def (function e) make-projection/t/symbol->text/text ()
  (make-projection 't/symbol->text/text))

(def (function e) make-projection/t/pathname->text/text ()
  (make-projection 't/pathname->text/text))

;;;;;;
;;; Construction

(def (macro e) t/null->text/text ()
  '(make-projection/t/null->text/text))

(def (macro e) t/number->text/text ()
  '(make-projection/t/number->text/text))

(def (macro e) t/string->text/text ()
  '(make-projection/t/string->text/text))

(def (macro e) t/symbol->text/text ()
  '(make-projection/t/symbol->text/text))

(def (macro e) t/pathname->text/text ()
  '(make-projection/t/pathname->text/text))

;;;;;;
;;; Printer

(def printer t/null->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string "NIL" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/number->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string (write-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/string->text/text (projection recursion input input-reference)
  (bind ((output (text/text ()
                   (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                   (text/string input :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                   (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer t/symbol->text/text (projection recursion input input-reference)
  (bind ((output (text/text () (text/string (symbol-name input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer t/pathname->text/text (projection recursion input input-reference)
  (bind ((output (text/text ()
                   (text/string "#P\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                   (text/string (princ-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                   (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
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
