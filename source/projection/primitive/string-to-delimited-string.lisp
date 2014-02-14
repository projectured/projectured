;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection string->delimited-string ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)))

;;;;;;
;;; Construction

(def (function e) make-projection/string->delimited-string (opening-delimiter closing-delimiter)
  (make-projection 'string->delimited-string
                   :opening-delimiter opening-delimiter
                   :closing-delimiter closing-delimiter))

;;;;;;
;;; Construction

(def (macro e) string->delimited-string (opening-delimiter closing-delimiter)
  `(make-projection/string->delimited-string ,opening-delimiter ,closing-delimiter))

;;;;;;
;;; Printer

(def printer string->delimited-string (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (opening-delimiter (opening-delimiter-of projection))
         (closing-delimiter (closing-delimiter-of projection))
         (output (string+ opening-delimiter input closing-delimiter)))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader string->delimited-string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
