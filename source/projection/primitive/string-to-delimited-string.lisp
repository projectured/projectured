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

(def printer string->delimited-string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (opening-delimiter (opening-delimiter-of projection))
         (closing-delimiter (closing-delimiter-of projection))
         (output (string+ opening-delimiter input closing-delimiter)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* opening-delimiter `(the string (opening-delimiter ,typed-input-reference (opening-delimiter-of ,projection))) 0
                                                    output `(the string ,output-reference) 0
                                                    (length opening-delimiter))
                                (make-iomap/string input input-reference 0
                                                   output output-reference (length opening-delimiter)
                                                   (length input))
                                (make-iomap/string* closing-delimiter `(the string (closing-delimiter ,typed-input-reference (closing-delimiter-of ,projection))) 0
                                                    output `(the string ,output-reference) (+ (length opening-delimiter) (length input))
                                                    (length closing-delimiter))))))

;;;;;;
;;; Reader

(def reader string->delimited-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
