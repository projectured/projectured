;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection primitive/boolean->boolean ()
  ())

(def projection primitive/number->number ()
  ())

(def projection primitive/string->string ()
  ())

;;;;;;
;;; Construction

(def function make-projection/primitive/boolean->boolean ()
  (make-projection 'primitive/boolean->boolean))

(def function make-projection/primitive/number->number ()
  (make-projection 'primitive/number->number))

(def function make-projection/primitive/string->string ()
  (make-projection 'primitive/string->string))

;;;;;;
;;; Construction

(def macro primitive/boolean->boolean ()
  `(make-projection/primitive/boolean->boolean))

(def macro primitive/number->number ()
  `(make-projection/primitive/number->number))

(def macro primitive/string->string ()
  `(make-projection/primitive/string->string))

;;;;;;
;;; Forward mapper

(def forward-mapper primitive/number->number ()
  (reference-case -reference-
    (((the primitive/number document))
     '((the number document)))
    (((the number (value-of (the primitive/number document)))
      (the string (write-to-string (the number document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the string (write-to-string (the number document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))))

;;;;;;
;;; Backward mapper

(def backward-mapper primitive/number->number ()
  (reference-case -reference-
    (((the number document))
     '((the primitive/number document)))
    (((the string (write-to-string (the number document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the number (value-of (the primitive/number document)))
       (the string (write-to-string (the number document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))))

;;;;;;
;;; Printer

(def printer primitive/boolean->boolean ()
  (bind ((output (as (value-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer primitive/number->number ()
  (bind ((output (as (value-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer primitive/string->string ()
  (bind ((output (as (value-of -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader primitive/boolean->boolean ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader primitive/number->number ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the number (value-of (the primitive/number document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range selection (replacement-of operation))))
                                  (((the number (printer-output (the primitive/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range '((the number (value-of (the primitive/number document)))
                                                                            (the string (write-to-string (the number document)))
                                                                            (the string (subseq (the string document) 0 0)))
                                                                          (replacement-of operation))))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader primitive/string->string ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))
