;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection json/null->common-lisp/application ()
  ())

(def projection json/boolean->common-lisp/application ()
  ())

(def projection json/number->common-lisp/application ()
  ())

(def projection json/string->common-lisp/application ()
  ())

(def projection json/array->common-lisp/progn ()
  ())

(def projection json/object-entry->common-lisp/progn ()
  ())

(def projection json/object->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def function make-projection/json/null->common-lisp/application ()
  (make-projection 'json/null->common-lisp/application))

(def function make-projection/json/boolean->common-lisp/application ()
  (make-projection 'json/boolean->common-lisp/application))

(def function make-projection/json/number->common-lisp/application ()
  (make-projection 'json/number->common-lisp/application))

(def function make-projection/json/string->common-lisp/application ()
  (make-projection 'json/string->common-lisp/application))

(def function make-projection/json/array->common-lisp/progn ()
  (make-projection 'json/array->common-lisp/progn))

(def function make-projection/json/object-entry->common-lisp/progn ()
  (make-projection 'json/object-entry->common-lisp/progn))

(def function make-projection/json/object->common-lisp/progn ()
  (make-projection 'json/object->common-lisp/progn))

;;;;;;
;;; Construction

(def macro json/null->common-lisp/application ()
  '(make-projection/json/null->common-lisp/application))

(def macro json/boolean->common-lisp/application ()
  '(make-projection/json/boolean->common-lisp/application))

(def macro json/number->common-lisp/application ()
  '(make-projection/json/number->common-lisp/application))

(def macro json/string->common-lisp/application ()
  '(make-projection/json/string->common-lisp/application))

(def macro json/array->common-lisp/progn ()
  '(make-projection/json/array->common-lisp/progn))

(def macro json/object-entry->common-lisp/progn ()
  '(make-projection/json/object-entry->common-lisp/progn))

(def macro json/object->common-lisp/progn ()
  '(make-projection/json/object->common-lisp/progn))

;;;;;;
;;; Printer

(def printer json/null->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                               (list (make-common-lisp/constant (value-of input))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/boolean->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                               (list (make-common-lisp/constant
                                                      (if (value-p input)
                                                          (true-value-of input)
                                                          (false-value-of input)))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/number->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (write-to-string (value-of input)))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/string->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ "\"" (value-of input) "\""))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/array->common-lisp/progn (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for element-index :from 0)
                               (for element :in-sequence (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,element-index)
                                                                       (the sequence (elements-of (the json/array document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               (collect element-iomap)))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "["))))
                                                 (iter (for element-iomap :in element-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant ", ")))))
                                                       (collect (output-of element-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "]"))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/object-entry->common-lisp/progn (projection recursion input input-reference)
  (bind ((value-iomap (recurse-printer recursion (value-of input)
                                       `((value-of (the json/object-entry document))
                                         ,@(typed-reference (form-type input) input-reference))))
         (output (make-common-lisp/progn (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ "\"" (key-of input) "\""))))
                                               (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant " : ")))
                                               (output-of value-iomap)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer json/object->common-lisp/progn (projection recursion input input-reference)
  (bind ((entry-iomaps (iter (for index :from 0)
                             (for entry :in-sequence (entries-of input))
                             (for entry-iomap = (recurse-printer recursion entry
                                                                 `((elt (the sequence (entries-of document)) ,index)
                                                                   ,@(typed-reference (form-type input) input-reference))))
                             (collect entry-iomap)))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "{"))))
                                                 (iter (for entry-iomap :in entry-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant ", ")))))
                                                       (collect (output-of entry-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "}"))))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader json/null->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/boolean->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/number->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/string->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/array->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/object-entry->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader json/object->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
