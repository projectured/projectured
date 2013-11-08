;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) list->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/list->string ()
  (make-projection 'list->string))

;;;;;;
;;; Construction

(def (macro e) list->string ()
  '(make-projection/list->string))

;;;;;;
;;; Printer

(def printer list->string (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (width (list/width input))
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (iter (for element-index :from 0)
                            (for element :in-sequence (elements-of input))
                            (for content = (content-of element))
                            (for element-reference = `(elt (the list (elements-of (the list/list ,input-reference))) ,element-index))
                            (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                      output `(the string ,output-reference) (file-position stream)
                                                      (+ 2 width))
                                  child-iomaps)
                            (if (first-iteration-p)
                                (progn
                                  (write-char #\U250C stream)
                                  (iter (repeat width)
                                        (write-char #\U2500 stream))
                                  (write-char #\U2510 stream))
                                (progn
                                  (write-char #\U251C stream)
                                  (iter (repeat width)
                                        (write-char #\U2500 stream))
                                  (write-char #\U2524 stream)))
                            (terpri stream)
                            (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                      output `(the string ,output-reference) (file-position stream)
                                                      1)
                                  child-iomaps)
                            (write-char #\U2502 stream)
                            (push (make-iomap/string content `(content-of (the ,(form-type element) ,element-reference)) 0
                                                     output output-reference (file-position stream)
                                                     (length content))
                                  child-iomaps)
                            (write-string content stream)
                            (write-string (make-string-of-spaces (- width (length content))) stream)
                            (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                      output `(the string ,output-reference) (file-position stream)
                                                      1)
                                  child-iomaps)
                            (write-char #\U2502 stream)
                            (terpri stream))
                      (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                output `(the string ,output-reference) (file-position stream)
                                                (+ 2 width))
                            child-iomaps)
                      (write-char #\U2514 stream)
                      (iter (repeat width)
                            (write-char #\U2500 stream))
                      (write-char #\U2518 stream))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader list->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
