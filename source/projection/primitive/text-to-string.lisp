;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) text->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text->string ()
  (make-projection 'text->string))

;;;;;;
;;; Construction

(def (macro e) text->string ()
  '(make-projection/text->string))

;;;;;;
;;; Printer

(def printer text->string (projection recursion input input-reference output-reference)
  (bind ((child-iomaps nil)
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (iter (for document-element-index :from 0)
                            (for document-element :in-sequence (elements-of input))
                            (for document-element-reference = `(elt (the list (elements-of (the ,(form-type input) ,input-reference))) ,document-element-index))
                            (iter (for paragraph-element-index :from 0)
                                  (for paragraph-element :in-sequence (elements-of document-element))
                                  (for paragraph-element-reference = `(elt (the list (elements-of (the ,(form-type document-element) ,document-element-reference))) ,paragraph-element-index))
                                  (for content = (content-of paragraph-element))
                                  (push (make-iomap/string content `(content-of (the ,(form-type paragraph-element) ,paragraph-element-reference)) 0
                                                           output output-reference (file-position stream)
                                                           (length content))
                                        child-iomaps)
                                  (write-string content stream))
                            (terpri stream)))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader text->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
