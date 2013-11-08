;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) string->line-numbered-string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/string->line-numbered-string ()
  (make-projection 'string->line-numbered-string))

;;;;;;
;;; Construction

(def (macro e) string->line-numbered-string ()
  '(make-projection/string->line-numbered-string))

;;;;;;
;;; Printer

(def printer string->line-numbered-string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (line-count (1+ (count #\NewLine input)))
         (line-number-length (1+ (floor (log line-count) (log 10))))
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (iter (with format-string = (format nil "\~~~A,' D " line-number-length))
                            (with input-offset = 0)
                            (for line-index :from 0)
                            (for line :in (split-sequence #\NewLine input))
                            (for line-number = (format nil format-string line-index))
                            (push (make-iomap/string line-number `(line-number ,typed-input-reference ,line-index) 0
                                                     output output-reference (file-position stream)
                                                     (length line-number))
                                  child-iomaps)
                            (write-string line-number stream)
                            (push (make-iomap/string input input-reference input-offset
                                                     output output-reference (file-position stream)
                                                     (1+ (length line)))
                                  child-iomaps)
                            (write-string line stream)
                            (terpri stream)
                            (incf input-offset (1+ (length line)))))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader string->line-numbered-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
