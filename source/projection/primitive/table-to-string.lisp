;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) table/table->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/table/table->string ()
  (make-projection 'table/table->string))

;;;;;;
;;; Construction

(def (macro e) table/table->string ()
  '(make-projection/table/table->string))

;;;;;;
;;; Printer

;; TODO: make it text, not string
(def printer table/table->string (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (row-heights (table/row-heights iomap input recursion))
         (column-widths (table/column-widths iomap input recursion))
         (total-width (+ 1 (reduce '+ column-widths) (length column-widths)))
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (iter (for row-index :from 0)
                            (for row :in-sequence (rows-of input))
                            (for row-height = (elt row-heights row-index))
                            (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                      nil `(the string ,output-reference) (file-position stream)
                                                      total-width)
                                  child-iomaps)
                            (if (first-iteration-p)
                                (write-char #\U250C stream)
                                (write-char #\U251C stream))
                            (if (first-iteration-p)
                                (iter (for column-width :in column-widths)
                                      (unless (first-iteration-p)
                                        (write-char #\U252C stream))
                                      (iter (repeat column-width)
                                            (write-char #\U2500 stream))
                                      (finally
                                       (write-char #\U2510 stream)))
                                (iter (for column-width :in column-widths)
                                      (unless (first-iteration-p)
                                        (write-char #\U253C stream))
                                      (iter (repeat column-width)
                                            (write-char #\U2500 stream))
                                      (finally
                                       (write-char #\U2524 stream))))
                            (terpri stream)
                            (iter (with cell-text-lines = (make-array row-height :initial-element ""))
                                  (for cell-index :from 0)
                                  (for cell :in-sequence (cells-of row))
                                  ;; TODO: reuse iomap
                                  (for iomap = (recurse-printer recursion iomap (content-of cell)
                                                                `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index)))
                                                                `(the string ,output-reference)))
                                  (for content = (output-of iomap))
                                  (push iomap child-iomaps)
                                  (for column-width = (elt column-widths cell-index))
                                  (for table-reference = `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index))))
                                  (iter (for text-line-index :from 0 :below row-height)
                                        (setf (elt cell-text-lines text-line-index)
                                              (string+ (elt cell-text-lines text-line-index)
                                                       ;; TODO: map this separator to output
                                                       (string #\U2502)
                                                       (bind ((lines (split-sequence #\NewLine content)))
                                                         (if (< text-line-index (length lines))
                                                             (bind ((line (elt lines text-line-index)))
                                                               (string+ line (make-string-of-spaces (max 0 (- column-width (length line))))))
                                                             (make-string-of-spaces column-width))))))
                                  (finally
                                   (iter (for text-line :in-sequence cell-text-lines)
                                         (write-string text-line stream)
                                         (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                                   output `(the string ,output-reference) (file-position stream)
                                                                   1)
                                               child-iomaps)
                                         (write-char #\U2502 stream)
                                         (terpri stream)))))
                      (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                nil `(the string ,output-reference) (file-position stream)
                                                total-width)
                            child-iomaps)
                      (write-char #\U2514 stream)
                      (iter (for column-width :in column-widths)
                            (unless (first-iteration-p)
                              (write-char #\U2534 stream))
                            (iter (repeat column-width)
                                  (write-char #\U2500 stream))
                            (finally
                             (write-char #\U2518 stream))))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader table/table->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
