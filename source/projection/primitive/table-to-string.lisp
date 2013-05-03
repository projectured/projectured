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

;; TODO: make it styled-string, not simple string
(def printer table/table->string (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (row-heights (table/row-heights iomap input recursion))
         (column-widths (table/column-widths iomap input recursion))
         (total-width (+ 1 (reduce '+ column-widths) (length column-widths)))
         (output (make-text/text
                  (nreverse
                   (prog1-bind elements nil
                     (flet ((push-character (character)
                              (push (make-text/string (string character) :font *font/default* :font-color *color/solarized/gray*) elements)))
                       (iter (for row-index :from 0)
                             (for row :in-sequence (rows-of input))
                             (for row-height = (elt row-heights row-index))
                             (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                       nil `(the string ,output-reference) 0 ;; TODO: (file-position stream)
                                                       total-width)
                                   child-iomaps)
                             (if (first-iteration-p)
                                 (push-character #\U250C)
                                 (push-character #\U251C))
                             (if (first-iteration-p)
                                 (iter (for column-width :in column-widths)
                                       (unless (first-iteration-p)
                                         (push-character #\U252C))
                                       (iter (repeat column-width)
                                             (push-character #\U2500))
                                       (finally
                                        (push-character #\U2510)))
                                 (iter (for column-width :in column-widths)
                                       (unless (first-iteration-p)
                                         (push-character #\U253C))
                                       (iter (repeat column-width)
                                             (push-character #\U2500))
                                       (finally
                                        (push-character #\U2524))))
                             (push-character #\NewLine)
                             (iter (with cell-text-lines = (make-array row-height :initial-element (make-text/text nil)))
                                   (for cell-index :from 0)
                                   (for cell :in-sequence (cells-of row))
                                   ;; TODO: reuse iomap
                                   (for cell-iomap = (recurse-printer recursion iomap (content-of cell)
                                                                      `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index)))
                                                                      `(the string ,output-reference)))
                                   (for content = (output-of cell-iomap))
                                   (push cell-iomap child-iomaps)
                                   (for column-width = (elt column-widths cell-index))
                                   (for table-reference = `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index))))
                                   (iter (for text-line-index :from 0 :below row-height)
                                         (setf (elt cell-text-lines text-line-index)
                                               (text/concatenate (elt cell-text-lines text-line-index)
                                                                 ;; TODO: map this separator to output
                                                                 (make-text/text (list (make-text/string (string #\U2502) :font *font/default* :font-color *color/solarized/gray*)))
                                                                 (bind ((lines (text/split content #\NewLine)))
                                                                   (if (< text-line-index (length lines))
                                                                       (bind ((line (elt lines text-line-index))
                                                                              (padding (make-text/text (list (make-text/string (make-string-of-spaces (max 0 (- column-width (text/length line)))) :font *font/default* :font-color *color/default*)))))
                                                                         (text/concatenate line padding))
                                                                       (make-text/text (list (make-text/string (make-string-of-spaces column-width) :font *font/default* :font-color *color/default*))))))))
                                   (finally
                                    (iter (for text-line :in-sequence cell-text-lines)
                                          (iter (for element :in-sequence (elements-of text-line))
                                                (push element elements))
                                          (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                                    nil `(the string ,output-reference) 0 ;; TODO: (file-position stream)
                                                                    1)
                                                child-iomaps)
                                          (push-character #\U2502)
                                          (push-character #\NewLine)))))
                       (push (make-iomap/string* input `(the string (border-of ,typed-input-reference)) 0
                                                 nil `(the string ,output-reference) 0 ;; TODO: (file-position stream)
                                                 total-width)
                             child-iomaps)
                       (push-character #\U2514)
                       (iter (for column-width :in column-widths)
                             (unless (first-iteration-p)
                               (push-character #\U2534))
                             (iter (repeat column-width)
                                   (push-character #\U2500))
                             (finally
                              (push-character #\U2518)))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader table/table->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
