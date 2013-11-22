;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection table/table->text ()
  ((output-border :type boolean)))

;;;;;;
;;; Construction

(def (function e) make-projection/table/table->text ()
  (make-projection 'table/table->text :output-border #f))

;;;;;;
;;; Construction

(def (macro e) table/table->text ()
  '(make-projection/table/table->text))

;;;;;;
;;; Printer

(def printer table/table->text (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (rows (rows-of input))
         (cell-iomaps (iter (for row-index :from 0)
                            (for row :in-sequence rows)
                            (collect (iter (for cell-index :from 0)
                                           (for cell :in-sequence (cells-of row))
                                           (collect (recurse-printer recursion iomap (content-of cell)
                                                                     `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index)))
                                                                     `(the text/text ,output-reference)))))))
         (row-heights (when rows
                        (iter (for row-index :from 0)
                              (for row :in-sequence rows)
                              (collect (iter (for cell-index :from 0)
                                             (for cell :in-sequence (cells-of row))
                                             (for content = (output-of (elt (elt cell-iomaps row-index) cell-index)))
                                             (maximizing (+ (text/count content #\NewLine)
                                                            ;; TODO:
                                                            1
                                                            #+nil
                                                            (if (or (string= content "")
                                                                    (char= #\NewLine (last-elt content)))
                                                                0
                                                                1))))))))
         (column-widths (iter (for cell-index :from 0 :below (length (cells-of (first-elt rows))))
                              (collect (iter (for row-index :from 0)
                                             (for row :in-sequence rows)
                                             (for content = (output-of (elt (elt cell-iomaps row-index) cell-index)))
                                             (maximizing (iter (for line :in (text/split content #\NewLine))
                                                               (maximizing (text/length line))))))))
         (total-width (+ 1 (reduce '+ column-widths) (length column-widths)))
         (output (make-text/text
                  (nreverse
                   (prog1-bind elements nil
                     (flet ((push-character (character &optional (count 1))
                              (push (make-text/string (make-string count :element-type 'character :initial-element character) :font *font/default* :font-color *color/solarized/gray*) elements)))
                       (iter (for row-index :from 0)
                             (for row :in-sequence rows)
                             (for row-height = (elt row-heights row-index))
                             (push (make-iomap/text* projection recursion
                                                     input `(the text/text (border-of ,typed-input-reference)) 0
                                                     nil `(the text/text ,output-reference) 0
                                                     total-width)
                                   child-iomaps)
                             (when (output-border-p projection)
                               (if (first-iteration-p)
                                   (push-character #\U250C)
                                   (push-character #\U251C))
                               (if (first-iteration-p)
                                   (iter (for column-width :in column-widths)
                                         (unless (first-iteration-p)
                                           (push-character #\U252C))
                                         (push-character #\U2500 column-width)
                                         (finally
                                          (push-character #\U2510)))
                                   (iter (for column-width :in column-widths)
                                         (unless (first-iteration-p)
                                           (push-character #\U253C))
                                         (push-character #\U2500 column-width)
                                         (finally
                                          (push-character #\U2524))))
                               (push-character #\NewLine))
                             (iter (with cell-text-lines = (make-array row-height :initial-element (make-text/text nil)))
                                   (for cell-index :from 0)
                                   (for cell :in-sequence (cells-of row))
                                   (for cell-iomap = (elt (elt cell-iomaps row-index) cell-index))
                                   (for content = (output-of cell-iomap))
                                   (push cell-iomap child-iomaps)
                                   (for column-width = (elt column-widths cell-index))
                                   (for table-reference = `(content-of (the table/cell (elt (the list (cells-of (the table/row (elt (the list (rows-of ,typed-input-reference)) ,row-index)))) ,cell-index))))
                                   (iter (for text-line-index :from 0 :below row-height)
                                         (setf (elt cell-text-lines text-line-index)
                                               (text/concatenate (elt cell-text-lines text-line-index)
                                                                 ;; TODO: map this separator to output
                                                                 (text/text ()
                                                                   (text/string (cond ((output-border-p projection) (string #\U2502))
                                                                                      ((zerop cell-index) "")
                                                                                      (t " "))
                                                                                :font *font/default* :font-color *color/solarized/gray*))
                                                                 (bind ((lines (text/split content #\NewLine)))
                                                                   (if (< text-line-index (length lines))
                                                                       (bind ((line (elt lines text-line-index))
                                                                              (padding (make-text/text (list (make-text/string (make-string-of-spaces (max 0 (- column-width (text/length line)))) :font *font/default* :font-color *color/default*)))))
                                                                         (text/concatenate line padding))
                                                                       (make-text/text (list (make-text/string (make-string-of-spaces column-width) :font *font/default* :font-color *color/default*))))))))
                                   (finally
                                    (iter (for text-line-index :from 0)
                                          (for text-line :in-sequence cell-text-lines)
                                          (iter (for element :in-sequence (elements-of text-line))
                                                (push element elements))
                                          (when (output-border-p projection)
                                            (push (make-iomap/text* projection recursion
                                                                    input `(the text/text (border-of ,typed-input-reference)) 0
                                                                    nil `(the text/text ,output-reference) 0 ;; TODO: (file-position stream)
                                                                    1)
                                                  child-iomaps)
                                            (push-character #\U2502))
                                          (unless (and (= row-index (1- (length rows)))
                                                       (= text-line-index (1- (length cell-text-lines))))
                                            (push-character #\NewLine))))))
                       (when (output-border-p projection)
                         (push (make-iomap/text* projection recursion
                                                 input `(the text/text (border-of ,typed-input-reference)) 0
                                                 nil `(the text/text ,output-reference) 0 ;; TODO: (file-position stream)
                                                 total-width)
                               child-iomaps)
                         (push-character #\U2514)
                         (iter (for column-width :in column-widths)
                               (unless (first-iteration-p)
                                 (push-character #\U2534))
                               (push-character #\U2500 column-width)
                               (finally
                                (push-character #\U2518))))))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader table/table->text (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
