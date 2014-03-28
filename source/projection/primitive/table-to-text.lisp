;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/table/table->text ()
  ((row-heights :type sequence)
   (column-widths :type sequence)
   (cell-iomaps :type sequence)))

;;;;;;
;;; Forward mapper

(def function table-character-index (row-heights column-widths row-index column-index cell-character-index)
  (bind ((table-width (+ (sum* column-widths) (length column-widths)))
         (cell-width (elt column-widths column-index))
         (cell-first-character-index (+ column-index
                                        (sum* (subseq column-widths 0 column-index))
                                        (* table-width (sum* (subseq row-heights 0 row-index)))))
         ((:values cell-line-index cell-line-character-index) (floor cell-character-index (1+ cell-width))))
    (+ (* cell-line-index table-width) cell-first-character-index cell-line-character-index)))

;;;;;;
;;; Backward mapper

(def function cell-character-index (row-heights column-widths table-character-index)
  (bind ((table-width (+ (sum* column-widths) (length column-widths)))
         ((:values line-index line-character-index) (floor table-character-index table-width))
         (row-index (iter (for row-index :from 0)
                          (for row-height :in-sequence row-heights)
                          (if (> row-height line-index)
                              (return row-index)
                              (decf line-index row-height))))
         (column-index (iter (for column-index :from 0)
                             (for column-width :in-sequence column-widths)
                             (if (>= column-width line-character-index)
                                 (return column-index)
                                 (decf line-character-index (1+ column-width)))))
         (cell-width (elt column-widths column-index))
         (cell-first-character-index (+ column-index
                                        (sum* (subseq column-widths 0 column-index))
                                        (* table-width (sum* (subseq row-heights 0 row-index)))))
         ((:values cell-line-index cell-line-character-index) (floor (- table-character-index cell-first-character-index) table-width)))
    (values row-index column-index (+ (* cell-line-index (1+ cell-width)) cell-line-character-index))))

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

(def printer table/table->text (projection recursion input input-reference)
  (bind ((rows (rows-of input))
         (cell-iomaps (iter (for row-index :from 0)
                            (for row :in-sequence rows)
                            (collect (iter (for column-index :from 0)
                                           (for cell :in-sequence (cells-of row))
                                           (for cell-content = (content-of cell))
                                           (collect (recurse-printer recursion cell-content
                                                                     `((content-of (the table/cell document))
                                                                       (the table/cell (elt (the sequence document) ,column-index))
                                                                       (the sequence (cells-of (the table/row document)))
                                                                       (the table/row (elt (the sequence document) ,row-index))
                                                                       (the sequence (rows-of (the table/table document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))))))
         (row-heights (when rows
                        (iter (for row-index :from 0)
                              (for row :in-sequence rows)
                              (collect (iter (for column-index :from 0)
                                             (for cell :in-sequence (cells-of row))
                                             (for content = (output-of (elt (elt cell-iomaps row-index) column-index)))
                                             (maximizing (1+ (text/count content #\NewLine))))))))
         (column-widths (iter (for column-index :from 0 :below (length (cells-of (first-elt rows))))
                              (collect (iter (for row-index :from 0)
                                             (for row :in-sequence rows)
                                             (for content = (output-of (elt (elt cell-iomaps row-index) column-index)))
                                             (maximizing (iter (for line :in (text/split content #\NewLine))
                                                               (maximizing (text/length line))))))))
         (output-index 0)
         (elements (nreverse
                    (prog1-bind elements nil
                      (flet ((write-character (character &optional (count 1))
                               (push (make-text/string (make-string count :element-type 'character :initial-element character) :font *font/default* :font-color *color/solarized/gray*) elements)
                               (incf output-index count))
                             (write-element (element)
                               (push element elements)
                               (incf output-index (length (content-of element)))))
                        (iter (for row-index :from 0)
                              (for row :in-sequence rows)
                              (for row-height = (elt row-heights row-index))
                              (when (output-border-p projection)
                                (if (first-iteration-p)
                                    (write-character #\U250C)
                                    (write-character #\U251C))
                                (if (first-iteration-p)
                                    (iter (for column-width :in column-widths)
                                          (unless (first-iteration-p)
                                            (write-character #\U252C))
                                          (write-character #\U2500 column-width)
                                          (finally
                                           (write-character #\U2510)))
                                    (iter (for column-width :in column-widths)
                                          (unless (first-iteration-p)
                                            (write-character #\U253C))
                                          (write-character #\U2500 column-width)
                                          (finally
                                           (write-character #\U2524))))
                                (write-character #\NewLine))
                              (iter (with cell-text-lines = (make-array row-height :initial-element (make-text/text nil)))
                                    (for column-index :from 0)
                                    (for cell :in-sequence (cells-of row))
                                    (for cell-iomap = (elt (elt cell-iomaps row-index) column-index))
                                    (for content = (output-of cell-iomap))
                                    (for column-width = (elt column-widths column-index))
                                    (iter (for text-line-index :from 0 :below row-height)
                                          (setf (elt cell-text-lines text-line-index)
                                                (text/concatenate (elt cell-text-lines text-line-index)
                                                                  ;; TODO: map this separator to output
                                                                  (text/text ()
                                                                    (text/string (cond ((output-border-p projection) (string #\U2502))
                                                                                       ((zerop column-index) "")
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
                                                 (write-element element))
                                           (when (output-border-p projection)
                                             (write-character #\U2502))
                                           (unless (and (= row-index (1- (length rows)))
                                                        (= text-line-index (1- (length cell-text-lines))))
                                             (write-character #\NewLine))))))
                        (when (output-border-p projection)
                          (write-character #\U2514)
                          (iter (for column-width :in column-widths)
                                (unless (first-iteration-p)
                                  (write-character #\U2534))
                                (write-character #\U2500 column-width)
                                (finally
                                 (write-character #\U2518))))))))
         (input-selection (selection-of input))
         (output-selection (pattern-case (reverse input-selection)
                             (((the sequence (rows-of (the table/table document)))
                               (the table/row (elt (the sequence document) ?row-index))
                               (the sequence (cells-of (the table/row document)))
                               (the table/cell (elt (the sequence document) ?column-index))
                               (the ?type (content-of (the table/cell document)))
                               . ?rest)
                              (bind ((cell-iomap (elt (elt cell-iomaps ?row-index) ?column-index)))
                                (pattern-case (selection-of (output-of cell-iomap))
                                  (((the text/text (text/subseq (the text/text document) ?cell-character-index ?cell-character-index)))
                                   (bind ((character-index (table-character-index row-heights column-widths ?row-index ?column-index ?cell-character-index)))
                                     `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                  (((the sequence-box (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                   `((the sequence-box (text/subbox (the text/text document)
                                                                    ,(table-character-index row-heights column-widths ?row-index ?column-index ?start-character-index)
                                                                    ,(table-character-index row-heights column-widths ?row-index ?column-index ?end-character-index))))))))
                             (((the sequence (rows-of (the table/table document)))
                               (the table/row (elt (the sequence document) ?row-index))
                               (the sequence (cells-of (the table/row document)))
                               (the table/cell (elt (the sequence document) ?column-index)))
                              (bind ((cell-character-length (1- (* (elt row-heights ?row-index)
                                                                   (1+ (elt column-widths ?column-index))))))
                                `((the sequence-box (text/subbox (the text/text document)
                                                                 ,(table-character-index row-heights column-widths ?row-index ?column-index 0)
                                                                 ,(table-character-index row-heights column-widths ?row-index ?column-index cell-character-length))))))))
         (output (make-text/text elements :selection output-selection)))
    (make-iomap 'iomap/table/table->text
                :projection projection :recursion recursion
                :input input :output output
                :row-heights row-heights
                :column-widths column-widths
                :cell-iomaps cell-iomaps)))

;;;;;;
;;; Reader

(def function cell-reference? (reference)
  (pattern-case reference
    (((the table/cell ?a)
      . ?rest)
     #t)))

(def function find-table-cell-reference (reference)
  (pattern-case reference
    (((the sequence ((?or subseq text/subseq) (the text/text document) ?a ?a))
      (the text/text (content-of (the table/cell ?b)))
      . ?rest)
     ?rest)))

(def function make-operation/replace-selection/move-cell (table reference row-delta column-delta)
  (pattern-case reference
    (((the table/cell (elt (the sequence document) ?column-index))
      (the sequence (cells-of (the table/row document)))
      (the table/row (elt (the sequence document) ?row-index))
      (the sequence (rows-of (the table/table document)))
      . ?rest)
     (bind ((new-row-index (+ ?row-index row-delta))
            (new-column-index (+ ?column-index column-delta)))
       (when (and (<= 0 new-row-index (1- (length (rows-of table))))
                  (<= 0 new-column-index (1- (length (cells-of (first-elt (rows-of table)))))))
         (make-operation/replace-selection table
                                           `((the table/cell (elt (the sequence document) ,new-column-index))
                                             (the sequence (cells-of (the table/row document)))
                                             (the table/row (elt (the sequence document) ,new-row-index))
                                             (the sequence (rows-of (the table/table document)))
                                             ,@?rest)))))))

(def reader table/table->text (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (selection (selection-of printer-input))
         (text-selection? (text-reference? selection))
         (cell-selection? (cell-reference? selection)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/functional operation)
                               (operation/replace-selection
                                (make-operation/replace-selection printer-input (pattern-case (selection-of operation)
                                                                                  (((the text/text (text/subseq (the text/text ?a) ?table-character-index ?table-character-index)) . ?rest)
                                                                                   (bind (((:values row-index column-index cell-character-index) (cell-character-index (row-heights-of printer-iomap) (column-widths-of printer-iomap) ?table-character-index))
                                                                                          (cell-iomap (elt (elt (cell-iomaps-of printer-iomap) row-index) column-index))
                                                                                          (input-cell-operation (make-operation/replace-selection (output-of cell-iomap)
                                                                                                                                                  `((the text/text (text/subseq (the text/text document) ,cell-character-index ,cell-character-index)))))
                                                                                          (output-cell-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-cell-operation) cell-iomap))))
                                                                                     (append (selection-of output-cell-operation)
                                                                                             `((the ,(form-type (input-of cell-iomap)) (content-of (the table/cell document)))
                                                                                               (the table/cell (elt (the sequence document) ,column-index))
                                                                                               (the sequence (cells-of (the table/row document)))
                                                                                               (the table/row (elt (the sequence document) ,row-index))
                                                                                               (the sequence (rows-of (the table/table document))))))))))
                               (operation/sequence/replace-element-range
                                (make-operation/sequence/replace-element-range printer-input
                                                                               (pattern-case (target-of operation)
                                                                                 (((the text/text (text/subseq (the text/text ?a) ?table-character-index ?table-character-index)) . ?rest)
                                                                                  (bind (((:values row-index column-index cell-character-index) (cell-character-index (row-heights-of printer-iomap) (column-widths-of printer-iomap) ?table-character-index))
                                                                                         (cell-iomap (elt (elt (cell-iomaps-of printer-iomap) row-index) column-index))
                                                                                         (input-cell-operation (make-operation/sequence/replace-element-range (output-of cell-iomap)
                                                                                                                                                              `((the text/text (text/subseq (the text/text document) ,cell-character-index ,cell-character-index)))
                                                                                                                                                              (replacement-of operation)))
                                                                                         (output-cell-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-cell-operation) cell-iomap))))
                                                                                    (append (target-of output-cell-operation)
                                                                                            `((the ,(form-type (input-of cell-iomap)) (content-of (the table/cell document)))
                                                                                              (the table/cell (elt (the sequence document) ,column-index))
                                                                                              (the sequence (cells-of (the table/row document)))
                                                                                              (the table/row (elt (the sequence document) ,row-index))
                                                                                              (the sequence (rows-of (the table/table document))))))))
                                                                               (replacement-of operation)))
                               (operation/describe
                                (pattern-case (target-of operation)
                                  (((the text/text (text/subseq (the text/text document) ?table-start-character-index ?table-end-character-index)) . ?rest)
                                   (bind (((:values row-index column-index cell-start-character-index) (cell-character-index (row-heights-of printer-iomap) (column-widths-of printer-iomap) ?table-start-character-index))
                                          ;; KLUDGE:
                                          (cell-end-character-index (1+ cell-start-character-index))
                                          (cell-iomap (elt (elt (cell-iomaps-of printer-iomap) row-index) column-index))
                                          (input-cell-operation (make-instance 'operation/describe :target `((the text/text (text/subseq (the text/text document) ,cell-start-character-index ,cell-end-character-index)))))
                                          (output-cell-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-cell-operation) cell-iomap))))
                                     (make-instance 'operation/describe
                                                    :target (append (target-of output-cell-operation)
                                                                    `((the ,(form-type (input-of cell-iomap)) (content-of (the table/cell document)))
                                                                      (the table/cell (elt (the sequence document) ,column-index))
                                                                      (the sequence (cells-of (the table/row document)))
                                                                      (the table/row (elt (the sequence document) ,row-index))
                                                                      (the sequence (rows-of (the table/table document))))))))))
                               (operation/compound
                                (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null child-operations)
                                    (make-operation/compound child-operations)))))))
                    (recurse (operation-of input))))
    ;; TODO: dispatch on selection
    #+nil
    (merge-commands (gesture-case latest-gesture
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Table" :description "Turn the selection into a text selection"
                       :operation (when cell-selection?
                                    (make-operation/replace-selection input `((the text/text (text/subseq (the text/text document) 0 0))
                                                                              (the text/text (content-of (the table/cell document)))
                                                                              ,@selection))))
                      ((gesture/keyboard/key-press :sdl-key-space :control)
                       :domain "Table" :description "Turn the selection into a table cell selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection input (find-table-cell-reference selection))))
                      ((gesture/keyboard/key-press :sdl-key-home :alt)
                       :domain "Table" :description "Moves the selection to the first cell of the first row"
                       :operation (bind ((new-selection '((the table/cell (elt (the sequence document) 0))
                                                          (the sequence (cells-of (the table/row document)))
                                                          (the table/row (elt (the sequence document) 0))
                                                          (the sequence (rows-of (the table/table document))))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection input new-selection))))
                      ((gesture/keyboard/key-press :sdl-key-up)
                       :domain "Table" :description "Moves the selection one cell up"
                       :operation (when cell-selection?
                                    (make-operation/replace-selection/move-cell input selection -1 0)))
                      ((gesture/keyboard/key-press :sdl-key-down)
                       :domain "Table" :description "Moves the selection one cell down"
                       :operation (when cell-selection?
                                    (make-operation/replace-selection/move-cell input selection 1 0)))
                      ((gesture/keyboard/key-press :sdl-key-left)
                       :domain "Table" :description "Moves the selection one cell left"
                       :operation (when cell-selection?
                                    (make-operation/replace-selection/move-cell input selection 0 -1)))
                      ((gesture/keyboard/key-press :sdl-key-right)
                       :domain "Table" :description "Moves the selection one cell right"
                       :operation (when cell-selection?
                                    (make-operation/replace-selection/move-cell input selection 0 1)))))))
