;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection table/table->graphics/canvas ()
  ())

;;;;;;
;;; Construction

(def function make-projection/table/table->graphics/canvas ()
  (make-projection 'table/table->graphics/canvas))

;;;;;;
;;; Construction

(def macro table/table->graphics/canvas ()
  '(make-projection/table/table->graphics/canvas))


;;;;;;
;;; Forward mapper

(def function forward-mapper/table/table->graphics/canvas (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (rows-of (the table/table document)))
        (the table/row (elt (the sequence document) ?row-index))
        (the sequence (cells-of (the table/row document)))
        (the table/cell (elt (the sequence document) ?cell-index))
        (the ?type (content-of (the table/cell document)))
        . ?rest)
       (bind ((column-count (length (cells-of (first-elt (rows-of printer-input)))))
              (cell-iomap (elt (elt (child-iomaps-of printer-iomap) ?row-index) ?cell-index))
              (index (+ (* column-count ?row-index) ?cell-index)))
         (values `((the sequence (elements-of (the graphics/canvas document)))
                   (the graphics/canvas (elt (the sequence document) ,index))
                   (the sequence (elements-of (the graphics/canvas document)))
                   (the ,(form-type (output-of cell-iomap)) (elt (the sequence document) 0)))
                 ?rest
                 cell-iomap)))
      (((the graphics/canvas (printer-output (the table/table document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/table/table->graphics/canvas (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the graphics/canvas document)))
        (the graphics/canvas (elt (the sequence document) ?index))
        (the sequence (elements-of (the graphics/canvas document)))
        (the graphics/canvas (elt (the sequence document) 0))
        . ?rest)
       (bind ((column-count (length (cells-of (first-elt (rows-of printer-input)))))
              (row-index (floor (/ ?index column-count)))
              (cell-index (mod ?index column-count))
              (cell-iomap (elt (elt (child-iomaps-of printer-iomap) row-index) cell-index)))
         (values `((the sequence (rows-of (the table/table document)))
                   (the table/row (elt (the sequence document) ,row-index))
                   (the sequence (cells-of (the table/row document)))
                   (the table/cell (elt (the sequence document) ,cell-index))
                   (the ,(form-type (input-of cell-iomap)) (content-of (the table/cell document))))
                 ?rest
                 cell-iomap)))
      (?a
       (append `((the graphics/canvas (printer-output (the table/table document) ,projection ,recursion))) reference)))))


;;;;;;
;;; Printer

(def printer table/table->graphics/canvas (projection recursion input input-reference)
  (bind ((spacing 5)
         (rows (rows-of input))
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
                                             (for cell-output = (output-of (elt (elt cell-iomaps row-index) column-index)))
                                             (maximizing (2d-y (size-of (bounds-of cell-output)))))))))
         (column-widths (iter (for column-index :from 0 :below (length (cells-of (first-elt rows))))
                              (collect (iter (for row-index :from 0)
                                             (for row :in-sequence rows)
                                             (for cell-output = (output-of (elt (elt cell-iomaps row-index) column-index)))
                                             (maximizing (2d-x (size-of (bounds-of cell-output))))))))
         (output-selection (print-selection (make-iomap/compound projection recursion input input-reference nil cell-iomaps)
                                            (selection-of input)
                                            'forward-mapper/table/table->graphics/canvas))
         (output (make-graphics/canvas (append (iter outer
                                                     (for y :from spacing)
                                                     (for row-index :from 0)
                                                     (for row :in-sequence rows)
                                                     (for row-height = (elt row-heights row-index))
                                                     (iter (for x :from spacing)
                                                           (for column-index :from 0)
                                                           (for column-width = (elt column-widths column-index))
                                                           (for cell :in-sequence (cells-of row))
                                                           (for cell-iomap = (elt (elt cell-iomaps row-index) column-index))
                                                           (for cell-output = (output-of cell-iomap))
                                                           (in outer (collect (make-graphics/canvas (list cell-output) (make-2d x y))))
                                                           (incf x (+ column-width (* 2 spacing))))
                                                     (incf y (+ row-height (* 2 spacing))))
                                               (iter (with column-width = (+ (reduce #'+ column-widths) (* 2 (length column-widths) spacing) (length column-widths)))
                                                     (for y :from (- spacing))
                                                     (for row-height :in-sequence (list* 0 row-heights))
                                                     (incf y (+ row-height spacing))
                                                     (collect (make-graphics/line (make-2d 0 y) (make-2d column-width y) :stroke-color *color/black*))
                                                     (incf y spacing))
                                               (iter (with row-height = (+ (reduce #'+ row-heights) (* 2 (length row-heights) spacing) (length row-heights)))
                                                     (for x :from (- spacing))
                                                     (for column-width :in-sequence (list* 0 column-widths))
                                                     (incf x (+ column-width spacing))
                                                     (collect (make-graphics/line (make-2d x 0) (make-2d x row-height) :stroke-color *color/black*))
                                                     (incf x spacing)))
                                       0
                                       :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output cell-iomaps)))

;;;;;;
;;; Reader

(def reader table/table->graphics/canvas (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/table/table->graphics/canvas 'backward-mapper/table/table->graphics/canvas)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/table/table->graphics/canvas nil)
                  (make-command/nothing (gesture-of input))))
