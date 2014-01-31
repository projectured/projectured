;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Text

(def function make-test-selection/text/character ()
  '((the sequence-position (text/pos (the text/text document) 2))))

;;;;;;
;;; Table

;; TODO: move
(def test test-table-index (row-heights column-widths)
  (iter (with table-width = (+ (sum* column-widths) (length column-widths)))
        (with table-height = (sum* row-heights))
        (for table-character-index :from 0 :below (* table-width table-height))
        (print table-character-index)
        (for (values row-index column-index cell-character-index) = (cell-character-index row-heights column-widths table-character-index))
        (is (= table-character-index (table-character-index row-heights column-widths row-index column-index cell-character-index)))))

(def function make-test-selection/table/character ()
  '((the sequence-position (text/pos (the text/text document) 1))
    (the text/text (content-of (the table/cell document)))
    (the table/cell (elt (the sequence document) 1))
    (the sequence (cells-of (the table/row document)))
    (the table/row (elt (the sequence document) 0))
    (the sequence (rows-of (the table/table document)))))

(def function make-test-selection/table/character/nested ()
  '((the sequence-position (text/pos (the text/text document) 1))
    (the text/text (content-of (the table/cell document)))
    (the table/cell (elt (the sequence document) 0))
    (the sequence (cells-of (the table/row document)))
    (the table/row (elt (the sequence document) 1))
    (the sequence (rows-of (the table/table document)))
    (the table/table (content-of (the table/cell document)))
    (the table/cell (elt (the sequence document) 1))
    (the sequence (cells-of (the table/row document)))
    (the table/row (elt (the sequence document) 1))
    (the sequence (rows-of (the table/table document)))))

(def function make-test-selection/table/cell ()
  '(the table/cell (elt (the sequence (cells-of (the table/row (elt (the sequence (rows-of (the table/table document))) 1)))) 0)))

;;;;;;
;;; Tree

(def function make-test-selection/tree/character ()
  '((the sequence-position (text/pos (the text/text document) 1))
    (the text/text (content-of (the tree/leaf document)))
    (the tree/leaf (elt (the sequence document) 1))
    (the sequence (children-of (the tree/node document)))
    (the tree/node (elt (the sequence document) 1))
    (the sequence (children-of (the tree/node document)))))

;;;;;;
;;; JSON

(def function make-test-selection/json/empty ()
  '(the json/nothing document))

(def function make-test-selection/json/object ()
  '(the json/object (elt (the sequence (elements-of (the json/array document))) 5)))

(def function make-test-selection/json/character ()
  '(the character (elt (the string (value-of (the json/string (elt (the sequence (elements-of (the json/array document))) 4)))) 8)))

(def function make-test-selection/json/character-position ()
  '((the sequence-position (pos (the string document) 8))
    (the string (value-of (the json/string document)))
    (the json/string (elt (the sequence document) 4))
    (the sequence (elements-of (the json/array document)))))

(def test test/selection/navigation/json/empty ()
  (test/selection/navigation (make-test-document/json/empty) nil (make-test-projection/json->graphics)))

(def test test/selection/navigation/json/null ()
  (test/selection/navigation (make-test-document/json/null) nil (make-test-projection/json->graphics)))

(def test test/selection/navigation/json/boolean ()
  (test/selection/navigation (make-test-document/json/boolean) nil (make-test-projection/json->graphics)))

(def test test/selection/navigation/json/array ()
  (test/selection/navigation (make-test-document/json/array) nil (make-test-projection/json->graphics)))

(def test test/selection/navigation/json/object ()
  (test/selection/navigation (make-test-document/json/object) nil (make-test-projection/json->graphics)))

(def test test/selection/navigation/json ()
  (test/selection/navigation (make-test-document/json) nil (make-test-projection/json->graphics)))

;;;;;;
;;; Selection

(def suite* (test/selection :in test))

(def test test/selection/navigation (content selection projection)
  (iter (with executed-operation-count = 0)
        (with processed-selections = nil)
        (with remaining-selections = (list selection))
        (while remaining-selections)
        (for selected-selection = (pop remaining-selections))
        (test.debug "Testing selection: ~A" selected-selection)
        (for document = (document (:selection selected-selection) content))
        (for printer-iomap = (finishes (apply-printer document projection)))
        (for gesture-queue = (make-instance 'gesture-queue :gestures (list (gesture/keyboard/key-press :sdl-key-h :control))))
        (for command-map = (finishes (apply-reader projection printer-iomap gesture-queue)))
        (iter (for command :in-sequence (commands-of command-map))
              (for gesture = (gesture-of command))
              (for operation = (operation-of command))
              (when (typep operation 'operation/replace-selection)
                (test.debug "Executing gesture: ~A" (gesture/describe gesture))
                (redo-operation operation)
                (incf executed-operation-count)
                (bind ((new-selection (selection-of document)))
                  (unless (or (member new-selection remaining-selections :test 'equal)
                              (member new-selection processed-selections :test 'equal))
                    (test.debug "Adding new selection to be tested: ~A" new-selection)
                    (push new-selection remaining-selections)))))
        (push selected-selection processed-selections)
        (test.debug "Remaining selections: ~A" (length processed-selections) (length remaining-selections))
        (finally
         (test.info "Completed processing ~A selections, executed ~A operations" (length processed-selections) executed-operation-count))))
