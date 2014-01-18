;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection book/book->tree/node ()
  ())

(def projection book/chapter->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/book/book->tree/node ()
  (make-projection 'book/book->tree/node))

(def (function e) make-projection/book/chapter->tree/node ()
  (make-projection 'book/chapter->tree/node))

;;;;;;
;;; Printer

(def printer book/book->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the list document) ,index)
                                                                       (the list (elements-of (the book/book document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               ;; KLUDGE:
                               (setf (indentation-of (output-of element-iomap)) 0)
                               (collect element-iomap)))
         (authors (authors-of input))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the string (title-of (the book/book document)))
                               (the sequence-position (pos (the string document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the list document) 0))
                                (the list (children-of (the tree/node document)))))
                             (((the list (elements-of (the book/book document)))
                               (the ?element-type (elt (the list document) ?element-index))
                               . ?rest)
                              (bind ((element-iomap (elt element-iomaps ?element-index))
                                     (element-output (output-of element-iomap)))
                                (append (selection-of element-output)
                                        `((the ,(form-type element-output) (elt (the list document) ,(+ ?element-index (if authors 2 1))))
                                          (the list (children-of (the tree/node document)))))))))
         (output (make-tree/node (append (list (tree/leaf (:selection (butlast output-selection 2))
                                                 (text/text (:selection (butlast output-selection 3))
                                                   (text/string (title-of input) :font *font/liberation/serif/bold/36* :font-color *color/solarized/red*))))
                                         (when authors
                                           (list (make-tree/node (iter (for index :from 0)
                                                                       (for author :in-sequence authors)
                                                                       (collect (tree/leaf ()
                                                                                  (text/text ()
                                                                                    (text/string author :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*)))))
                                                                 :opening-delimiter (text/text () (text/string "Written by " :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*))
                                                                 :indentation 0)))
                                         (mapcar 'output-of element-iomaps))
                                 :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer book/chapter->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the list document) ,index)
                                                                       (the list (elements-of (the book/chapter document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               ;; KLUDGE:
                               (setf (indentation-of (output-of element-iomap)) 0)
                               (collect element-iomap)))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the string (title-of (the book/chapter document)))
                               (the sequence-position (pos (the string document) ?character-index)))
                              `((the sequence-position (text/pos (the text/text document) ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))
                                (the tree/leaf (elt (the list document) 0))
                                (the list (children-of (the tree/node document)))))
                             (((the list (elements-of (the book/chapter document)))
                               (the ?element-type (elt (the list document) ?element-index))
                               . ?rest)
                              (bind ((element-iomap (elt element-iomaps ?element-index))
                                     (element-output (output-of element-iomap)))
                                (append (selection-of element-output)
                                        `((the ,(form-type element-output) (elt (the list document) ,(+ ?element-index 1)))
                                          (the list (children-of (the tree/node document)))))))))
         (output (make-tree/node (list* (tree/leaf (:selection (butlast output-selection 2))
                                          (text/text (:selection (butlast output-selection 3))
                                            (text/string (title-of input) :font *font/liberation/serif/bold/24* :font-color *color/solarized/blue*)))
                                        (mapcar 'output-of element-iomaps))
                                 :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

;;;;;;
;;; Reader

;; TODO: factor out common parts
(def reader book/book->tree/node (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (reverse (selection-of operation))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the tree/leaf (elt (the list document) 0))
                                                        (the text/text (content-of (the tree/leaf document)))
                                                        (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                       `((the sequence-position (pos (the string document) ,?character-index))
                                                         (the string (title-of (the book/book document)))))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the ?child-type (elt (the list document) ?child-index))
                                                        . ?rest)
                                                       (bind ((element-index (- ?child-index (if (authors-of input) 2 1)))
                                                              (element (elt (elements-of input) element-index))
                                                              (input-element-operation (make-operation/replace-selection element (reverse ?rest)))
                                                              (output-element-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) element-index) gesture-queue input-element-operation)))
                                                         (append (selection-of output-element-operation)
                                                                 `((the ,(form-type element) (elt (the list document) ,element-index))
                                                                   (the list (elements-of (the book/book document))))))))))
                 (operation/sequence/replace-element-range
                  (awhen (pattern-case (reverse (target-of operation))
                           (((the list (children-of (the tree/node document)))
                             (the tree/leaf (elt (the list document) 0))
                             (the text/text (content-of (the tree/leaf document)))
                             (the sequence-position (text/pos (the text/text document) ?character-index)))
                            `((the sequence-position (pos (the string document) ,?character-index))
                              (the string (title-of (the book/book document)))))
                           (((the list (children-of (the tree/node document)))
                             (the ?child-type (elt (the list document) ?child-index))
                             . ?rest)
                            (bind ((element-index (- ?child-index (if (authors-of input) 2 1)))
                                   (element (elt (elements-of input) element-index))
                                   (input-element-operation (make-operation/sequence/replace-element-range element (reverse ?rest) (replacement-of operation)))
                                   (output-element-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) element-index) gesture-queue input-element-operation)))
                              (append (target-of output-element-operation)
                                      `((the ,(form-type element) (elt (the list document) ,element-index))
                                        (the list (elements-of (the book/book document))))))))
                    (make-operation/sequence/replace-element-range input it (replacement-of operation))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))

(def reader book/chapter->tree/node (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (bind ((input (input-of projection-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/quit operation)
                 (operation/replace-selection
                  (make-operation/replace-selection input
                                                    (pattern-case (reverse (selection-of operation))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the tree/leaf (elt (the list document) 0))
                                                        (the text/text (content-of (the tree/leaf document)))
                                                        (the sequence-position (text/pos (the text/text document) ?character-index)))
                                                       `((the sequence-position (pos (the string document) ,?character-index))
                                                         (the string (title-of (the book/chapter document)))))
                                                      (((the list (children-of (the tree/node document)))
                                                        (the ?child-type (elt (the list document) ?child-index))
                                                        . ?rest)
                                                       (bind ((element-index (- ?child-index 1))
                                                              (element (elt (elements-of input) element-index))
                                                              (input-element-operation (make-operation/replace-selection element (reverse ?rest)))
                                                              (output-element-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) element-index) gesture-queue input-element-operation)))
                                                         (append (selection-of output-element-operation)
                                                                 `((the ,(form-type element) (elt (the list document) ,element-index))
                                                                   (the list (elements-of (the book/chapter document))))))))))
                 (operation/sequence/replace-element-range
                  (awhen (pattern-case (reverse (target-of operation))
                           (((the list (children-of (the tree/node document)))
                             (the tree/leaf (elt (the list document) 0))
                             (the text/text (content-of (the tree/leaf document)))
                             (the sequence-position (text/pos (the text/text document) ?character-index)))
                            `((the sequence-position (pos (the string document) ,?character-index))
                              (the string (title-of (the book/chapter document)))))
                           (((the list (children-of (the tree/node document)))
                             (the ?child-type (elt (the list document) ?child-index))
                             . ?rest)
                            (bind ((element-index (- ?child-index 1))
                                   (element (elt (elements-of input) element-index))
                                   (input-element-operation (make-operation/sequence/replace-element-range element (reverse ?rest) (replacement-of operation)))
                                   (output-element-operation (recurse-reader recursion (elt (child-iomaps-of projection-iomap) element-index) gesture-queue input-element-operation)))
                              (append (target-of output-element-operation)
                                      `((the ,(form-type element) (elt (the list document) ,element-index))
                                        (the list (elements-of (the book/chapter document))))))))
                    (make-operation/sequence/replace-element-range input it (replacement-of operation))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse operation))))
