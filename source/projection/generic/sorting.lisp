;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sorting ()
  ((key :type function)
   (predicate :type function)))

;;;;;;
;;; Construction

(def function make-projection/sorting (&key key predicate)
  (make-projection 'sorting :key key :predicate predicate))

;;;;;;
;;; Construction

(def macro sorting (&key key predicate)
  `(make-projection/sorting :key ,key :predicate ,predicate))

;;;;;
;;; IO map

(def iomap iomap/sorting ()
  ((element-iomaps :type sequence)
   (input-indices :type sequence)))

;;;;;;
;;; Forward mapper

(def forward-mapper sorting ()
  (pattern-case -reference-
    (((the ?element-type (elt (the sequence document) ?element-index))
      . ?rest)
     (bind ((output-index (position ?element-index (input-indices-of -printer-iomap-))))
       (values`((the ,?element-type (elt (the sequence document) ,output-index)))
              ?rest
              (elt (element-iomaps-of -printer-iomap-) ?element-index))))))

;;;;;;
;;; Backward mapper

(def backward-mapper sorting ()
  (pattern-case -reference-
    (((the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
     (bind ((element-index (elt (input-indices-of -printer-iomap-) ?element-index)))
       (values `((the ,?element-type (elt (the sequence document) ,element-index)))
               ?rest
               (elt (element-iomaps-of -printer-iomap-) element-index))))))

;;;;;;
;;; Printer

(def printer sorting ()
  (bind ((element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence -input-)
                                   (collect (recurse-printer -recursion- element `((elt (the sequence document) ,index)
                                                                                   ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (key (or (key-of -input-) (key-of -projection-)))
         (predicate (or (predicate-of -input-) (predicate-of -projection-)))
         (indices (iter (for index :from 0 :below (length -input-))
                        (collect index)))
         (input-indices (if (and key predicate)
                            (stable-sort indices predicate :key (lambda (index) (funcall key (elt -input- index))))
                            indices))
         (output-elements (as (iter (for input-index :in input-indices)
                                    (collect (output-of (elt (va element-iomaps) input-index))))))
         (output-selection (as (print-selection (make-instance 'iomap/sorting
                                                               :projection -projection- :recursion -recursion-
                                                               :input -input- :output nil
                                                               :element-iomaps element-iomaps
                                                               :input-indices input-indices)
                                                (selection-of -input-)
                                                'forward-mapper/sorting)))
         (output (as (etypecase -input-
                       (t ;; KLUDGE: typechecking fails in SBCL document/sequence
                        (make-document/sequence (va output-elements) :selection output-selection))
                       (sequence (va output-elements))))))
    (make-instance 'iomap/sorting
                   :projection -projection- :recursion -recursion-
                   :input -input- :output output
                   :element-iomaps element-iomaps
                   :input-indices input-indices)))

;;;;;;
;;; Reader

(def reader sorting ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/sorting nil)
                  (make-nothing-command -gesture-)))
