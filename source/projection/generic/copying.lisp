;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) copying ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/copying ()
  (make-projection 'copying))

;;;;;;
;;; Construction

(def (macro e) copying ()
  '(make-projection/copying))

;;;;;;
;;; Printer

(def printer copying (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
    (etypecase input
      (symbol (make-iomap/object projection recursion input input-reference input output-reference))
      (number (make-iomap/object projection recursion input input-reference input output-reference))
      (string (make-iomap/compound projection recursion input input-reference input output-reference
                                    (list (make-iomap/object projection recursion input input-reference input output-reference)
                                          (make-iomap/string input input-reference 0 input output-reference 0 (length input)))))
      (pathname (make-iomap/object projection recursion input input-reference input output-reference))
      (style/color (make-iomap/object projection recursion input input-reference input output-reference))
      (style/font (make-iomap/object projection recursion input input-reference input output-reference))
      (cons
       (bind ((car-iomap (recurse-printer recursion iomap (car input) `(car ,typed-input-reference) `(car (the ,(form-type input) ,output-reference))))
              (cdr-iomap (recurse-printer recursion iomap (cdr input) `(cdr ,typed-input-reference) `(cdr (the ,(form-type input) ,output-reference))))
              (output (cons (output-of car-iomap) (output-of cdr-iomap))))
         (make-iomap/compound projection recursion input input-reference output output-reference
                               (list (make-iomap/object projection recursion input input-reference output output-reference) car-iomap cdr-iomap))))
      (standard-object
       (bind ((class (class-of input))
              (child-iomaps nil)
              (output (prog1-bind clone (allocate-instance class)
                        (dolist (slot (class-slots class))
                          (when (slot-boundp-using-class class input slot)
                            (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class)))
                                   (slot-reader (first (slot-definition-readers direct-slot)))
                                   (slot-value (slot-value-using-class class input slot))
                                   (iomap (recurse-printer recursion iomap slot-value
                                                           (if slot-reader
                                                               `(,slot-reader ,typed-input-reference)
                                                               `(slot-value ,typed-input-reference ',(slot-definition-name slot)))
                                                           (if slot-reader
                                                               `(,slot-reader (the ,(form-type input) ,output-reference))
                                                               `(slot-value (the ,(form-type input) ,output-reference) ',(slot-definition-name slot))))))
                              (push iomap child-iomaps)
                              (setf (slot-value-using-class class clone slot) (output-of iomap))))))))
         (make-iomap/compound projection recursion input input-reference output output-reference
                               (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps))))))))

;;;;;;
;;; Reader

(def reader copying (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (bind ((selection (tree-replace (selection-of (input-of document-iomap)) '(the document document) `(the document ,(input-reference-of document-iomap)))))
    (iter (for index :from 0 :below (length (class-slots (class-of (input-of projection-iomap)))))
          (for child-iomap = (elt (child-iomaps-of projection-iomap) (1+ index)))
          (for child-operation =
               (when (tree-search selection (input-reference-of child-iomap))
                 (recurse-reader recursion printer-iomap child-iomap gesture-queue operation document-iomap)))
          (when child-operation
            (return child-operation)))))
