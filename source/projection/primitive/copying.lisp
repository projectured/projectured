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

(def printer copying (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
    (etypecase input
      (symbol (make-iomap/object projection recursion input input-reference input output-reference))
      (number (make-iomap/object projection recursion input input-reference input output-reference))
      (string (make-iomap/recursive projection recursion input input-reference input output-reference
                                    (list (make-iomap/object projection recursion input input-reference input output-reference)
                                          (make-iomap/string input input-reference 0 input output-reference 0 (length input)))))
      (cons
       (bind ((car-iomap (recurse-printer recursion (car input) `(car ,typed-input-reference) `(car (the ,(form-type input) ,output-reference))))
              (cdr-iomap (recurse-printer recursion (cdr input) `(cdr ,typed-input-reference) `(cdr (the ,(form-type input) ,output-reference))))
              (output (cons (output-of car-iomap) (output-of cdr-iomap))))
         (make-iomap/recursive projection recursion input input-reference output output-reference
                               (list (make-iomap/object projection recursion input input-reference output output-reference) car-iomap cdr-iomap))))
      (standard-object
       (bind ((class (class-of input))
              (child-iomaps nil)
              (output (prog1-bind clone (allocate-instance class)
                        (dolist (slot (class-slots class))
                          (bind ((iomap (recurse-printer recursion (slot-value-using-class class input slot) `(slot-value ,typed-input-reference ,(slot-definition-name slot)) output-reference)))
                            (push iomap child-iomaps)
                            (setf (slot-value-using-class class clone slot) (output-of iomap)))))))
         (make-iomap/recursive projection recursion input input-reference output output-reference
                               (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps))))))))

;;;;;;
;;; Reader

(def reader copying (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
