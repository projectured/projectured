;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection copying ()
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
;;; IO map

(def iomap iomap/copying (iomap)
  ((slot-value-iomaps :type sequence)))

;;;;;;
;;; Printer

(def printer copying (projection recursion input input-reference)
  (etypecase input
    (null (make-iomap/object projection recursion input input-reference input))
    (symbol (make-iomap/object projection recursion input input-reference input))
    (number (make-iomap/object projection recursion input input-reference input))
    (string (make-iomap/object projection recursion input input-reference input))
    (pathname (make-iomap/object projection recursion input input-reference input))
    (style/color (make-iomap/object projection recursion input input-reference input))
    (style/font (make-iomap/object projection recursion input input-reference input))
    (cons
     (bind ((car-iomap (recurse-printer recursion (car input) `((car (the list document))
                                                                ,@(typed-reference (form-type input) input-reference))))
            (cdr-iomap (recurse-printer recursion (cdr input) `((cdr (the list document))
                                                                ,@(typed-reference (form-type input) input-reference))))
            (output (cons (output-of car-iomap) (output-of cdr-iomap))))
       (make-iomap/compound projection recursion input output
                            (list (make-iomap/object projection recursion input input-reference output) car-iomap cdr-iomap))))
    (standard-object
     (bind ((class (class-of input))
            (slot-value-iomaps (iter (for slot :in (class-slots class))
                                     (when (slot-boundp-using-class class input slot)
                                       (bind ((direct-slot (some (lambda (super) (find-direct-slot super (slot-definition-name slot) :otherwise nil)) (class-precedence-list class)))
                                              (slot-reader (first (slot-definition-readers direct-slot)))
                                              (slot-value (slot-value-using-class class input slot)))
                                         (collect (recurse-printer recursion slot-value
                                                                   (if slot-reader
                                                                       `((,slot-reader (the ,(form-type input) document))
                                                                         ,@(typed-reference (form-type input) input-reference))
                                                                       `((slot-value (the ,(form-type input) document) ',(slot-definition-name slot))
                                                                         ,@(typed-reference (form-type input) input-reference)))))))))
            (output (prog1-bind clone (allocate-instance class)
                      (iter (for slot :in (class-slots class))
                            (for slot-value-iomap :in slot-value-iomaps)
                            (setf (slot-value-using-class class clone slot) (output-of slot-value-iomap))))))
       (make-iomap 'iomap/copying
                   :projection projection :recursion recursion
                   :input input :output output
                   :slot-value-iomaps slot-value-iomaps)))))

;;;;;;
;;; Reader

(def reader copying (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection))
  (labels ((recurse (operation)
             (typecase operation
               (operation/quit operation)
               (operation/replace-selection
                (etypecase (input-of projection-iomap)
                  (standard-object
                   (bind ((slot-index 1) ;; TODO:
                          (slot-value-iomap (elt (slot-value-iomaps-of projection-iomap) slot-index))
                          (input-slot-value-operation (make-operation/replace-selection (input-of slot-value-iomap) (butlast (selection-of operation)))))
                     (make-operation/replace-selection (input-of projection-iomap)
                                                       (append (selection-of (recurse-reader recursion slot-value-iomap gesture-queue input-slot-value-operation))
                                                               (last (selection-of operation))))))))
               (operation/sequence/replace-element-range)
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations)))))))
    (recurse operation))
  #+nil
  (bind ((selection (tree-replace (selection-of (input-of document-iomap)) '(the document document) `(the document ,(input-reference-of document-iomap)))))
    (or (iter (for index :from 0 :below (length (class-slots (class-of (input-of projection-iomap)))))
              (for child-iomap = (elt (child-iomaps-of projection-iomap) (1+ index)))
              (for child-operation =
                   (when (tree-search selection (input-reference-of child-iomap))
                     (recurse-reader recursion child-iomap gesture-queue operation)))
              (when child-operation
                (return child-operation)))
        (operation/read-backward operation projection-iomap document-iomap))))
