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

(def function make-projection/copying ()
  (make-projection 'copying))

;;;;;;
;;; Construction

(def macro copying ()
  '(make-projection/copying))

;;;;;;
;;; IO map

(def iomap iomap/copying ()
  ((slot-value-iomaps :type sequence)))

;;;;;;
;;; Printer

(def printer copying (projection recursion input input-reference)
  (etypecase input
    ((and symbol (not null)) (make-iomap/object projection recursion input input-reference input))
    (number (make-iomap/object projection recursion input input-reference input))
    (string (make-iomap/object projection recursion input input-reference input))
    (pathname (make-iomap/object projection recursion input input-reference input))
    (style/color (make-iomap/object projection recursion input input-reference input))
    (style/font (make-iomap/object projection recursion input input-reference input))
    (sequence
     (bind ((element-iomaps (as (map-ll* (ll input) (lambda (element index)
                                                      (recurse-printer recursion (value-of element) `((elt (the sequence document) ,index)
                                                                                                      ,@(typed-reference (form-type input) input-reference)))))))
            (output (as (etypecase input
                          (document/sequence ;; KLUDGE: typechecking fails in SBCL sequence/
                              (make-document/sequence (map-ll (va element-iomaps) 'output-of) :selection (selection-of input)))
                          (sequence (map-ll (va element-iomaps) 'output-of))))))
       (make-iomap/compound projection recursion input input-reference output element-iomaps)))
    (standard-object
     (bind ((class (class-of input))
            (slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
            (slot-value-iomaps (as (iter (for slot :in slots)
                                         (collect (rebind (slot)
                                                    (as (when (slot-boundp-using-class class input slot)
                                                          (bind ((slot-reader (find-slot-reader class slot))
                                                                 (slot-value (slot-value-using-class class input slot)))
                                                            (recurse-printer recursion slot-value
                                                                             (if slot-reader
                                                                                 `((,slot-reader (the ,(form-type input) document))
                                                                                   ,@(typed-reference (form-type input) input-reference))
                                                                                 `((slot-value (the ,(form-type input) document) ',(slot-definition-name slot))
                                                                                   ,@(typed-reference (form-type input) input-reference))))))))))))
            (output (as (prog1-bind clone (allocate-instance class)
                          (when (typep input 'document)
                            (setf (selection-of clone) (as (selection-of input))))
                          (iter (for slot :in slots)
                                (for slot-value-iomap :in (va slot-value-iomaps))
                                (rebind (slot-value-iomap)
                                  (setf (slot-value-using-class class clone slot) (as (output-of (va slot-value-iomap))))))))))
       (make-iomap 'iomap/copying
                   :projection projection :recursion recursion
                   :input input :output output
                   :slot-value-iomaps slot-value-iomaps)))))

;;;;;;
;;; Reader

(def reader copying (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap
                                         (lambda (printer-iomap selection)
                                           (bind ((printer-input (input-of printer-iomap))
                                                  (reverse-selection (reverse selection)))
                                             (etypecase printer-input
                                               (number selection)
                                               (string selection)
                                               (sequence
                                                (pattern-case reverse-selection
                                                  (((the ?type (elt (the sequence document) ?index)) . ?rest)
                                                   (values (list (first reverse-selection)) (reverse ?rest) (elt (child-iomaps-of printer-iomap) ?index)))
                                                  (?a selection)))
                                               (standard-object
                                                (pattern-case reverse-selection
                                                  (((the ?type (?reader (the ?input-type document))) . ?rest)
                                                   (bind ((class (class-of printer-input))
                                                          (slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
                                                          (slot-index (position ?reader slots :key (curry 'find-slot-reader class)))
                                                          (slot-value-iomap (va (elt (slot-value-iomaps-of printer-iomap) slot-index))))
                                                     (values (list (first reverse-selection)) (reverse ?rest) slot-value-iomap)))
                                                  (?a selection))))))
                                         nil)
                  (make-command/nothing (gesture-of input))))
