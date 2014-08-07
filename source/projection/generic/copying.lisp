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
    #+nil
    (null (make-iomap/object projection recursion input input-reference input))
    (symbol (make-iomap/object projection recursion input input-reference input))
    (number (make-iomap/object projection recursion input input-reference input))
    (string (make-iomap/object projection recursion input input-reference input))
    (pathname (make-iomap/object projection recursion input input-reference input))
    (style/color (make-iomap/object projection recursion input input-reference input))
    (style/font (make-iomap/object projection recursion input input-reference input))
    (sequence
     (bind ((element-iomaps (iter (for index :from 0)
                                  (for element :in-sequence input)
                                  (collect (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                                ,@(typed-reference (form-type input) input-reference))))))
            (output (etypecase input
                      (list (mapcar 'output-of element-iomaps))
                      (sequence/sequence ;; KLUDGE: typechecking fails in SBCL sequence/
                       (make-sequence/sequence (mapcar 'output-of element-iomaps) :selection (selection-of input)))
                      (sequence (mapcar 'output-of element-iomaps)))))
       (make-iomap/compound projection recursion input input-reference output element-iomaps)))
    #+nil
    (cons
     (bind ((car-iomap (recurse-printer recursion (car input) `((car (the sequence document))
                                                                ,@(typed-reference (form-type input) input-reference))))
            (cdr-iomap (recurse-printer recursion (cdr input) `((cdr (the sequence document))
                                                                ,@(typed-reference (form-type input) input-reference))))
            (output (cons (output-of car-iomap) (output-of cdr-iomap))))
       (make-iomap/compound projection recursion input input-reference output
                            (list (make-iomap/object projection recursion input input-reference output) car-iomap cdr-iomap))))
    (standard-object
     (bind ((class (class-of input))
            (slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
            (slot-value-iomaps (iter (for slot :in slots)
                                     (when (slot-boundp-using-class class input slot)
                                       (bind ((slot-reader (find-slot-reader class slot))
                                              (slot-value (slot-value-using-class class input slot)))
                                         (collect (recurse-printer recursion slot-value
                                                                   (if slot-reader
                                                                       `((,slot-reader (the ,(form-type input) document))
                                                                         ,@(typed-reference (form-type input) input-reference))
                                                                       `((slot-value (the ,(form-type input) document) ',(slot-definition-name slot))
                                                                         ,@(typed-reference (form-type input) input-reference)))))))))
            (output (prog1-bind clone (allocate-instance class)
                      (when (typep input 'document)
                        (setf (selection-of clone) (selection-of input)))
                      (iter (for slot :in slots)
                            (for slot-value-iomap :in slot-value-iomaps)
                            (setf (slot-value-using-class class clone slot) (output-of slot-value-iomap))))))
       (make-iomap 'iomap/copying
                   :projection projection :recursion recursion
                   :input input :output output
                   :slot-value-iomaps slot-value-iomaps)))))

;;;;;;
;;; Reader

(def reader copying (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (class (class-of printer-input)))
    (make-command (gesture-of input)
                  (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/functional operation)
                               (operation/replace-selection
                                (etypecase printer-input
                                  (number
                                   (awhen (pattern-case (reverse (selection-of operation))
                                            (((the string (write-to-string (the number document)))
                                              (the string (subseq (the string document) ?start-index ?end-index)))
                                             `((the string (subseq (the string document) ,?start-index ,?end-index))
                                               (the string (write-to-string (the number document))))))
                                     (make-operation/replace-selection printer-input it)))
                                  (string
                                   (awhen (pattern-case (reverse (selection-of operation))
                                            (((the string (subseq (the string document) ?start-index ?end-index)))
                                             `((the string (subseq (the string document) ,?start-index ,?end-index)))))
                                     (make-operation/replace-selection printer-input it)))
                                  (sequence
                                   (awhen (pattern-case (reverse (selection-of operation))
                                            (((the ?type (elt (the sequence document) ?index)) . ?rest)
                                             (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?index))
                                                    (input-element-operation (make-operation/replace-selection (input-of element-iomap) (butlast (selection-of operation))))
                                                    (output-element-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-element-operation :domain (domain-of input) :description (description-of input)) element-iomap))))
                                               (when (typep output-element-operation 'operation/replace-selection)
                                                 (append (selection-of output-element-operation) (last (selection-of operation))))))
                                            (?a
                                             (selection-of operation)))
                                     (make-operation/replace-selection printer-input it)))
                                  (standard-object
                                   (awhen (pattern-case (reverse (selection-of operation))
                                            (((the ?type (?reader (the ?input-type document))) . ?rest)
                                             (bind ((slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
                                                    (slot-index (position ?reader slots :key (curry 'find-slot-reader class)))
                                                    (slot-value-iomap (elt (slot-value-iomaps-of printer-iomap) slot-index))
                                                    (input-slot-value-operation (make-operation/replace-selection (input-of slot-value-iomap) (butlast (selection-of operation))))
                                                    (output-slot-value-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-slot-value-operation :domain (domain-of input) :description (description-of input)) slot-value-iomap))))
                                               (when (typep output-slot-value-operation 'operation/replace-selection)
                                                 (append (selection-of output-slot-value-operation) (last (selection-of operation))))))
                                            (?a
                                             (selection-of operation)))
                                     (make-operation/replace-selection printer-input it)))
                                  (t
                                   (not-yet-implemented))))
                               (operation/sequence/replace-element-range
                                (etypecase printer-input
                                  (string
                                   (awhen (pattern-case (reverse (target-of operation))
                                            (((the string (subseq (the string document) ?start-index ?end-index)))
                                             `((the string (subseq (the string document) ,?start-index ,?end-index)))))
                                     (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                                  (text/text
                                   (awhen (pattern-case (reverse (target-of operation))
                                            (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                             `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))))
                                     (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                                  (sequence
                                   (pattern-case (reverse (target-of operation))
                                     (((the ?type (elt (the sequence document) ?index)) . ?rest)
                                      (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?index))
                                             (input-element-operation (make-operation/sequence/replace-element-range (input-of element-iomap) (butlast (target-of operation)) (replacement-of operation)))
                                             (output-element-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-element-operation :domain (domain-of input) :description (description-of input)) element-iomap))))
                                        (typecase output-element-operation
                                          (operation/sequence/replace-element-range
                                           (make-operation/sequence/replace-element-range printer-input (append (target-of output-element-operation) (last (target-of operation))) (replacement-of operation)))
                                          (operation/number/replace-range
                                           (make-operation/number/replace-range printer-input (append (target-of output-element-operation) (last (target-of operation))) (replacement-of operation))))))))
                                  (standard-object
                                   (pattern-case (reverse (target-of operation))
                                     (((the ?type (?reader (the ?input-type document))) . ?rest)
                                      (bind ((slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
                                             (slot-index (position ?reader slots :key (curry 'find-slot-reader class)))
                                             (slot-value-iomap (elt (slot-value-iomaps-of printer-iomap) slot-index))
                                             (input-slot-value-operation (make-operation/sequence/replace-element-range (input-of slot-value-iomap) (butlast (target-of operation)) (replacement-of operation)))
                                             (output-slot-value-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-slot-value-operation :domain (domain-of input) :description (description-of input)) slot-value-iomap))))
                                        (typecase output-slot-value-operation
                                          (operation/sequence/replace-element-range
                                           (make-operation/sequence/replace-element-range printer-input (append (target-of output-slot-value-operation) (last (target-of operation))) (replacement-of operation)))
                                          (operation/number/replace-range
                                           (make-operation/number/replace-range printer-input (append (target-of output-slot-value-operation) (last (target-of operation))) (replacement-of operation))))))))
                                  (t
                                   (not-yet-implemented))))
                               (operation/number/replace-range
                                (etypecase printer-input
                                  (number
                                   (awhen (pattern-case (reverse (target-of operation))
                                            (((the string (write-to-string (the number document)))
                                              (the string (subseq (the string document) ?start-index ?end-index)))
                                             `((the string (subseq (the string document) ,?start-index ,?end-index))
                                               (the string (write-to-string (the number document))))))
                                     (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                  (sequence
                                   (awhen (pattern-case (reverse (target-of operation))
                                            (((the ?type (elt (the sequence document) ?index)) . ?rest)
                                             (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?index))
                                                    (input-element-operation (make-operation/number/replace-range (input-of element-iomap) (butlast (target-of operation)) (replacement-of operation)))
                                                    (output-element-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-element-operation :domain (domain-of input) :description (description-of input)) element-iomap))))
                                               (when (typep output-element-operation 'operation/number/replace-range)
                                                 (append (target-of output-element-operation)
                                                         (last (target-of operation)))))))
                                     (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                  (standard-object
                                   (awhen (pattern-case (reverse (target-of operation))
                                            (((the ?type (?reader (the ?input-type document))) . ?rest)
                                             (bind ((slots (remove-if (lambda (slot) (member (slot-definition-name slot) '(selection))) (class-slots class)))
                                                    (slot-index (position ?reader slots :key (curry 'find-slot-reader class)))
                                                    (slot-value-iomap (elt (slot-value-iomaps-of printer-iomap) slot-index))
                                                    (input-slot-value-operation (make-operation/number/replace-range (input-of slot-value-iomap) (butlast (target-of operation)) (replacement-of operation)))
                                                    (output-slot-value-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-slot-value-operation :domain (domain-of input) :description (description-of input)) slot-value-iomap))))
                                               (when (typep output-slot-value-operation 'operation/number/replace-range)
                                                 (append (target-of output-slot-value-operation)
                                                         (last (target-of operation)))))))
                                     (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                  (t
                                   (not-yet-implemented))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (recurse (operation-of input)))
                  :domain (domain-of input)
                  :description (description-of input))))
