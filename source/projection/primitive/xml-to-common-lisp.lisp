;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection xml/text->common-lisp/application ()
  ())

(def projection xml/attribute->common-lisp/progn ()
  ())

(def projection xml/element->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def function make-projection/xml/text->common-lisp/application ()
  (make-projection 'xml/text->common-lisp/application))

(def function make-projection/xml/attribute->common-lisp/progn ()
  (make-projection 'xml/attribute->common-lisp/progn))

(def function make-projection/xml/element->common-lisp/progn ()
  (make-projection 'xml/element->common-lisp/progn))

;;;;;;
;;; Construction

(def macro xml/text->common-lisp/application ()
  '(make-projection/xml/text->common-lisp/application))

(def macro xml/attribute->common-lisp/progn ()
  '(make-projection/xml/attribute->common-lisp/progn))

(def macro xml/element->common-lisp/progn ()
  '(make-projection/xml/element->common-lisp/progn))

;;;;;;
;;; Forward mapper

(def function forward-mapper/xml/text->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (value-of (the xml/text document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (subseq (the string document) ,?start-index ,?end-index))
         (the string (value-of (the common-lisp/constant document)))
         (the common-lisp/constant (elt (the sequence document) 0))
         (the sequence (arguments-of (the common-lisp/application document)))))
      (((the common-lisp/application (printer-output (the xml/text document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/xml/attribute->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the common-lisp/application (printer-output (the xml/attribute document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/xml/element->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (xml/start-tag (the xml/element document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (subseq (the string document) ,(1+ ?start-index) ,(1+ ?end-index)))
         (the string (value-of (the common-lisp/constant document)))
         (the common-lisp/constant (elt (the sequence document) 0))
         (the sequence (arguments-of (the common-lisp/application document)))
         (the common-lisp/application (elt (the sequence document) 0))
         (the sequence (body-of (the common-lisp/progn document)))))
      (((the sequence (children-of (the xml/element document)))
        (the ?element-type (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((body-iomap (elt (child-iomaps-of printer-iomap) ?element-index))
              (body-element (output-of body-iomap)))
         (values `((the ,(form-type body-element) (elt (the sequence document) ,(+ ?element-index (if (attributes-of printer-input) 4 2))))
                   (the sequence (body-of (the common-lisp/progn document))))
                 (reverse ?rest)
                 body-iomap)))
      (((the common-lisp/application (printer-output (the xml/element document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/xml/text->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the sequence (arguments-of (the common-lisp/application document)))
        (the common-lisp/constant (elt (the sequence document) 0))
        (the string (value-of (the common-lisp/constant document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (subseq (the string document) ,?start-index ,?end-index))
         (the string (value-of (the xml/text document)))))
      (?
       (append reference `((the common-lisp/application (printer-output (the xml/text document) ,projection ,recursion))))))))

(def function backward-mapper/xml/attribute->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (?
       (append reference `((the common-lisp/application (printer-output (the xml/attribute document) ,projection ,recursion))))))))

(def function backward-mapper/xml/element->common-lisp/application (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the sequence (body-of (the common-lisp/progn document)))
        (the common-lisp/application (elt (the sequence document) 0))
        (the sequence (arguments-of (the common-lisp/application document)))
        (the common-lisp/constant (elt (the sequence document) 0))
        (the string (value-of (the common-lisp/constant document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (if (> ?start-index 0)
           `((the string (subseq (the string document) ,(1- ?start-index) ,(1- ?end-index)))
             (the string (xml/start-tag (the xml/element document))))
           (append reference `((the common-lisp/application (printer-output (the xml/element document) ,projection ,recursion))))))
      (((the sequence (body-of (the common-lisp/progn document)))
        (the ?type (elt (the sequence document) ?body-element-index))
        . ?rest)
       (bind ((element-index (- ?body-element-index (if (attributes-of printer-input) 4 2))))
         (if (<= 0 element-index (1- (length (children-of printer-input))))
             (bind ((element-iomap (elt (child-iomaps-of printer-iomap) element-index))
                    (element (elt (children-of printer-input) element-index)))
               (values `((the ,(form-type element) (elt (the sequence document) ,element-index))
                         (the sequence (children-of (the xml/element document))))
                       (reverse ?rest)
                       element-iomap))
             (append reference `((the common-lisp/application (printer-output (the xml/element document) ,projection ,recursion)))))))
      (?
       (append reference `((the common-lisp/application (printer-output (the xml/element document) ,projection ,recursion))))))))

;;;;;;
;;; Printer

(def printer xml/text->common-lisp/application (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/xml/text->common-lisp/application)))
         (output (as (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                   (list (make-common-lisp/constant (value-of input) :selection (as (butlast (va output-selection) 2))))
                                                   :selection output-selection))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer xml/attribute->common-lisp/progn (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/xml/attribute->common-lisp/application)))
         (output (as (make-common-lisp/progn (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (name-of input)))
                                                                                 :selection (as (butlast (va output-selection) 2)))
                                                   (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "=\"")))
                                                   (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (value-of input)))
                                                                                 :selection (as (butlast (va output-selection) 2)))
                                                   (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant "\""))))
                                             :selection output-selection))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer xml/element->common-lisp/progn (projection recursion input input-reference)
  (bind ((attribute-iomaps (as (iter (for attribute :in-sequence (attributes-of input))
                                     (for attribute-index :from 0)
                                     (collect (recurse-printer recursion attribute
                                                               `((elt (the sequence document) ,attribute-index)
                                                                 (the sequence (attributes-of document))
                                                                 ,@(typed-reference (form-type input) input-reference)))))))
         (child-iomaps (as (iter (for child :in-sequence (children-of input))
                                 (for child-index :from 0)
                                 (for child-iomap = (recurse-printer recursion child
                                                                     `((elt (the sequence document) ,child-index)
                                                                       (the sequence (children-of document))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                                 (collect child-iomap))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil child-iomaps)
                                                (selection-of input)
                                                'forward-mapper/xml/element->common-lisp/application)))
         (output (as (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                         (list (make-common-lisp/constant (string+ "<" (name-of input))
                                                                                                                          :selection (as (butlast (va output-selection) 4))))
                                                                                         :selection (as (butlast (va output-selection) 2))))
                                                     (iter (for attribute-iomap :in (va attribute-iomaps))
                                                           (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant " "))))
                                                           (collect (output-of attribute-iomap)))
                                                     (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant ">"))))
                                                     (iter (for child-iomap :in (va child-iomaps))
                                                           (collect (output-of child-iomap))
                                                           #+nil(collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string #\NewLine))))))
                                                     (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (string+ "</" (name-of input) ">"))))))
                                             :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output child-iomaps)))

;;;;;;
;;; Reader

(def reader xml/text->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/xml/text->common-lisp/application nil)
                  (make-command/nothing (gesture-of input))))

(def reader xml/attribute->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/xml/attribute->common-lisp/application nil)
                  (make-command/nothing (gesture-of input))))

(def reader xml/element->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/xml/element->common-lisp/application nil)
                  (make-command/nothing (gesture-of input))))
