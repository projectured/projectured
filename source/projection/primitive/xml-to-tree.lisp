;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) xml/text->string ()
  ())

(def (projection e) xml/attribute->tree/node ()
  ())

(def (projection e) xml/element->tree/node ()
  ())

(def (projection e) xml->tree ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/xml/text->string ()
  (make-projection 'xml/text->string))

(def (function e) make-projection/xml/attribute->tree/node ()
  (make-projection 'xml/attribute->tree/node))

(def (function e) make-projection/xml/element->tree/node ()
  (make-projection 'xml/element->tree/node))

;;;;;;
;;; Construction

(def (macro e) xml/text->string ()
  '(make-projection/xml/text->string))

(def (macro e) xml/attribute->tree/node ()
  '(make-projection/xml/attribute->tree/node))

(def (macro e) xml/element->tree/node ()
  '(make-projection/xml/element->tree/node))

;;;;;;
;;; Printer

(def printer xml/text->string (projection recursion iomap input input-reference output-reference)
  (bind ((output (text-of input)))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer xml/attribute->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((name (name-of input))
         (value (value-of input))
         (output (make-tree/node (list name value)))
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (name-reference `(elt (the list (children-of (the tree/node ,output-reference))) 0))
         (value-reference `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string name `(name-of ,typed-input-reference) 0
                                                   name name-reference 0
                                                   (length name))
                                (make-iomap/object projection recursion name `(name-of ,typed-input-reference)
                                                   name name-reference)
                                (make-iomap/string value `(value-of ,typed-input-reference) 0
                                                   value value-reference 0
                                                   (length value))
                                (make-iomap/object projection recursion value `(value-of ,typed-input-reference)
                                                   value value-reference)))))

(def printer xml/element->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (name (name-of input))
         (attributes (attributes-of input))
         (children (children-of input))
         (output (make-tree/node (append (list (prog1 (make-tree/leaf name
                                                                      :opening-delimiter "<"
                                                                      :closing-delimiter (unless attributes (if children ">" "/>")))
                                                 (push (make-iomap/object* projection recursion input `(the string (start-tag ,typed-input-reference))
                                                                           name `(the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))
                                                       child-iomaps)
                                                 (push (make-iomap/string* name `(the string (start-tag ,typed-input-reference)) 0
                                                                           name `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))) 0
                                                                           (length name))
                                                       child-iomaps)))
                                         (when attributes
                                           (list (prog1-bind output
                                                     (make-tree/node (iter (for attribute :in attributes)
                                                                           (for attribute-index :from 0)
                                                                           (for iomap = (recurse-printer recursion iomap attribute
                                                                                                         `(elt (the list (attributes-of ,typed-input-reference)) ,attribute-index)
                                                                                                         `(elt (the list (children-of (the tree/node (elt (the list (children-of (the tree/node ,output-reference))) ,1)))) ,attribute-index)))
                                                                           (push iomap child-iomaps)
                                                                           (collect (output-of iomap)))
                                                                     :closing-delimiter (if children ">" "/>")
                                                                     :separator " ")
                                                   (push (make-iomap/object* projection recursion input `(the list (attributes-of ,typed-input-reference))
                                                                             output `(the tree/node (elt (the list (children-of (the tree/node ,output-reference))) 1)))
                                                         child-iomaps))))
                                         (when children
                                           (append (prog1-bind output
                                                       (iter (for child :in children)
                                                             (for child-index :from 0)
                                                             (for iomap = (recurse-printer recursion iomap child
                                                                                           `(elt (the list (children-of ,typed-input-reference)) ,child-index)
                                                                                           `(elt (the list (children-of (the tree/node ,output-reference))) ,(+ child-index (if attributes 2 1)))))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap))))
                                                   (prog1 (list (make-tree/leaf name :opening-delimiter "</" :closing-delimiter ">"))
                                                     (push (make-iomap/object* projection recursion input `(the string (end-tag ,typed-input-reference))
                                                                               name `(the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) ,(+ (length children) (if attributes 2 1)))))
                                                           child-iomaps)
                                                     (push (make-iomap/string* name `(the string (end-tag ,typed-input-reference)) 0
                                                                               name `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) ,(+ (length children) (if attributes 2 1)))))) 0
                                                                               (length name))
                                                           child-iomaps))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader xml/text->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader xml/attribute->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader xml/element->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
