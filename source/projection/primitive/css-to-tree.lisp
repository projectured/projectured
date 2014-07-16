;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection css/attribute->tree/leaf ()
  ())

(def projection css/rule->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/css/attribute->tree/leaf ()
  (make-projection 'css/attribute->tree/leaf))

(def (function e) make-projection/css/rule->tree/node ()
  (make-projection 'css/rule->tree/node))

;;;;;;
;;; Printer

(def printer css/attribute->tree/leaf (projection recursion input input-reference)
  (bind ((output (tree/leaf ()
                   (text/text ()
                     (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)
                     (text/string ": " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                     (text/string (value-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                     (text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer css/rule->tree/node (projection recursion input input-reference)
  (bind ((attribute-iomaps (iter (for attribute :in-sequence (attributes-of input))
                                 (for attribute-index :from 0)
                                 (collect (recurse-printer recursion attribute
                                                           `((elt (the sequence document) ,attribute-index)
                                                             (the sequence (attributes-of document))
                                                             ,@(typed-reference (form-type input) input-reference))))))
         (output (make-tree/node (list (tree/leaf ()
                                         (text/text ()
                                           (text/string (selector-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (make-tree/node (iter (for attribute-iomap :in attribute-iomaps)
                                                             (setf (indentation-of (output-of attribute-iomap)) 2)
                                                             (collect (output-of attribute-iomap)))
                                                       :indentation 0
                                                       :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :closing-delimiter (text/text () (text/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader css/attribute->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader css/rule->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
