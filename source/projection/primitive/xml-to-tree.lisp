;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) xml/text->tree/leaf ()
  ())

(def (projection e) xml/attribute->tree/node ()
  ())

(def (projection e) xml/element->tree/node ()
  ())

(def (projection e) xml->tree ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/xml/text->tree/leaf ()
  (make-projection 'xml/text->tree/leaf))

(def (function e) make-projection/xml/attribute->tree/node ()
  (make-projection 'xml/attribute->tree/node))

(def (function e) make-projection/xml/element->tree/node ()
  (make-projection 'xml/element->tree/node))

;;;;;;
;;; Construction

(def (macro e) xml/text->tree/leaf ()
  '(make-projection/xml/text->tree/leaf))

(def (macro e) xml/attribute->tree/node ()
  '(make-projection/xml/attribute->tree/node))

(def (macro e) xml/element->tree/node ()
  '(make-projection/xml/element->tree/node))

;;;;;;
;;; Printer

;; TODO: rename projections ->tree/leaf

(def printer xml/text->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((text (text-of input))
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/leaf (make-text/string text :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string text `(text-of ,typed-input-reference) 0
                                                   text `(content-of (the text/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length text))))))

(def printer xml/attribute->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((name (name-of input))
         (value (value-of input))
         (name-leaf (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))
         (value-leaf (make-tree/leaf (make-text/string value :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                     :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                     :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
         (output (make-tree/node (list name-leaf value-leaf)
                                 :separator (make-text/string "=" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (name-reference `(elt (the list (children-of (the tree/node ,output-reference))) 0))
         (value-reference `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/object projection recursion
                                                   name `(name-of ,typed-input-reference)
                                                   name-leaf name-reference)
                                (make-iomap/string name `(name-of ,typed-input-reference) 0
                                                   name `(content-of (the text/string (content-of (the tree/leaf ,name-reference)))) 0
                                                   (length name))
                                (make-iomap/object projection recursion
                                                   value `(value-of ,typed-input-reference)
                                                   value-leaf value-reference)
                                (make-iomap/string value `(value-of ,typed-input-reference) 0
                                                   value `(content-of (the text/string (content-of (the tree/leaf ,value-reference)))) 0
                                                   (length value))))))

(def printer xml/element->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (name (name-of input))
         (attributes (attributes-of input))
         (children (children-of input))
         (deep-element (find-if (of-type 'xml/element) (children-of input)))
         (output (make-tree/node (append (list (prog1 (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)
                                                                      :opening-delimiter (make-text/string "<" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                      :closing-delimiter (unless attributes
                                                                                           (make-text/string (if children ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                 (push (make-iomap/object* projection recursion input `(the string (start-tag ,typed-input-reference))
                                                                           name `(the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))
                                                       child-iomaps)
                                                 (push (make-iomap/string name `(start-tag ,typed-input-reference) 0
                                                                          name `(content-of (the text/string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0))))) 0
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
                                                                     :closing-delimiter (make-text/string (if children ">" "/>") :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                     :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                   ;; TODO: this kills editing xml attribute names and values because it kicks in when mapping backward
                                                   #+nil
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
                                                             ;; KLUDGE:
                                                             (when deep-element
                                                               (setf (indentation-of (output-of iomap)) 2))
                                                             (collect (output-of iomap))))
                                                   (prog1 (list (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)
                                                                                :indentation (if deep-element 0 nil)
                                                                                :opening-delimiter (make-text/string "</" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                                :closing-delimiter (make-text/string ">" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                     (push (make-iomap/object* projection recursion input `(the string (end-tag ,typed-input-reference))
                                                                               name `(the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) ,(+ (length children) (if attributes 2 1)))))
                                                           child-iomaps)
                                                     (push (make-iomap/string name `(end-tag ,typed-input-reference) 0
                                                                              name `(content-of (the text/string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) ,(+ (length children) (if attributes 2 1))))))) 0
                                                                              (length name))
                                                           child-iomaps)))))
                                 :indentation 4
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader xml/text->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader xml/attribute->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader xml/element->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
