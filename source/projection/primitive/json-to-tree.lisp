;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) json/null->string ()
  ())

(def (projection e) json/boolean->string ()
  ())

(def (projection e) json/number->string ()
  ())

(def (projection e) json/string->string ()
  ())

(def (projection e) json/array->tree/node ()
  ())

(def (projection e) json/object->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/json/null->string ()
  (make-projection 'json/null->string))

(def (function e) make-projection/json/boolean->string ()
  (make-projection 'json/boolean->string))

(def (function e) make-projection/json/number->string ()
  (make-projection 'json/number->string))

(def (function e) make-projection/json/string->string ()
  (make-projection 'json/string->string))

(def (function e) make-projection/json/array->tree/node ()
  (make-projection 'json/array->tree/node))

(def (function e) make-projection/json/object->tree/node ()
  (make-projection 'json/object->tree/node))

;;;;;;
;;; Construction

(def (macro e) json/null->string ()
  '(make-projection/json/null->string))

(def (macro e) json/boolean->string ()
  '(make-projection/json/boolean->string))

(def (macro e) json/number->string ()
  '(make-projection/json/number->string))

(def (macro e) json/string->string ()
  '(make-projection/json/string->string))

(def (macro e) json/array->tree/node ()
  '(make-projection/json/array->tree/node))

(def (macro e) json/object->tree/node ()
  '(make-projection/json/object->tree/node))

;;;;;;
;;; Printer

(def printer json/null->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output "null")
         (name-reference `(value (the ,(form-type input) ,input-reference) ,output)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string ,name-reference) 0 output `(the string ,output-reference) 0 (length output))))))

(def printer json/boolean->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (boolean-to-string (value-p input)))
         (name-reference `(boolean-to-string (the boolean (value-p (the ,(form-type input) ,input-reference))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string ,name-reference) 0 output `(the string ,output-reference) 0 (length output))))))

(def printer json/number->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (write-to-string (value-of input)))
         (value-reference `(write-to-string (the number (value-of (the ,(form-type input) ,input-reference))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string ,value-reference) 0 output `(the string ,output-reference) 0 (length output))))))

(def printer json/string->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (string+ "\"" (text-of input) "\""))
         (text-reference `(text-of (the ,(form-type input) ,input-reference))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output text-reference 0 output output-reference 1 (length (text-of input)))))))

(def printer json/array->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for element :in-sequence (elements-of input))
                                       (for index :from 0)
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push element-iomap child-iomaps)
                                       (collect (output-of element-iomap))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer json/object->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for (key value) :in-hashtable (key-value-map-of input))
                                       (for index :from 0)
                                       (collect (bind ((entry-node-reference `(the tree/node (elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                                       (value-node-reference `(the list (children-of ,entry-node-reference)))
                                                       (iomap (recurse-printer recursion iomap value
                                                                               `(gethash ,key (key-value-map-of ,typed-input-reference))
                                                                               `(elt ,value-node-reference 1)))
                                                       (output-key (string+ "\"" key "\""))
                                                       (output (make-tree/node (list output-key (output-of iomap)))))
                                                  (push (make-iomap/object* projection recursion input `(the t (gethash-entry ,key ,typed-input-reference))
                                                                            output entry-node-reference)
                                                        child-iomaps)
                                                  (push (make-iomap/string* input `(the string (entry-key (gethash-entry ,key (key-value-map-of ,typed-input-reference)))) 0
                                                                            output-key `(the string (elt ,value-node-reference 0)) 0 (length output-key))
                                                        child-iomaps)
                                                  (push iomap child-iomaps)
                                                  output))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader json/null->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/boolean->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/number->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/string->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/array->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/object->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
