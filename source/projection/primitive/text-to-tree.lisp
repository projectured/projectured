;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) text/text->tree/node ()
  ())

(def (projection e) text/paragraph->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text/text->tree/node ()
  (make-projection 'text/text->tree/node))

(def (function e) make-projection/text/paragraph->tree/node ()
  (make-projection 'text/paragraph->tree/node))

;;;;;;
;;; Printer

(def printer text/text->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                       (push element-iomap child-iomaps)
                                       ;; KLUDGE:
                                       ;;(setf (indentation-of (output-of element-iomap)) 0)
                                       (collect (output-of element-iomap))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

(def printer text/paragraph->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                       (push element-iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of element-iomap)) 0)
                                       (collect (output-of element-iomap))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader text/text->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap gesture-queue))
  (operation/read-backward operation projection-iomap document-iomap))

(def reader text/paragraph->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap gesture-queue))
  (operation/read-backward operation projection-iomap document-iomap))
