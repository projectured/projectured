;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection file-system/file->tree/leaf ()
  ())

(def projection file-system/directory->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/file-system/file->tree/leaf ()
  (make-projection 'file-system/file->tree/leaf))

(def (function e) make-projection/file-system/directory->tree/node ()
  (make-projection 'file-system/directory->tree/node))

;;;;;;
;;; Construction

(def (macro e) file-system/file->tree/leaf ()
  '(make-projection/file-system/file->tree/leaf))

(def (macro e) file-system/directory->tree/node ()
  '(make-projection/file-system/directory->tree/node))

;;;;;;
;;; Printer

(def printer file-system/file->tree/leaf (projection recursion input input-reference)
  (bind ((pathname (pathname-of input))
         (output (tree/leaf ()
                   (text/text ()
                     ;; TODO: icon
                     (image/image (asdf:system-relative-pathname :projectured "etc/file.png"))
                     (text/string (string+ (pathname-name pathname) "." (pathname-type (pathname-of input))) :font *font/ubuntu/regular/18* :font-color *color/solarized/blue*)))))
    (make-iomap/compound projection recursion input input-reference output)))

(def printer file-system/directory->tree/node (projection recursion input input-reference)
  (bind ((pathname (pathname-of input))
         (output (make-tree/node (list* (tree/leaf ()
                                          (text/text ()
                                            ;; TODO: icon
                                            (image/image (asdf:system-relative-pathname :projectured "etc/directory.png"))
                                            (text/string (last-elt (pathname-directory pathname)) :font *font/ubuntu/regular/18* :font-color *color/solarized/red*)))
                                        (iter (for index :from 0)
                                              (for element :in-sequence (elements-of input))
                                              (for element-iomap = (recurse-printer recursion element
                                                                                    `((elt (the sequence (elements-of document)) ,index)
                                                                                      ,@(typed-reference (form-type input) input-reference))))
                                              (setf (indentation-of (output-of element-iomap)) 1)
                                              ;; TODO: iomap
                                              (collect (output-of element-iomap)))))))
    (make-iomap/compound projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader file-system/file->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion operation))
  (document/read-operation (input-of printer-iomap) (gesture-of input)))

(def reader file-system/directory->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion operation))
  (document/read-operation (input-of printer-iomap) (gesture-of input)))
