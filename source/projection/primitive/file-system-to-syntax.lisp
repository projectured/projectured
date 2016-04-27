;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection file-system/file->syntax/leaf ()
  ())

(def projection file-system/directory->syntax/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/file-system/file->syntax/leaf ()
  (make-projection 'file-system/file->syntax/leaf))

(def function make-projection/file-system/directory->syntax/node ()
  (make-projection 'file-system/directory->syntax/node))

;;;;;;
;;; Construction

(def macro file-system/file->syntax/leaf ()
  '(make-projection/file-system/file->syntax/leaf))

(def macro file-system/directory->syntax/node ()
  '(make-projection/file-system/directory->syntax/node))

;;;;;;
;;; Forward mapper

(def forward-mapper file-system/file->syntax/leaf ()
  (reference-case -reference-
    (((the file-system/file document))
     '((the syntax/leaf document)))
    (((the pathname (pathname-of (the file-system/file document)))
      (the string (file-namestring (the pathname document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper file-system/directory->syntax/node ()
  (reference-case -reference-
    (((the file-system/directory document))
     '((the syntax/node document)))
    (((the sequence (elements-of (the file-system/directory document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) (1+ ?index))))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of element-iomap)) (elt (the sequence document) ,(1+ ?index))))
               ?rest
               element-iomap)))))

;;;;;;
;;; Backward mapper

(def backward-mapper file-system/file->syntax/leaf ()
  (reference-case -reference-
    (((the syntax/leaf document))
     '((the file-system/file document)))
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the pathname (pathname-of (the file-system/file document)))
       (the string (file-namestring (the pathname document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))))

(def backward-mapper file-system/directory->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     '((the file-system/directory document)))
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (when (> ?index 0)
       (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) (1- ?index))))
         (values `((the sequence (elements-of (the file-system/directory document)))
                   (the ,(document-type (input-of element-iomap)) (elt (the sequence document) ,(1- ?index))))
                 ?rest
                 element-iomap))))))

;;;;;;
;;; Printer

(def printer file-system/file->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         ;; (text/graphics (image/file () (resource-pathname "image/file.png")))
                         (text/string (string+ " " (file-namestring (pathname (pathname-of -input-)))) :font *font/liberation/sans/regular/24* :font-color *color/solarized/blue*))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer file-system/directory->syntax/node ()
  (bind ((element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence (elements-of -input-))
                                   (for element-iomap = (recurse-printer -recursion- element
                                                                         `((elt (the sequence (elements-of document)) ,index)
                                                                           ,@(typed-reference (document-type -input-) -input-reference-))))
                                   (collect element-iomap))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (list* (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                  ;; (text/graphics (image/file () (resource-pathname "image/directory.png")))
                                                  (text/string (string+ " " (last-elt (pathname-directory (pathname-of -input-)))) :font *font/liberation/sans/regular/24* :font-color *color/solarized/red*)))
                                              (mapcar (lambda (element-iomap)
                                                        (syntax/indentation (:indentation 2)
                                                          (output-of element-iomap)))
                                                      (va element-iomaps)))
                                       :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

;;;;;;
;;; Reader

(def reader file-system/file->syntax/leaf ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader file-system/directory->syntax/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))
