;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document file-system/base ()
  ())

(def document file-system/file (file-system/base)
  ((pathname :type string)
   (open-target :type widget/base)))

(def document file-system/directory (file-system/base)
  ((pathname :type string)
   (elements :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-file-system/file (pathname open-target)
  (make-instance 'file-system/file :pathname pathname :open-target open-target))

(def (function e) make-file-system/directory (pathname elements)
  (make-instance 'file-system/directory :pathname pathname :elements elements))

;;;;;;
;;; Construction

(def (macro e) file-system/file (pathname open-target)
  `(make-file-system/file ,pathname ,open-target))

(def (macro e) file-system/directory (pathname &body elements)
  `(make-file-system/directory ,pathname (list ,@elements)))

;;;;;;
;;; API

(def (function e) make-file-system/pathname (pathname open-target)
  (if (uiop/pathname::directory-pathname-p pathname)
      (make-file-system/directory pathname
                                  (iter (for file :in-sequence (directory (merge-pathnames pathname "*.*")))
                                        (collect (make-file-system/pathname file open-target))))
      (make-file-system/file pathname open-target)))
