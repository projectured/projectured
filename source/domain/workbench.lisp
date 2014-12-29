;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document workbench/base ()
  ())

(def document workbench/workbench (workbench/base)
  ((navigator :type workbench/navigator)
   (console :type workbench/console)
   (editor :type worbench/editor)))

(def document workbench/navigator (workbench/base)
  ((folders :type sequence)))

(def document workbench/console (workbench/base)
  ((content :type t)))

(def document workbench/editor (workbench/base)
  ((documents :type sequence)))

(def document workbench/document (workbench/base)
  ((title :type string)
   (filename :type pathname)
   (content :type t)))

;;;;;;
;;; Construction

(def function workbench/make-workbench (navigator editor &key selection)
  (make-instance 'workbench/workbench :navigator navigator
                 :console (workbench/make-console (text/text () (text/string "Welcome to ProjecturEd!" :font *font/ubuntu/regular/14*)))
                 :editor editor :selection selection))

(def function workbench/make-navigator (folders &key selection)
  (make-instance 'workbench/navigator :folders folders :selection selection))

(def function workbench/make-console (content &key selection)
  (make-instance 'workbench/console :content content :selection selection))

(def function workbench/make-editor (documents &key selection)
  (make-instance 'workbench/editor :documents documents :selection selection))

(def function workbench/make-document (content &rest args &key title filename selection)
  (declare (ignore title filename selection))
  (apply #'make-instance 'workbench/document :content content args))

;;;;;;
;;; Construction

(def macro workbench/workbench ((&key selection) &body elements)
  `(workbench/make-workbench ,(first elements) ,(second elements) :selection ,selection))

(def macro workbench/navigator ((&key selection) &body folders)
  `(workbench/make-navigator (list ,@folders) :selection ,selection))

(def macro workbench/console ((&key selection) &body content)
  `(workbench/make-console ,@content :selection ,selection))

(def macro workbench/editor ((&key selection) &body documents)
  `(workbench/make-editor (list ,@documents) :selection ,selection))

(def macro workbench/document ((&rest args &key selection title filename) &body content)
  (declare (ignore title filename selection))
  `(workbench/make-document ,(first content) ,@args))
