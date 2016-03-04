;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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
  ((content :type generic-document)))

(def document workbench/selection (workbench/base)
  ())

(def document workbench/gesture (workbench/base)
  ())

(def document workbench/editor (workbench/base)
  ((documents :type sequence)))

(def document workbench/document (workbench/base)
  ((title :type string)
   (filename :type pathname)
   (content :type t)))

;;;;;;
;;; Construction

(def function make-workbench/workbench (navigator editor &key selection)
  (make-instance 'workbench/workbench :navigator navigator
                 :console (make-workbench/console (text/text () (text/string "Welcome to ProjecturEd!" :font *font/ubuntu/regular/14* :font-color *color/default*)))
                 :editor editor :selection selection))

(def function make-workbench/navigator (folders &key selection)
  (make-instance 'workbench/navigator :folders folders :selection selection))

(def function make-workbench/console (content &key selection)
  (make-instance 'workbench/console :content content :selection selection))

(def function make-workbench/selection (&key selection)
  (make-instance 'workbench/selection :selection selection))

(def function make-workbench/gesture (&key selection)
  (make-instance 'workbench/gesture :selection selection))

(def function make-workbench/editor (documents &key selection)
  (make-instance 'workbench/editor :documents documents :selection selection))

(def function make-workbench/document (content &key title filename selection)
  (make-instance 'workbench/document :content content :title title :filename filename :selection selection))

;;;;;;
;;; Construction

(def macro workbench/workbench ((&key selection) &body elements)
  `(make-workbench/workbench ,(first elements) ,(second elements) :selection ,selection))

(def macro workbench/navigator ((&key selection) &body folders)
  `(make-workbench/navigator (list ,@folders) :selection ,selection))

(def macro workbench/console ((&key selection) &body content)
  `(make-workbench/console ,@content :selection ,selection))

(def macro workbench/selection (&key selection)
  `(make-workbench/selection :selection ,selection))

(def macro workbench/gesture (&key selection)
  `(make-workbench/selection :selection ,selection))

(def macro workbench/editor ((&key selection) &body documents)
  `(make-workbench/editor (list ,@documents) :selection ,selection))

(def macro workbench/document ((&key selection title filename) &body content)
  `(make-workbench/document ,(first content) :title ,title :filename ,filename :selection ,selection))
