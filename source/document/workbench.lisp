;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;; TODO: clipboard, collection?

;;;;;;
;;; Document

(def document workbench/base ()
  ())

(def document workbench/workbench (workbench/base)
  ((navigation-page :type workbench/page)
   (editing-page :type workbench/page)
   (information-page :type workbench/page)))

(def document workbench/page (workbench/base)
  ((elements :type sequence)))

(def document workbench/navigator (workbench/base)
  ((title "Navigator" :type string :allocation :class)
   (folders :type sequence)))

(def document workbench/console (workbench/base)
  ((title "Console" :type :string :allocation :class)
   (content :type text/text)))

(def document workbench/descriptor (workbench/base)
  ((title "Descriptor" :type :string :allocation :class)
   (content :type document/reference)))

(def document workbench/operator (workbench/base)
  ((title "Operator" :type :string :allocation :class)))

(def document workbench/searcher (workbench/base)
  ((title "Searcher" :type :string :allocation :class)))

(def document workbench/evaluator (workbench/base)
  ((title "Evaluator" :type :string :allocation :class)
   (content :type evaluator/toplevel)))

(def document workbench/document (workbench/base)
  ((title :type string)
   (filename :type pathname)
   (content :type t)))

;;;;;;
;;; Construction

(def function make-workbench/workbench (navigation-page editing-page information-page &key selection)
  (make-instance 'workbench/workbench :navigation-page navigation-page :editing-page editing-page :information-page information-page :selection selection))

(def function make-workbench/page (elements &key selection)
  (make-instance 'workbench/page :elements elements :selection selection))

(def function make-workbench/navigator (folders &key selection)
  (make-instance 'workbench/navigator :folders folders :selection selection))

(def function make-workbench/console (content &key selection)
  (make-instance 'workbench/console :content content :selection selection))

(def function make-workbench/descriptor (content &key selection)
  (make-instance 'workbench/descriptor :content content :selection selection))

(def function make-workbench/operator (&key selection)
  (make-instance 'workbench/operator :selection selection))

(def function make-workbench/searcher (&key selection)
  (make-instance 'workbench/searcher :selection selection))

(def function make-workbench/evaluator (content &key selection)
  (make-instance 'workbench/evaluator :content content :selection selection))

(def function make-workbench/document (content &key title filename selection)
  (make-instance 'workbench/document :content content :title title :filename filename :selection selection))

;;;;;;
;;; Construction

(def macro workbench/workbench ((&key selection) &body elements)
  `(make-workbench/workbench ,(first elements) ,(second elements) ,(third elements) :selection ,selection))

(def macro workbench/page ((&key selection) &body elements)
  `(make-workbench/page (list ,@elements) :selection ,selection))

(def macro workbench/navigator ((&key selection) &body folders)
  `(make-workbench/navigator (list ,@folders) :selection ,selection))

(def macro workbench/console ((&key selection) &body content)
  `(make-workbench/console ,@content :selection ,selection))

(def macro workbench/descriptor ((&key selection) &body content)
  `(make-workbench/descriptor ,(first content) :selection ,selection))

(def macro workbench/operator ((&key selection) &body content)
  `(make-workbench/operator :selection ,selection))

(def macro workbench/searcher ((&key selection) &body content)
  `(make-workbench/searcher :selection ,selection))

(def macro workbench/evaluator ((&key selection) &body content)
  `(make-workbench/evaluator ,(first content) :selection ,selection))

(def macro workbench/document ((&key selection title filename) &body content)
  `(make-workbench/document ,(first content) :title ,title :filename ,filename :selection ,selection))
