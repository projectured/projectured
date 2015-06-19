;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; TODO: either rename to s-expression or merge as literals into common-lisp?

;;;;;;
;;; Document

(def document lisp-form/base ()
  ((indentation :type integer)))

(def document lisp-form/insertion (lisp-form/base)
  ((value :type string)
   (factory :type function)))

(def document lisp-form/comment (lisp-form/base)
  ((content :type string)))

(def document lisp-form/number (lisp-form/base)
  ((value :type number)))

(def document lisp-form/string (lisp-form/base)
  ((value :type string)
   (default-value :type string)))

(def document lisp-form/symbol (lisp-form/base)
  ((name :type string)
   (package :type t)
   (default-value :type string)
   (font :type style/font)
   (font-color :type style/color)))

(def document lisp-form/quote (lisp-form/base)
  ((value :type lisp-form/base)))

(def document lisp-form/object (lisp-form/base)
  ((value :type standard-object)))

(def document lisp-form/list (lisp-form/base)
  ((elements :type sequence)))

(def document lisp-form/top-level (lisp-form/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction

(def function make-lisp-form/insertion (value factory &key indentation selection)
  (make-instance 'lisp-form/insertion :value value :factory factory :indentation indentation  :selection selection))

(def function make-lisp-form/comment (content &key indentation selection)
  (make-instance 'lisp-form/comment :content content :indentation indentation  :selection selection))

(def function make-lisp-form/number (value &key indentation selection)
  (make-instance 'lisp-form/number :value value :indentation indentation  :selection selection))

(def function make-lisp-form/string (value &key default-value indentation selection)
  (make-instance 'lisp-form/string :value value :default-value default-value :indentation indentation :selection selection))

(def function make-lisp-form/symbol (name package &key default-value indentation font font-color selection)
  (make-instance 'lisp-form/symbol :name name :package package :default-value default-value :indentation indentation :font font :font-color font-color :selection selection))

(def function make-lisp-form/quote (value &key indentation font font-color selection)
  (make-instance 'lisp-form/quote :value value :indentation indentation :selection selection))

(def function make-lisp-form/object (value &key indentation selection)
  (make-instance 'lisp-form/object :value value :indentation indentation :selection selection))

(def function make-lisp-form/list (elements &key indentation selection)
  (make-instance 'lisp-form/list :elements elements :indentation indentation :selection selection))

(def function make-lisp-form/top-level (elements &key indentation selection)
  (make-instance 'lisp-form/top-level :elements elements :indentation indentation :selection selection))

;;;;;;
;;; Construction

(def macro lisp-form/insertion ((&key indentation selection) value factory)
  `(make-lisp-form/insertion ,value ,factory :indentation ,indentation :selection ,selection))

(def macro lisp-form/comment ((&key indentation selection) &body content)
  `(make-lisp-form/comment ,(first content) :indentation ,indentation :selection ,selection))

(def macro lisp-form/number ((&key indentation selection) &body value)
  `(make-lisp-form/number ,(first value) :indentation ,indentation :selection ,selection))

(def macro lisp-form/string ((&key default-value indentation selection) &body value)
  `(make-lisp-form/string ,(first value) :default-value ,default-value :indentation ,indentation :selection ,selection))

(def macro lisp-form/symbol ((&key default-value indentation font font-color selection) name package)
  `(make-lisp-form/symbol ,name ,package :default-value ,default-value :indentation ,indentation :font ,font :font-color ,font-color :selection ,selection))

(def macro lisp-form/quote ((&key indentation font font-color selection) &body value)
  `(make-lisp-form/quote ,(first value) :indentation ,indentation :selection ,selection))

(def macro lisp-form/object ((&key indentation selection) &body value)
  `(make-lisp-form/object ,(first value) :indentation ,indentation :selection ,selection))

(def macro lisp-form/list ((&key indentation selection) &body elements)
  `(make-lisp-form/list (list ,@elements) :indentation ,indentation :selection ,selection))

(def macro lisp-form/top-level ((&key indentation selection) &body elements)
  `(make-lisp-form/top-level (list ,@elements) :indentation ,indentation :selection ,selection))

;;;;;;
;;; API

(def function make-lisp-form/symbol* (symbol &rest args)
  (apply 'make-lisp-form/symbol (symbol-name symbol) (package-name (symbol-package symbol)) args))

(def function lisp-form/clone (document &key (indentation nil indentation?))
  (etypecase document
    (lisp-form/comment (make-lisp-form/comment (content-of document)
                                               :indentation (if indentation? indentation (indentation-of document))
                                               :selection (selection-of document)))
    (lisp-form/number (make-lisp-form/number (value-of document)
                                             :indentation (if indentation? indentation (indentation-of document))
                                             :selection (selection-of document)))
    (lisp-form/list (make-lisp-form/list (elements-of document)
                                         :indentation (if indentation? indentation (indentation-of document))
                                         :selection (selection-of document)))
    (lisp-form/top-level (make-lisp-form/top-level (elements-of document)
                                                   :indentation (if indentation? indentation (indentation-of document))
                                                   :selection (selection-of document)))))
