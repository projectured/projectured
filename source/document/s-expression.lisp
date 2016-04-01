;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document s-expression/base ()
  ((indentation :type integer)))

(def document s-expression/insertion (s-expression/base)
  ((value :type string)
   (default-value :type string)
   (compound :type boolean)
   (factory :type function)))

(def document s-expression/comment (s-expression/base)
  ((content :type string)))

(def document s-expression/number (s-expression/base)
  ((value :type number)))

(def document s-expression/string (s-expression/base)
  ((value :type string)
   (default-value :type string)))

(def document s-expression/symbol (s-expression/base)
  ((name :type string)
   (package :type t)
   (default-value :type string)
   (font :type style/font)
   (font-color :type style/color)))

(def document s-expression/quote (s-expression/base)
  ((value :type s-expression/base)))

(def document s-expression/object (s-expression/base)
  ((value :type standard-object)))

(def document s-expression/list (s-expression/base)
  ((elements :type sequence)
   (collapsed :type boolean)))

(def document s-expression/toplevel (s-expression/base)
  ((elements :type sequence)
   (collapsed :type boolean)))

;;;;;;
;;; Construction

(def function make-s-expression/insertion (value factory &key default-value compound indentation selection)
  (make-instance 's-expression/insertion :value value :factory factory :default-value default-value :compound compound :indentation indentation :selection selection))

(def function make-s-expression/comment (content &key indentation selection)
  (make-instance 's-expression/comment :content content :indentation indentation  :selection selection))

(def function make-s-expression/number (value &key indentation selection)
  (make-instance 's-expression/number :value value :indentation indentation  :selection selection))

(def function make-s-expression/string (value &key default-value indentation selection)
  (make-instance 's-expression/string :value value :default-value default-value :indentation indentation :selection selection))

(def function make-s-expression/symbol (name package &key default-value indentation font font-color selection)
  (make-instance 's-expression/symbol :name name :package package :default-value default-value :indentation indentation :font font :font-color font-color :selection selection))

(def function make-s-expression/quote (value &key indentation font font-color selection)
  (make-instance 's-expression/quote :value value :indentation indentation :selection selection))

(def function make-s-expression/object (value &key indentation selection)
  (make-instance 's-expression/object :value value :indentation indentation :selection selection))

(def function make-s-expression/list (elements &key indentation collapsed selection)
  (make-instance 's-expression/list :elements elements :indentation indentation :collapsed collapsed :selection selection))

(def function make-s-expression/toplevel (elements &key indentation collapsed selection)
  (make-instance 's-expression/toplevel :elements elements :indentation indentation :collapsed collapsed :selection selection))

;;;;;;
;;; Construction

(def macro s-expression/insertion ((&key default-value compound indentation selection) value factory)
  `(make-s-expression/insertion ,value ,factory :default-value ,default-value :compound ,compound :indentation ,indentation :selection ,selection))

(def macro s-expression/comment ((&key indentation selection) &body content)
  `(make-s-expression/comment ,(first content) :indentation ,indentation :selection ,selection))

(def macro s-expression/number ((&key indentation selection) &body value)
  `(make-s-expression/number ,(first value) :indentation ,indentation :selection ,selection))

(def macro s-expression/string ((&key default-value indentation selection) &body value)
  `(make-s-expression/string ,(first value) :default-value ,default-value :indentation ,indentation :selection ,selection))

(def macro s-expression/symbol ((&key default-value indentation font font-color selection) name package)
  `(make-s-expression/symbol ,name ,package :default-value ,default-value :indentation ,indentation :font ,font :font-color ,font-color :selection ,selection))

(def macro s-expression/quote ((&key indentation font font-color selection) &body value)
  `(make-s-expression/quote ,(first value) :indentation ,indentation :selection ,selection))

(def macro s-expression/object ((&key indentation selection) &body value)
  `(make-s-expression/object ,(first value) :indentation ,indentation :selection ,selection))

(def macro s-expression/list ((&key indentation collapsed selection) &body elements)
  `(make-s-expression/list (list ,@elements) :indentation ,indentation :collapsed ,collapsed :selection ,selection))

(def macro s-expression/toplevel ((&key indentation collapsed selection) &body elements)
  `(make-s-expression/toplevel (list ,@elements) :indentation ,indentation :collapsed ,collapsed :selection ,selection))

;;;;;;
;;; API

(def function make-s-expression/symbol* (symbol &rest args)
  (apply 'make-s-expression/symbol (symbol-name symbol) (package-name (symbol-package symbol)) args))

(def function s-expression/clone (document &key (indentation nil indentation?))
  (etypecase document
    (s-expression/comment (make-s-expression/comment (content-of document)
                                                     :indentation (if indentation? indentation (indentation-of document))
                                                     :selection (as (selection-of document))))
    (s-expression/number (make-s-expression/number (value-of document)
                                                   :indentation (if indentation? indentation (indentation-of document))
                                                   :selection (as (selection-of document))))
    (s-expression/string (make-s-expression/string (value-of document)
                                                   :indentation (if indentation? indentation (indentation-of document))
                                                   :selection (as (selection-of document))))
    (s-expression/symbol (make-s-expression/symbol (name-of document) (package-of document)
                                                   :indentation (if indentation? indentation (indentation-of document))
                                                   :selection (as (selection-of document))))
    (s-expression/list (make-s-expression/list (elements-of document)
                                               :indentation (if indentation? indentation (indentation-of document))
                                               :collapsed (collapsed-p document)
                                               :selection (as (selection-of document))))
    (s-expression/toplevel (make-s-expression/toplevel (elements-of document)
                                                       :indentation (if indentation? indentation (indentation-of document))
                                                       :collapsed (collapsed-p document)
                                                       :selection (as (selection-of document))))))
