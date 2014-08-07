;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Lisp form domain provides:
;;;;  - all types provided by the Common Lisp implementation

;; TODO: either rename to s-expression or merge as literals into common-lisp?

;;;;;;
;;; Lisp form document classes

(def document lisp-form/base ()
  ((indentation :type integer)))

(def document lisp-form/comment (lisp-form/base)
  ((content :type string)))

(def document lisp-form/number (lisp-form/base)
  ((value :type number)))

(def document lisp-form/string (lisp-form/base)
  ((value :type string)))

(def document lisp-form/symbol (lisp-form/base)
  ((name :type string)
   (package :type t)
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
;;; Lisp form document constructors

(def function make-lisp-form/comment (content &key indentation selection)
  (make-instance 'lisp-form/comment :content content :indentation indentation  :selection selection))

(def function make-lisp-form/number (value &key indentation selection)
  (make-instance 'lisp-form/number :value value :indentation indentation  :selection selection))

(def function make-lisp-form/string (value &key indentation selection)
  (make-instance 'lisp-form/string :value value :indentation indentation :selection selection))

(def function make-lisp-form/symbol (name package &key indentation font font-color selection)
  (make-instance 'lisp-form/symbol :name name :package package :indentation indentation :font font :font-color font-color :selection selection))

(def function make-lisp-form/quote (value &key indentation font font-color selection)
  (make-instance 'lisp-form/quote :value value :indentation indentation :selection selection))

(def function make-lisp-form/object (value &key indentation selection)
  (make-instance 'lisp-form/object :value value :indentation indentation :selection selection))

(def function make-lisp-form/list (elements &key indentation selection)
  (make-instance 'lisp-form/list :elements elements :indentation indentation :selection selection))

(def function make-lisp-form/top-level (elements &key indentation selection)
  (make-instance 'lisp-form/top-level :elements elements :indentation indentation :selection selection))

;;;;;;
;;; API

(def function make-lisp-form/symbol* (symbol &rest args)
  (apply 'make-lisp-form/symbol (symbol-name symbol) (package-name (symbol-package symbol)) args))
