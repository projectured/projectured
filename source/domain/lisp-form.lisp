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
  ((value :type symbol)
   (font :type style/font)
   (font-color :type style/color)))

(def document lisp-form/object (lisp-form/base)
  ((value :type standard-object)))

(def document lisp-form/list (lisp-form/base)
  ((elements :type sequence)))

(def document lisp-form/top-level (lisp-form/base)
  ((elements :type sequence)))

;;;;;;
;;; Lisp form document constructors

(def (function e) make-lisp-form/comment (content &key indentation)
  (make-instance 'lisp-form/comment :content content :indentation indentation))

(def (function e) make-lisp-form/number (value &key indentation)
  (make-instance 'lisp-form/number :value value :indentation indentation))

(def (function e) make-lisp-form/string (value &key indentation)
  (make-instance 'lisp-form/string :value value :indentation indentation))

(def (function e) make-lisp-form/symbol (value &key indentation font font-color)
  (make-instance 'lisp-form/symbol :value value :indentation indentation :font font :font-color font-color))

(def (function e) make-lisp-form/object (value &key indentation)
  (make-instance 'lisp-form/object :value value :indentation indentation))

(def (function e) make-lisp-form/list (elements &key indentation)
  (make-instance 'lisp-form/list :elements elements :indentation indentation))

(def (function e) make-lisp-form/top-level (elements &key indentation)
  (make-instance 'lisp-form/top-level :elements elements :indentation indentation))