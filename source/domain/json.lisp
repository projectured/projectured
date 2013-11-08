;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document json/base ()
  ((indentation :type integer)))

(def document json/nothing (json/base)
  ())

(def document json/null (json/base)
  ())

(def document json/boolean (json/base)
  ((value :type boolean)))

(def document json/number (json/base)
  ((value :type number)))

(def document json/string (json/base)
  ((value :type string)))

(def document json/array (json/base)
  ((elements :type sequence)))

(def document json/object-entry (json/base)
  ((key :type t)
   (value :type t)))

(def document json/object (json/base)
  ((entries :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-document/json/nothing (&key indentation)
  (make-instance 'json/nothing :indentation indentation))

(def (function e) make-document/json/null (&key indentation)
  (make-instance 'json/null :indentation indentation))

(def (function e) make-document/json/boolean (value &key indentation)
  (make-instance 'json/boolean :value value :indentation indentation))

(def (function e) make-document/json/number (value &key indentation)
  (make-instance 'json/number :value value :indentation indentation))

(def (function e) make-document/json/string (value &key indentation)
  (make-instance 'json/string :value value :indentation indentation))

(def (function e) make-document/json/array (elements &key indentation)
  (make-instance 'json/array :elements elements :indentation indentation))

(def (function e) make-document/json/object-entry (key value &key indentation)
  (make-instance 'json/object-entry :key key :value value :indentation indentation))

(def (function e) make-document/json/object (entries &key indentation)
  (make-instance 'json/object :entries entries :indentation indentation))

;;;;;;
;;; Construction

(def (macro e) json/nothing ()
  '(make-document/json/nothing))

(def (macro e) json/null ()
  '(make-document/json/null))

(def (macro e) json/boolean (value)
  `(make-document/json/boolean ,value))

(def (macro e) json/number (value)
  `(make-document/json/number ,value))

(def (macro e) json/string (value)
  `(make-document/json/string ,value))

(def (macro e) json/array (&body elements)
  `(make-document/json/array (list ,@elements)))

(def (macro e) json/object-entry (key value)
  `(make-document/json/object-entry ,key ,value))

(def (macro e) json/object (&body key-value-pairs)
  `(make-document/json/object (list ,@(iter (for (key value) :in key-value-pairs)
                                            (collect `(make-document/json/object-entry ,key ,value))))))
