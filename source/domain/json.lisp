;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document json/base ()
  ())

;; TODO: rename and generalize?
(def document json/insertion (json/base)
  ((value "" :type string :allocation :class :computed-in nil)))

(def document json/null (json/base)
  ((value "null" :type string :allocation :class :computed-in nil)))

(def document json/boolean (json/base)
  ((value :type boolean)
   (true-value "true" :type string :allocation :class :computed-in nil)
   (false-value "false" :type string :allocation :class :computed-in nil)))

(def document json/number (json/base)
  ((value :type number)))

(def document json/string (json/base)
  ((value :type string)))

(def document json/array (json/base)
  ;; TODO: json/array-elements?
  ((elements :type sequence)))

(def document json/object-entry (json/base)
  ;; TODO: json/object-entry-key?
  ((key :type string)
   (value :type json/base)))

(def document json/object (json/base)
  ((entries :type sequence)))

;;;;;;
;;; Construction

(def function make-json/insertion (&key selection)
  (make-instance 'json/insertion :selection selection))

(def function make-json/null (&key selection)
  (make-instance 'json/null :selection selection))

(def function make-json/boolean (value &key selection)
  (make-instance 'json/boolean :value value :selection selection))

(def function make-json/number (value &key selection)
  (make-instance 'json/number :value value :selection selection))

(def function make-json/string (value &key selection)
  (make-instance 'json/string :value value :selection selection))

(def function make-json/array (elements &key selection)
  (make-instance 'json/array :elements elements :selection selection))

(def function make-json/object-entry (key value &key selection)
  (make-instance 'json/object-entry :key key :value value :selection selection))

(def function make-json/object (entries &key selection)
  (make-instance 'json/object :entries entries :selection selection))

;;;;;;
;;; Construction

(def macro json/insertion ((&key selection))
  `(make-json/insertion :selection ,selection))

(def macro json/null ((&key selection))
  `(make-json/null :selection ,selection))

(def macro json/boolean ((&key selection) &body value)
  `(make-json/boolean ,(first value) :selection ,selection))

(def macro json/number ((&key selection) &body value)
  `(make-json/number ,(first value) :selection ,selection))

(def macro json/string ((&key selection) &body value)
  `(make-json/string ,(first value) :selection ,selection))

(def macro json/array ((&key selection) &body elements)
  `(make-json/array (document/sequence () ,@elements) :selection ,selection))

(def macro json/object-entry ((&key selection) key value)
  `(make-json/object-entry ,key ,value :selection ,selection))

(def macro json/object ((&key selection) &body key-value-pairs)
  `(make-json/object (document/sequence () ,@(iter (for (key value) :in key-value-pairs)
                                                   (collect `(make-json/object-entry ,key ,value))))
                     :selection ,selection))

;;;;;;
;;; API

(def function json/load-document (input)
  (cl-json:with-shadowed-custom-vars
    (cl-json:set-custom-vars
     :integer (lambda (value) (make-json/number (cl-json::parse-number value)))
     :real (lambda (value) (make-json/number (cl-json::parse-number value)))
     :boolean (lambda (value) (make-json/boolean (cl-json::json-boolean-to-lisp value)))
     :array-element #'cl-json::accumulator-add
     :end-of-array (lambda () (make-json/array (cl-json::accumulator-get-sequence)))
     :object-key #'cl-json::accumulator-add-key
     :object-value #'cl-json::accumulator-add-value
     :end-of-object (lambda () (make-json/object (iter (for (key . value) :in (cl-json::accumulator-get))
                                                  (collect (make-json/object-entry key value)))))
     :string-char #'cl-json::string-stream-accumulator-add
     :end-of-string (lambda () (make-json/string (cl-json::string-stream-accumulator-get))))
    (bind ((cl-json::*json-identifier-name-to-lisp* 'identity)
           (cl-json::*identifier-name-to-key* 'value-of))
      (cl-json:decode-json input))))
