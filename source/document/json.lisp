;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document json/base ()
  ())

(def document json/insertion (json/base)
  ((value "" :type string :allocation :class)))

(def document json/null (json/base)
  ((value "null" :type string :allocation :class)))

(def document json/boolean (json/base)
  ((value :type boolean)
   (true-value "true" :type string :allocation :class)
   (false-value "false" :type string :allocation :class)))

(def document json/number (json/base)
  ((value :type number)))

(def document json/string (json/base)
  ((value :type string)))

(def document json/array (json/base)
  ((elements :type sequence)
   (collapsed :type boolean)))

(def document json/object-entry (json/base)
  ((key :type string)
   (value :type json/base)
   (collapsed :type boolean)))

(def document json/object (json/base)
  ((entries :type sequence)
   (collapsed :type boolean)))

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

(def function make-json/array (elements &key collapsed selection)
  (make-instance 'json/array :elements elements :collapsed collapsed :selection selection))

(def function make-json/object-entry (key value &key collapsed selection)
  (make-instance 'json/object-entry :key key :value value :collapsed collapsed :selection selection))

(def function make-json/object (entries &key collapsed selection)
  (make-instance 'json/object :entries entries :collapsed collapsed :selection selection))

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

(def macro json/array ((&key collapsed key predicate selection) &body elements)
  `(make-json/array (document/sequence (:key ,key :predicate ,predicate :selection (rest ,selection)) ,@elements)
                    :collapsed ,collapsed
                    :selection ,selection))

(def macro json/object-entry ((&key collapsed selection) key value)
  `(make-json/object-entry ,key ,value :collapsed ,collapsed :selection ,selection))

(def macro json/object ((&key collapsed key predicate selection) &body key-value-pairs)
  `(make-json/object (document/sequence (:key ,key :predicate ,predicate)
                       ,@(iter (for (key value) :in key-value-pairs)
                               (collect `(make-json/object-entry ,key ,value))))
                     :collapsed ,collapsed
                     :selection ,selection))

;;;;;;
;;; API

(def maker json ()
  (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                (the string (subseq (the string document) 0 0))))))

(def loader json (filename)
  (with-input-from-file (input filename :element-type 'character)
    (cl-json:with-shadowed-custom-vars
      (cl-json:set-custom-vars
       :integer (lambda (value) (make-json/number (cl-json::parse-number value)))
       :real (lambda (value) (make-json/number (cl-json::parse-number value)))
       :boolean (lambda (value) (make-json/boolean (cl-json::json-boolean-to-lisp value)))
       :array-element #'cl-json::accumulator-add
       :end-of-array (lambda () (make-json/array (make-document/sequence (cl-json::accumulator-get-sequence))))
       :object-key #'cl-json::accumulator-add-key
       :object-value #'cl-json::accumulator-add-value
       :end-of-object (lambda () (make-json/object (make-document/sequence (iter (for (key . value) :in (cl-json::accumulator-get))
                                                                            (collect (make-json/object-entry key value))))))
       :string-char #'cl-json::string-stream-accumulator-add
       :end-of-string (lambda () (make-json/string (cl-json::string-stream-accumulator-get))))
      (bind ((cl-json::*json-identifier-name-to-lisp* 'identity)
             (cl-json::*identifier-name-to-key* 'value-of))
        (cl-json:decode-json input)))))

(def saver json (filename document)
  (with-output-to-file (output filename :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (write-sequence (babel:string-to-octets (print-document document nil)) output)))
