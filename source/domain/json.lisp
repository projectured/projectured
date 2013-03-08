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

(def document json/null (json/base)
  ())

(def document json/boolean (json/base)
  ((value :type boolean)))

(def document json/number (json/base)
  ((value :type number)))

(def document json/string (json/base)
  ((text :type string)))

(def document json/array (json/base)
  ((elements :type sequence)))

(def document json/object (json/base)
  ((key-value-map :type hash-table)))

;;;;;;
;;; Construction

(def (function e) make-json/null ()
  (make-instance 'json/null))

(def (function e) make-json/boolean (value)
  (make-instance 'json/boolean :value value))

(def (function e) make-json/number (value)
  (make-instance 'json/number :value value))

(def (function e) make-json/string (text)
  (make-instance 'json/string :text text))

(def (function e) make-json/array (elements)
  (make-instance 'json/array :elements elements))

(def (function e) make-json/object (elements)
  (make-instance 'json/object
                 :key-value-map (if (listp elements)
                                    (prog1-bind key-value-map (make-hash-table)
                                      (iter (for (key value) :in elements)
                                            (setf (gethash key key-value-map) value)))
                                    elements)))

;;;;;;
;;; Construction

(def (macro e) json/null ()
  '(make-json/null))

(def (macro e) json/boolean (value)
  `(make-json/boolean ,value))

(def (macro e) json/number (value)
  `(make-json/number ,value))

(def (macro e) json/string (text)
  `(make-json/string ,text))

(def (macro e) json/array (&body elements)
  `(make-json/array (list ,@elements)))

(def (macro e) json/object (&body key-value-pairs)
  `(make-json/object (list ,@(iter (for (key value) :in key-value-pairs)
                                   (collect `(list ,key ,value))))))

;;;;;;
;;; Provider

(def (function e) json-font-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from json-font-color-provider
                       *color/solarized/gray*))
                    ((the character (elt (the string (value (the json/null ?a) ?b)) ?c))
                     (return-from json-font-color-provider
                       *color/solarized/red*))
                    ((the character (elt (the string (boolean-to-string (the boolean (value-p (the json/boolean ?a))))) ?b))
                     (return-from json-font-color-provider
                       *color/solarized/blue*))
                    ((the character (elt (the string (write-to-string (the number (value-of (the json/number ?a))))) ?b))
                     (return-from json-font-color-provider
                       *color/solarized/magenta*))
                    ((the character (elt (the string (text-of (the json/string ?a))) ?b))
                     (return-from json-font-color-provider
                       *color/solarized/green*))
                    ((the character (elt (the string (entry-key ?a)) ?b))
                     (return-from json-font-color-provider
                       *color/solarized/orange*))))))

(def (function e) json-delimiter-provider (iomap reference)
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (map-backward iomap ?node
                     (lambda (iomap reference)
                       (declare (ignore iomap))
                       (pattern-case reference
                         ((the json/array ?a)
                          (return-from json-delimiter-provider
                            (ecase delimiter
                              (opening-delimiter "[")
                              (closing-delimiter "]"))))
                         ((the json/object ?a)
                          (return-from json-delimiter-provider
                            (ecase delimiter
                              (opening-delimiter "{")
                              (closing-delimiter "}"))))
                         ((the json/string ?a)
                          (return-from json-delimiter-provider "\""))
                         ((the string (entry-key ?a))
                          (return-from json-delimiter-provider "\"")))))))))

(def (function e) json-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore previous-child-reference))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((?or (the ?a (gethash-entry ?b (the json/object ?c)))
                          (the ?a (elt (the list (elements-of (the json/array ?b))) ?c)))
                     (return-from json-separator-provider ","))
                    ((the ?a (gethash ?b (key-value-map-of (the json/object ?c))))
                     (return-from json-separator-provider " : "))))))

(def (function e) json-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (when (some (of-type 'tree/node) (children-of parent-node))
    (map-backward iomap next-child-reference
                  (lambda (iomap reference)
                    (declare (ignore iomap))
                    (pattern-case reference
                      ((the ?a (elt (the list (elements-of (the json/array ?b))) ?c))
                       (when (> ?c 0)
                         (return-from json-indentation-provider 1)))
                      ((the t (gethash-entry ?a (the json/object ?b)))
                       (when previous-child-reference
                         (return-from json-indentation-provider 1))))))))
