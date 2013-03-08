;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document xml/base ()
  ())

(def document xml/element (xml/base)
  ((name :type string)
   (attributes :type sequence)
   (children :type sequence)))

(def document xml/attribute (xml/base)
  ((name :type string)
   (value :type string)))

(def document xml/text (xml/base)
  ((text :type string)))

;;;;;;
;;; Construction

(def (function e) make-xml/element (name attributes children)
  (make-instance 'xml/element :name name :attributes attributes :children children))

(def (function e) make-xml/attribute (name value)
  (make-instance 'xml/attribute :name name :value value))

(def (function e) make-xml/text (text)
  (make-instance 'xml/text :text text))

;;;;;;
;;; Construction

(def (macro e) xml/element ()
  ;; TODO:
  )

(def (macro e) xml/attribute ()
  ;; TODO:
  )

(def (macro e) xml/text ()
  ;; TODO:
  )

;;;;;;
;;; Reference

(def macro start-tag (reference)
  `(name-of ,reference))

(def macro end-tag (reference)
  `(name-of ,reference))

;;;;;;
;;; Provider

(def (function e) xml-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from xml-color-provider
                       *color/solarized/gray*))
                    ((the character (elt (the string (name-of (the xml/attribute ?a))) ?b))
                     (return-from xml-color-provider
                       *color/solarized/red*))
                    ((the character (elt (the string (value-of (the xml/attribute ?a))) ?b))
                     (return-from xml-color-provider
                       *color/solarized/green*))
                    ((the character (elt (the string ((?or start-tag end-tag) (the xml/element ?a))) ?b))
                     (return-from xml-color-provider
                       *color/solarized/blue*))))))

(def (function e) xml-delimiter-provider (iomap reference)
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (declare (ignore delimiter))
       (map-backward iomap (second reference)
                     (lambda (iomap reference)
                       (declare (ignore iomap))
                       (pattern-case reference
                         ;; " around attribute value
                         ((the string (value-of (the xml/attribute ?a)))
                          (return-from xml-delimiter-provider
                            "\"")))))))))

(def (function e) xml-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore previous-child-reference))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ;; = between attribute name and value
                    ((the string (value-of (the xml/attribute ?a)))
                     (return-from xml-separator-provider "="))
                    ;; space between two attributes
                    ((the xml/attribute ?a)
                     (return-from xml-separator-provider " "))
                    ;; space after the element name and before the list of attributes
                    ((the list (attributes-of ?a))
                     (return-from xml-separator-provider " "))))))

(def (function e) xml-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore previous-child-reference parent-node))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ;; new line before element
                    ((the xml/element ?a)
                     (return-from xml-indentation-provider 2))
                    ((the string (end-tag (the xml/element ?a)))
                     (return-from xml-indentation-provider 0))))))
