;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Lisp form domain provides:
;;;;  - all types provided by the Common Lisp implementation

;;;;;;
;;; Lisp form document classes

(def document lisp-form/base ()
  ())

(def document lisp-form/number (lisp-form/base)
  ((value :type number)))

(def document lisp-form/string (lisp-form/base)
  ((value :type string)))

(def document lisp-form/symbol (lisp-form/base)
  ((value :type symbol)))

(def document lisp-form/list (lisp-form/base)
  ((elements :type list)))

(def document lisp-form/object (lisp-form/base)
  ((value :type standard-object)))

;;;;;;
;;; Lisp form document constructors

(def (function e) make-lisp-form/number (value)
  (make-instance 'lisp-form/number :value value))

(def (function e) make-lisp-form/string (value)
  (make-instance 'lisp-form/string :value value))

(def (function e) make-lisp-form/symbol (value)
  (make-instance 'lisp-form/symbol :value value))

(def (function e) make-lisp-form/list (elements)
  (make-instance 'lisp-form/list :elements elements))

(def (function e) make-lisp-form/object (value)
  (make-instance 'lisp-form/object :value value))

;;;;;;
;;; Lisp form provider

(def (function e) lisp-form-color-provider (iomap reference)
  (map-backward iomap reference (lambda (iomap reference)
                                  (declare (ignore iomap))
                                  (pattern-case reference
                                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                                          (closing-delimiter ?a ?b))) ?c))
                                     (return-from lisp-form-color-provider
                                       (make-style/color 255 196 196 196)))
                                    ((the character (elt (the string (write-to-string (the ?type (?if (subtypep ?type 'number)) ?a))) ?b))
                                     (return-from lisp-form-color-provider
                                       (make-style/color 255 0 196 0)))
                                    ((the character (elt (the string (string-downcase (the symbol (value-of (the lisp-form/symbol ?a))))) ?b))
                                     (return-from lisp-form-color-provider
                                       (make-style/color 255 196 0 0)))
                                    ((the character (elt (the string (value-of (the lisp-form/string ?a))) ?b))
                                     (return-from lisp-form-color-provider
                                       (make-style/color 255 196 0 196)))
                                    ((the character (elt (the string (write-to-string (value-of (the lisp-form/object ?a)))) ?b))
                                     (return-from lisp-form-color-provider
                                       (make-style/color 255 0 196 196)))))))

(def (function e) lisp-form-delimiter-provider (iomap reference)
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (map-backward iomap ?node
                     (lambda (iomap reference)
                       (declare (ignore iomap))
                       (pattern-case reference
                         ((the lisp-form/list ?a)
                          (return-from lisp-form-delimiter-provider
                            (ecase delimiter
                              (opening-delimiter "(")
                              (closing-delimiter ")")))))))))))

(def (function e) lisp-form-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore previous-child-reference))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the ?a (elt (the list (elements-of (the lisp-form/list ?b))) ?c))
                     (return-from lisp-form-separator-provider " "))))))

(def (function e) lisp-form-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore previous-child-reference parent-node))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the ?a (elt (the list (elements-of (the lisp-form/list ?b))) ?c))
                     (when (> ?c 0)
                       (return-from lisp-form-indentation-provider 2)))))))
