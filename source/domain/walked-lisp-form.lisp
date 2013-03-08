;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Walked lisp form domain provides:
;;;;  - all form classes provided by :hu.dwim.walker

;;;;;;
;;; Walked lisp form document classes
;;;
;;; The walked lisp form document classes are provided by the :hu.dwim.walker project.

;;;;;;
;;; Walked lisp form constructors
;;;
;;; The walked lisp form document constructores are provided by the :hu.dwim.walker project.

;;;;;;
;;; Walked lisp form provider

(def (function e) walked-lisp-form-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 196 196 196)))
                    ;; variable reference name
                    ((the character (elt (the string (string-downcase (the symbol (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker:variable-reference-form)) ?a) 'hu.dwim.walker::name)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 196 196 0)))
                    ;; special form name
                    ((the character (elt (the string (string-downcase (the symbol (slot-value ?a 'hu.dwim.walker::form-name)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 0 196 196)))
                    ;; constant number
                    ((the character (elt (the string (write-to-string (the ?type (?if (subtypep ?type 'number)) (slot-value (the hu.dwim.walker::constant-form ?a) 'hu.dwim.walker::value)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 0 196 0)))
                    ;; constant string
                    ((the character (elt (the string (slot-value (the hu.dwim.walker::constant-form ?a) 'hu.dwim.walker::value)) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 0 196 0)))
                    ;; function application name
                    ((the character (elt (the string (string-downcase (the symbol (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker:application-form)) ?a) 'hu.dwim.walker::operator)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 0 0 196)))
                    ;; function definition name
                    ((the character (elt (the string (string-downcase (the symbol (slot-value (the hu.dwim.walker::function-definition-form ?a) 'hu.dwim.walker::name)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 196 0 0)))
                    ;; docstring
                    ((the character (elt (the string (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker::docstring-mixin)) ?a) 'hu.dwim.walker::docstring)) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 196 0 196)))
                    ;; function argument name
                    ((the character (elt (the string (string-downcase (the symbol (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker:function-argument-form)) ?a) 'hu.dwim.walker::name)))) ?b))
                     (return-from walked-lisp-form-color-provider
                       (make-style/color 255 128 128 0)))))))

(def (function e) walked-lisp-form-delimiter-provider (iomap reference)
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (map-backward iomap (second reference)
                     (lambda (iomap reference)
                       (declare (ignore iomap))
                       (pattern-case reference
                         ((the lisp-form/list ?a)
                          (return-from walked-lisp-form-delimiter-provider
                            (ecase delimiter
                              (opening-delimiter "(")
                              (closing-delimiter ")")))))))))))

(def (function e) walked-lisp-form-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore previous-child-reference))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the ?a (elt (the list (elements-of (the lisp-form/list ?b))) ?c))
                     (return-from walked-lisp-form-separator-provider " "))))))

(def (function e) walked-lisp-form-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore next-child-reference parent-node))
  (map-backward iomap previous-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ;; new line after bindings and docstring
                    ((?or (the list (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker:lambda-function-form)) ?a) 'hu.dwim.walker::bindings))
                          (the string (slot-value (the ?type (?if (subtypep ?type 'hu.dwim.walker::docstring-mixin)) ?a) 'hu.dwim.walker::docstring)))
                     (return-from walked-lisp-form-indentation-provider 2))
                    ;; new line after if special form condition and then parts
                    ((?or (the ?a (slot-value (the hu.dwim.walker:if-form ?b) 'hu.dwim.walker::condition))
                          (the ?a (slot-value (the hu.dwim.walker:if-form ?b) 'hu.dwim.walker::then)))
                     (return-from walked-lisp-form-indentation-provider 4))
                    ;; TODO: new line after second argument of function application
                    #+nil
                    ((the ?a (elt (the list (slot-value (the hu.dwim.walker:free-application-form ?b) 'hu.dwim.walker::arguments)) ?c))
                     (when next-child-reference
                       (return-from walked-lisp-form-indentation-provider 2)))))))
