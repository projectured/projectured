;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Util

(def (class* ea) sequence-position ()
  ((sequence :type sequence)
   (index :type integer)))

(def (function e) pos (sequence index)
  (make-instance 'sequence-position :sequence sequence :index index))

(def (function e) form-type (form)
  (typecase form
    (cons 'list)
    (number 'number)
    (string 'string)
    (t (type-of form))))

;; TODO: rename
(def (function e) provider-combinator (&rest functions)
  (lambda (&rest args)
    (iter (for function :in functions)
          (thereis (apply function args)))))

(def (function e) boolean-to-string (value)
  (if value "true" "false"))

(def (function e) tree-search (tree element)
  (iter (for tree-element :in tree)
        (thereis (cond ((equal tree-element element)
                        #t)
                       ((listp tree-element)
                        (tree-search tree-element element))))))

(def (function e) tree-replace (tree element replacement)
  (iter (for tree-element :in tree)
        (collect (cond ((equal tree-element element)
                        replacement)
                       ((listp tree-element)
                        (tree-replace tree-element element replacement))
                       (t tree-element)))))
