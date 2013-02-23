;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Tree domain provides:
;;;;  - node

;;;;;;
;;; Tree document classes

(def document tree/base ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)
   (separator :type string)))

(def document tree/node (tree/base)
  ((expanded :type boolean)
   (children :type sequence)))

(def document tree/leaf (tree/base)
  ((content :type t)))

;;;;;;
;;; Tree document constructors

(def (function e) make-tree/node (children &key opening-delimiter closing-delimiter separator)
  (make-instance 'tree/node
                 :children children
                 :expanded #t
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :separator separator))

(def (function e) make-tree/leaf (content &key opening-delimiter closing-delimiter separator)
  (make-instance 'tree/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :separator separator))

;;;;;;
;;; Tree reference

(def macro opening-delimiter (reference value)
  (declare (ignore reference))
  value)

(def macro closing-delimiter (reference value)
  (declare (ignore reference))
  value)

(def macro separator (previous-child-reference next-child-reference value)
  (declare (ignore previous-child-reference next-child-reference))
  value)

(def macro indentation (reference value)
  (declare (ignore reference))
  value)

(def (function e) new-line (reference)
  (declare (ignore reference))
  "
")

;;;;;;
;;; Tree operation classes

(def operation operation/tree (operation)
  ())

(def operation operation/tree/toggle-node (operation/tree)
  ((target :type reference)))

;;;;;;
;;; Tree operation constructors

(def (function e) make-operation/tree/toggle-node (reference)
  (make-instance 'operation/tree/toggle-node :reference reference))

;;;;;;
;;; Tree operation API implementation

(def method redo-operation ((operation operation/tree/toggle-node))
  (not-yet-implemented))

;;;;;;
;;; Provider

(def (function e) tree-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from tree-color-provider
                       (make-style/color 255 196 196 196)))))))

(def (function e) tree-delimiter-provider (iomap reference)
  (declare (ignore iomap))
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (pattern-case ?node
         ((the tree/node ?a)
          (return-from tree-delimiter-provider
            (ecase delimiter
              (opening-delimiter "(")
              (closing-delimiter ")")))))))))

(def (function e) tree-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore iomap previous-child-reference))
  (pattern-case next-child-reference
    ((the ?a (elt (the list (children-of (the tree/node ?b))) ?c))
     (return-from tree-separator-provider " "))))

(def (function e) tree-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore iomap previous-child-reference))
  (when (some (of-type 'tree/node) (children-of parent-node))
    (pattern-case next-child-reference
      ((the ?a (elt (the list (children-of (the tree/node ?b))) ?c))
       (when (> ?c 0)
         (return-from tree-indentation-provider 2))))))
