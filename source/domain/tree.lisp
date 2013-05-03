;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document tree/base ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)
   (indentation :type integer)))

(def document tree/leaf (tree/base)
  ((content :type t)))

(def document tree/node (tree/base)
  ((expanded :type boolean)
   (separator :type string)
   (children :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-tree/leaf (content &key opening-delimiter closing-delimiter indentation)
  (make-instance 'tree/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation))

(def (function e) make-tree/node (children &key opening-delimiter closing-delimiter separator indentation)
  (make-instance 'tree/node
                 :children children
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :expanded #t
                 :separator separator))

;;;;;;
;;; Construction

(def (macro e) tree/leaf (() &body content)
  `(make-tree/leaf ,(first content)))

(def (macro e) tree/node (() &body children)
  `(make-tree/node (list ,@children)))

;;;;;;
;;; Reference

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
;;; Operation

(def operation operation/tree (operation)
  ())

(def operation operation/tree/toggle-node (operation/tree)
  ((document :type document)
   (target :type reference)))

;;;;;;
;;; Construction

(def (function e) make-operation/tree/toggle-node (document target)
  (make-instance 'operation/tree/toggle-node :document document :target target))

;;;;;;
;;; Redo

(def method redo-operation ((operation operation/tree/toggle-node))
  (notf (expanded-p (eval-reference (document-of operation) (target-of operation)))))
