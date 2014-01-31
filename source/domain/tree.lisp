;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document tree/base (document/base)
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

(def (function e) make-tree/leaf (content &key opening-delimiter closing-delimiter indentation selection)
  (make-instance 'tree/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :selection selection))

(def (function e) make-tree/node (children &key opening-delimiter closing-delimiter separator indentation (expanded #t) selection)
  (make-instance 'tree/node
                 :children children
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :separator separator
                 :indentation indentation
                 :expanded expanded
                 :selection selection))

;;;;;;
;;; Construction

(def (macro e) tree/leaf ((&key opening-delimiter closing-delimiter indentation selection) &body content)
  `(make-tree/leaf ,(first content)
                   :opening-delimiter ,opening-delimiter
                   :closing-delimiter ,closing-delimiter
                   :indentation ,indentation
                   :selection ,selection))

(def (macro e) tree/node ((&key opening-delimiter closing-delimiter separator indentation selection) &body children)
  `(make-tree/node (list ,@children)
                   :opening-delimiter ,opening-delimiter
                   :closing-delimiter ,closing-delimiter
                   :separator ,separator
                   :indentation ,indentation
                   :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/tree (operation)
  ())

(def operation operation/tree/toggle-expanded (operation/tree)
  ((document :type document)
   (target :type reference)))

;;;;;;
;;; Construction

(def (function e) make-operation/tree/toggle-expanded (document target)
  (make-instance 'operation/tree/toggle-expanded :document document :target target))

;;;;;;
;;; Redo

(def method redo-operation ((operation operation/tree/toggle-expanded))
  (notf (expanded-p (eval-reference (document-of operation) (reference/flatten (reverse (target-of operation)))))))
