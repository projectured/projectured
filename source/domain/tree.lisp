;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document tree/base ()
  ((opening-delimiter :type text/text)
   (closing-delimiter :type text/text)
   (indentation :type integer)
   (padding :type inset)))

(def document tree/leaf (tree/base)
  ((content :type t)))

(def document tree/node (tree/base)
  ((expanded :type boolean)
   (separator :type text/text)
   (children :type sequence)))

;;;;;;
;;; Construction

(def function make-tree/leaf (content &key opening-delimiter closing-delimiter indentation selection)
  (make-instance 'tree/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :selection selection))

(def function make-tree/node (children &key (expanded #t) separator opening-delimiter closing-delimiter indentation selection)
  (make-instance 'tree/node
                 :children children
                 :expanded expanded
                 :separator separator
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :selection selection))

;;;;;;
;;; Construction

(def macro tree/leaf ((&key opening-delimiter closing-delimiter indentation selection) &body content)
  `(make-tree/leaf ,(first content)
                   :opening-delimiter ,opening-delimiter
                   :closing-delimiter ,closing-delimiter
                   :indentation ,indentation
                   :selection ,selection))

(def macro tree/node ((&key separator opening-delimiter closing-delimiter indentation selection) &body children)
  `(make-tree/node (list-ll ,@children)
                   :opening-delimiter ,opening-delimiter
                   :closing-delimiter ,closing-delimiter
                   :separator ,separator
                   :indentation ,indentation
                   :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/tree ()
  ())

(def operation operation/tree/toggle-expanded (operation/tree)
  ((document :type document)
   (selection :type reference)))

;;;;;;
;;; Construction

(def function make-operation/tree/toggle-expanded (document selection)
  (make-instance 'operation/tree/toggle-expanded :document document :selection selection))

;;;;;;
;;; Run

(def method run-operation ((operation operation/tree/toggle-expanded))
  (notf (expanded-p (eval-reference (document-of operation) (reference/flatten (reverse (selection-of operation)))))))

(def function tree/reference? (reference)
  (pattern-case (reverse reference)
    (((the ?type (?if (subtypep ?type 'tree/base)) ?a) . ?rest)
     #t)))
