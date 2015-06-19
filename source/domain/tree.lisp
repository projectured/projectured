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
   (indentation :type integer)))

(def document tree/leaf (tree/base)
  ((content :type t)))

(def document tree/node (tree/base)
  ((collapsed :type boolean)
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

(def function make-tree/node (children &key collapsed separator opening-delimiter closing-delimiter indentation selection)
  (make-instance 'tree/node
                 :children children
                 :collapsed collapsed
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

(def macro tree/node ((&key collapsed separator opening-delimiter closing-delimiter indentation selection) &body children)
  `(make-tree/node (list-ll ,@children)
                   :collapsed ,collapsed
                   :opening-delimiter ,opening-delimiter
                   :closing-delimiter ,closing-delimiter
                   :separator ,separator
                   :indentation ,indentation
                   :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/tree ()
  ())

(def operation operation/tree/toggle-collapsed (operation/tree)
  ((document :type document)
   (selection :type reference)))

;;;;;;
;;; Construction

(def function make-operation/tree/toggle-collapsed (document selection)
  (make-instance 'operation/tree/toggle-collapsed :document document :selection selection))

;;;;;;
;;; API

(def method run-operation ((operation operation/tree/toggle-collapsed))
  (notf (collapsed-p (eval-reference (document-of operation) (reference/flatten (selection-of operation))))))

(def function tree/reference? (reference)
  (pattern-case (reverse reference)
    (((the ?type (?if (subtypep ?type 'tree/base)) ?a) . ?rest)
     #t)))

(def function tree/clone-leaf (document &key opening-delimiter closing-delimiter (indentation nil indentation?) (selection nil selection?))
  (make-tree/leaf (if selection?
                      (text/make-text (elements-of (content-of document)) :selection (as (nthcdr 1 (va selection))))
                      (content-of document))
                  :opening-delimiter (opening-delimiter-of document) :closing-delimiter (closing-delimiter-of document)
                  :indentation (if indentation? indentation (indentation-of document))
                  :selection (if selection? selection (selection-of document))))

(def function tree/clone-node (document &key collapsed separator opening-delimiter closing-delimiter (indentation nil indentation?) (selection nil selection?))
  (make-tree/node (if selection?
                      (map-ll (ll (children-of document)) (lambda (element) (tree/clone element :selection (as (nthcdr 2 (va selection))))))
                      (children-of document))
                  :collapsed (collapsed-p document)
                  :opening-delimiter (opening-delimiter-of document) :closing-delimiter (closing-delimiter-of document) :separator (separator-of document)
                  :indentation (if indentation? indentation (indentation-of document))
                  :selection (if selection? selection (selection-of document))))

(def function tree/clone (document &rest args &key &allow-other-keys)
  (etypecase document
    (tree/leaf (apply #'tree/clone-leaf document args))
    (tree/node (apply #'tree/clone-node document args))))
