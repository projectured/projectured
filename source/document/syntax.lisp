;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document syntax/base ()
  ())

(def document syntax/delimitation (syntax/base)
  ((content :type t)
   (opening-delimiter :type text/text)
   (closing-delimiter :type text/text)))

(def document syntax/indentation (syntax/base)
  ((content :type t)
   (indentation :type integer)))

(def document syntax/collapsible (syntax/base)
  ((content :type t)
   (collapsed :type boolean)))

(def document syntax/leaf (syntax/delimitation syntax/indentation)
  ())

(def document syntax/separation (syntax/base)
  ((children :type sequence)
   (separator :type text/text)))

(def document syntax/node (syntax/delimitation syntax/separation syntax/indentation syntax/collapsible)
  ())

;;;;;;
;;; Construction

(def function make-syntax/delimitation (content &key opening-delimiter closing-delimiter selection)
  (make-instance 'syntax/delimitation
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :selection selection))

(def function make-syntax/indentation (content &key indentation selection)
  (make-instance 'syntax/indentation
                 :content content
                 :indentation indentation
                 :selection selection))

(def function make-syntax/collapsible (content &key collapsed selection)
  (make-instance 'syntax/collapsible
                 :content content
                 :collapsed collapsed
                 :selection selection))

(def function make-syntax/leaf (content &key opening-delimiter closing-delimiter indentation selection)
  (make-instance 'syntax/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :selection selection))

(def function make-syntax/separation (children &key separator selection)
  (make-instance 'syntax/separation
                 :children children
                 :separator separator
                 :selection selection))

(def function make-syntax/node (children &key collapsed separator opening-delimiter closing-delimiter indentation selection)
  (make-instance 'syntax/node
                 :children children
                 :collapsed collapsed
                 :separator separator
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :selection selection))

;;;;;;
;;; Construction

(def macro syntax/delimitation ((&key opening-delimiter closing-delimiter selection) &body content)
  `(make-syntax/delimitation ,(first content)
                             :opening-delimiter ,opening-delimiter
                             :closing-delimiter ,closing-delimiter
                             :selection ,selection))

(def macro syntax/indentation ((&key indentation selection) &body content)
  `(make-syntax/indentation ,(first content)
                            :indentation ,indentation
                            :selection ,selection))

(def macro syntax/collapsible ((&key collapsed selection) &body content)
  `(make-syntax/collapsible ,(first content)
                            :collapsed ,collapsed
                            :selection ,selection))

(def macro syntax/leaf ((&key opening-delimiter closing-delimiter indentation selection) &body content)
  `(make-syntax/leaf ,(first content)
                     :opening-delimiter ,opening-delimiter
                     :closing-delimiter ,closing-delimiter
                     :indentation ,indentation
                     :selection ,selection))

(def macro syntax/separation ((&key separator selection) &body children)
  `(make-syntax/separation (list-ll ,@children)
                           :separator ,separator
                           :selection ,selection))

(def macro syntax/node ((&key collapsed separator opening-delimiter closing-delimiter indentation selection) &body children)
  `(make-syntax/node (list-ll ,@children)
                     :collapsed ,collapsed
                     :opening-delimiter ,opening-delimiter
                     :closing-delimiter ,closing-delimiter
                     :separator ,separator
                     :indentation ,indentation
                     :selection ,selection))

;;;;;;
;;; Operation

(def operation operation/syntax ()
  ())

(def operation operation/syntax/toggle-collapsed (operation/syntax)
  ((document :type document)
   (selection :type reference)))

;;;;;;
;;; Construction

(def function make-operation/syntax/toggle-collapsed (document selection)
  (make-instance 'operation/syntax/toggle-collapsed :document document :selection selection))

;;;;;;
;;; Evaluator

(def evaluator operation/syntax/toggle-collapsed (operation)
  (notf (collapsed-p (eval-reference (document-of operation) (flatten-reference (selection-of operation))))))

;;;;;;
;;; API

(def function syntax/reference? (reference)
  (pattern-case (reverse reference)
    (((the ?type (?if (subtypep ?type 'syntax/base)) ?a) . ?rest)
     #t)))

(def function syntax/clone-leaf (document &key opening-delimiter closing-delimiter (indentation nil indentation?) (selection nil selection?))
  (make-syntax/leaf (if selection?
                        (text/make-text (elements-of (content-of document)) :selection (as (nthcdr 1 (va selection))))
                        (content-of document))
                    :opening-delimiter (opening-delimiter-of document) :closing-delimiter (closing-delimiter-of document)
                    :indentation (if indentation? indentation (indentation-of document))
                    :selection (if selection? selection (as (selection-of document)))))

(def function syntax/clone-node (document &key collapsed separator opening-delimiter closing-delimiter (indentation nil indentation?) (selection nil selection?))
  (make-syntax/node (children-of document)
                    #+nil ;; TODO: this breaks common-lisp nested into json for some reason
                    (if selection?
                        (map-ll (ll (children-of document)) (lambda (element) (syntax/clone element :selection (as (nthcdr 2 (va selection))))))
                        (children-of document))
                    :collapsed (collapsed-p document)
                    :opening-delimiter (opening-delimiter-of document) :closing-delimiter (closing-delimiter-of document) :separator (separator-of document)
                    :indentation (if indentation? indentation (indentation-of document))
                    :selection (if selection? selection (as (selection-of document)))))

(def function syntax/clone (document &rest args &key &allow-other-keys)
  (etypecase document
    (syntax/leaf (apply #'syntax/clone-leaf document args))
    (syntax/node (apply #'syntax/clone-node document args))))
