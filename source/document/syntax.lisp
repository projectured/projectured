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

(def document syntax/navigation (syntax/base)
  ((content :type t)))

(def document syntax/concatenation (syntax/base)
  ((children :type sequence)))

(def document syntax/separation (syntax/base)
  ((children :type sequence)
   (separator :type text/text)))

(def document syntax/leaf (syntax/navigation syntax/collapsible syntax/indentation syntax/delimitation)
  ())

(def document syntax/node (syntax/separation syntax/navigation syntax/collapsible syntax/indentation syntax/delimitation)
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

(def function make-syntax/navigation (content &key selection)
  (make-instance 'syntax/navigation
                 :content content
                 :selection selection))

(def function make-syntax/concatenation (children &key selection)
  (make-instance 'syntax/concatenation
                 :children children
                 :selection selection))

(def function make-syntax/separation (children &key separator selection)
  (make-instance 'syntax/separation
                 :children children
                 :separator separator
                 :selection selection))

(def function make-syntax/leaf (content &key collapsed opening-delimiter closing-delimiter indentation selection)
  (make-instance 'syntax/leaf
                 :content content
                 :collapsed collapsed
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
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

(def macro syntax/navigation ((&key selection) &body content)
  `(make-syntax/navigation ,(first content)
                           :selection ,selection))

(def macro syntax/concatenation ((&key selection) &body children)
  `(make-syntax/concatenation (list-ll ,@children)
                              :selection ,selection))

(def macro syntax/separation ((&key separator selection) &body children)
  `(make-syntax/separation (list-ll ,@children)
                           :separator ,separator
                           :selection ,selection))

(def macro syntax/leaf ((&key collapsed opening-delimiter closing-delimiter indentation selection) &body content)
  `(make-syntax/leaf ,(first content)
                     :collapsed ,collapsed
                     :opening-delimiter ,opening-delimiter
                     :closing-delimiter ,closing-delimiter
                     :indentation ,indentation
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

(def operation operation/syntax/toggle-collapsed ()
  ((selection :type reference)))

;;;;;;
;;; Construction

(def function make-operation/syntax/toggle-collapsed (selection)
  (make-instance 'operation/syntax/toggle-collapsed :selection selection))

;;;;;;
;;; Evaluator

(def evaluator operation/syntax/toggle-collapsed ()
  (notf (collapsed-p (eval-reference -document- (flatten-reference (selection-of -operation-))))))

;;;;;;
;;; API

(def function syntax/reference? (reference)
  (reference-case (reverse (force-cc reference))
    (((the ?type (?if (subtypep ?type 'syntax/base)) ?a) . ?rest)
     #t)))

(def function syntax/clone-leaf (document &key (content nil content?) (collapsed nil collapsed?) (opening-delimiter nil opening-delimiter?) (closing-delimiter nil closing-delimiter?) (indentation nil indentation?) (selection nil selection?))
  (make-syntax/leaf (if content? content (content-of document))
                    :collapsed (if collapsed? collapsed (collapsed-p document))
                    :opening-delimiter (if opening-delimiter? opening-delimiter (opening-delimiter-of document))
                    :closing-delimiter (if closing-delimiter? closing-delimiter (closing-delimiter-of document))
                    :indentation (if indentation? indentation (indentation-of document))
                    :selection (if selection? selection (as (selection-of document)))))

(def function syntax/clone-node (document &key (children nil children?) (collapsed nil collapsed?) (separator nil separator?) (opening-delimiter nil opening-delimiter?) (closing-delimiter nil closing-delimiter?) (indentation nil indentation?) (selection nil selection?))
  (make-syntax/node (if children? children (children-of document))
                    :collapsed (if collapsed? collapsed (collapsed-p document))
                    :separator (if separator? separator (separator-of document))
                    :opening-delimiter (if opening-delimiter? opening-delimiter (opening-delimiter-of document))
                    :closing-delimiter (if closing-delimiter? closing-delimiter (closing-delimiter-of document))
                    :indentation (if indentation? indentation (indentation-of document))
                    :selection (if selection? selection (as (selection-of document)))))

(def function syntax/clone (document &rest args &key &allow-other-keys)
  (etypecase document
    (syntax/leaf (apply #'syntax/clone-leaf document args))
    (syntax/node (apply #'syntax/clone-node document args))))
