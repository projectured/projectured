;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection inlining ()
  ())

;;;;;;
;;; Construction

(def function make-projection/inlining ()
  (make-projection 'inlining))

;;;;;;
;;; Construction

(def macro inlining ()
  '(make-projection/inlining))

;;;;;;
;;; Forward mapper

(def forward-mapper inlining ()
  (pattern-case -reference-
    (((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
      (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
      (the sequence (body-of (the common-lisp/function-definition document)))
      . ?rest)
     `((the sequence (body-of (the common-lisp/let document)))
       ,@?rest))
    (((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
      (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
      (the sequence (bindings-of (the common-lisp/function-definition document)))
      (the common-lisp/required-function-argument (elt (the sequence document) ?index))
      (the s-expression/symbol (name-of (the common-lisp/required-function-argument document)))
      . ?rest)
     `((the sequence (bindings-of (the common-lisp/let document)))
       (the common-lisp/lexical-variable-binding (elt (the sequence document) ,?index))
       (the s-expression/symbol (name-of (the common-lisp/lexical-variable-binding document)))
       ,@?rest))
    (((the sequence (arguments-of (the common-lisp/application document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     `((the sequence (bindings-of (the common-lisp/let document)))
       (the common-lisp/lexical-variable-binding (elt (the sequence document) ,?index))
       (the ,?type (value-of (the common-lisp/lexical-variable-binding document)))
       ,@?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper inlining ()
  (pattern-case -reference-
    (((the sequence (body-of (the common-lisp/let document)))
      . ?rest)
     `((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
       (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
       (the sequence (body-of (the common-lisp/function-definition document)))
       ,@?rest))
    (((the sequence (bindings-of (the common-lisp/let document)))
      (the common-lisp/lexical-variable-binding (elt (the sequence document) ?index))
      (the s-expression/symbol (name-of (the common-lisp/lexical-variable-binding document)))
      . ?rest)
     `((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
       (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
       (the sequence (bindings-of (the common-lisp/function-definition document)))
       (the common-lisp/required-function-argument (elt (the sequence document) ,?index))
       (the s-expression/symbol (name-of (the common-lisp/required-function-argument document)))
       ,@?rest))
    (((the sequence (bindings-of (the common-lisp/let document)))
      (the common-lisp/lexical-variable-binding (elt (the sequence document) ?index))
      (the ?type (value-of (the common-lisp/lexical-variable-binding document)))
      . ?rest)
     `((the sequence (arguments-of (the common-lisp/application document)))
       (the ,?type (elt (the sequence document) ,?index))
       ,@?rest))))

;;;;;;
;;; printer

(def printer inlining ()
  (bind ((output-selection (as (print-selection -printer-iomap- (get-selection -input-))))
         (output (as (bind ((operator (operator-of -input-)))
                       ;; TODO: delete kludge to avoid infinite inlining of recursion functions
                       (if (and (typep operator 'common-lisp/function-reference) (< (length -input-reference-) 16))
                           (make-common-lisp/let (map-ll* (arguments-of -input-)
                                                          (lambda (argument-element index)
                                                            (bind ((binding (elt (bindings-of (function-of operator)) index)))
                                                              (make-common-lisp/lexical-variable-binding (name-of binding) (value-of argument-element)
                                                                                                         :selection (as (nthcdr 2 (va output-selection)))))))
                                                 (body-of (function-of operator))
                                                 :selection output-selection)
                           (make-common-lisp/application operator
                                                         (map-ll* (arguments-of -input-)
                                                                  (lambda (argument-element index)
                                                                    (bind ((argument (value-of argument-element)))
                                                                      (output-of (recurse-printer -recursion- argument
                                                                                                  `((elt (the sequence document) ,index)
                                                                                                    (the sequence (arguments-of (the ,(document-type -input-) document)))
                                                                                                    ,@(typed-reference (document-type -input-) -input-reference-)))))))))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader inlining ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap- nil)
                  (make-nothing-command -gesture-)))
