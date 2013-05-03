;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Document

(def document graph/base ()
  ())

(def document graph/graph (graph/base)
  ((vertices :type sequence)
   (edges :type sequence)))

(def document graph/vertex (graph/base)
  ((content :type string)))

(def document graph/edge (graph/base)
  ((source :type graph/vertex)
   (target :type graph/vertex)))

;;;;;;
;;; Construction

(def (function e) make-graph/graph (vertices edges)
  (make-instance 'graph/graph :vertices vertices :edges edges))

(def (function e) make-graph/vertex (content)
  (make-instance 'graph/vertex :content content))

(def (function e) make-graph/edge (source target)
  (make-instance 'graph/edge :source source :target target))

;;;;;;
;;; Construction

(def (macro e) graph (() vertices edges)
  `(make-graph/graph (list ,@vertices) (list ,@edges)))

(def (macro e) vertex (() content)
  `(make-graph/vertex ,content))

(def (macro e) edge (() source target)
  `(make-graph/edge ,source ,target))
