;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document test/base (selection/base)
  ())

(def document test/result (test/base)
  ((failed-assertions :type sequence)
   (succeeded-assertions :type sequence)
   (error :type serious-condition)))

(def document test/check (test/base)
  ((content :type common-lisp/base)
   (evaluator :type evaluator/evaluator)))

;;;;;;
;;; Construction

(def (function e) make-test/result (failed-assertions succeeded-assertions error)
  (make-instance 'test/result :failed-assertions failed-assertions :succeeded-assertions succeeded-assertions :error error))

(def (function e) make-test/check (content evaluator)
  (make-instance 'test/check :content content :evaluator evaluator))
