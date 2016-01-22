;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document help/base ()
  ())

(def document help/context-sensitive (help/base)
  ((available-commands :type sequence)))

;;;;;;
;;; Construction

(def function make-help/context-sensitive (available-commands &key selection)
  (make-instance 'help/context-sensitive :available-commands available-commands :selection selection))

;;;;;;
;;; Construction

(def macro help/context-sensitive ((&key selection) &body available-commands)
  `(make-help/context-sensitive (list ,@available-commands) :selection ,selection))
