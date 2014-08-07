;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document state-machine/base ()
  ())

(def document state-machine/state-machine (state-machine/base)
  ((name :type string)
   (states :type sequence)
   (transitions :type sequence)))

(def document state-machine/state (state-machine/base)
  ((name :type string)
   (entry-action :type t)
   (exit-action :type t)))

(def document state-machine/transition (state-machine/base)
  ((name :type string)
   (event :type string)
   (source :type state-machine/state)
   (target :type state-machine/state)
   (action :type t)))

;;;;;;
;;; Construction

(def function make-state-machine/state-machine (name states transitions)
  (make-instance 'state-machine/state-machine :name name :states states :transitions transitions))

(def function make-state-machine/state (name)
  (make-instance 'state-machine/state :name name))

(def function make-state-machine/transition (name event source target)
  (make-instance 'state-machine/transition :name name :event event :source source :target target))

;;;;;;
;;; Construction

(def macro state-machine (name states transitions)
  `(make-state-machine/state-machine ,name (list ,@states) (list ,@transitions)))

(def macro state (name)
  `(make-state-machine/state ,name))

(def macro transition (name event source target)
  `(make-state-machine/transition ,name ,event ,source ,target))
