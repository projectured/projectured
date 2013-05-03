;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Document

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

(def (function e) make-state-machine/state-machine (name states transitions)
  (make-instance 'state-machine/state-machine :name name :states states :transitions transitions))

(def (function e) make-state-machine/state (name)
  (make-instance 'state-machine/state :name name))

(def (function e) make-state-machine/transition (name event source target)
  (make-instance 'state-machine/transition :name name :event event :source source :target target))

;;;;;;
;;; Construction

(def (macro e) state-machine (name states transitions)
  `(make-state-machine/state-machine ,name (list ,@states) (list ,@transitions)))

(def (macro e) state (name)
  `(make-state-machine/state ,name))

(def (macro e) transition (name event source target)
  `(make-state-machine/transition ,name ,event ,source ,target))

;;;;;;
;;; Provider

(def (function e) state-machine-font-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from state-machine-font-color-provider *color/solarized/gray*))
                    ((the character (elt (the string (name-of (the ?type (?if (subtypep ?type 'state-machine/base))  ?a))) ?b))
                     (return-from state-machine-font-color-provider *color/solarized/red*))
                    ((the character (elt (the string (event-of (the ?type (?if (subtypep ?type 'state-machine/base))  ?a))) ?b))
                     (return-from state-machine-font-color-provider *color/solarized/red*))
                    ((the character (elt (the string (keyword (the ?type (?if (subtypep ?type 'state-machine/base))  ?a))) ?b))
                     (return-from state-machine-font-color-provider *color/solarized/blue*))
                    ((the character (elt (the string (label (the ?type ?a))) ?b))
                     (return-from state-machine-font-color-provider *color/solarized/green*))))))

(def (function e) state-machine-font-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (name-of (the ?type (?if (subtypep ?type 'state-machine/base))  ?a))) ?b))
                     (return-from state-machine-font-provider *font/ubuntu/monospace/bold/18*))
                    ((the character (elt (the string (event-of (the ?type (?if (subtypep ?type 'state-machine/base))  ?a))) ?b))
                     (return-from state-machine-font-provider *font/ubuntu/monospace/bold/18*))))))

(def (function e) state-machine-delimiter-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  )))

(def (function e) state-machine-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore next-child-reference))
  " "
  #+nil
  (map-backward iomap previous-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  )))

(def (function e) state-machine-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore previous-child-reference parent-node))
  (map-backward iomap next-child-reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the state-machine/state ?a)
                     (return-from state-machine-indentation-provider 2))
                    ((the state-machine/transition ?a)
                     (return-from state-machine-indentation-provider 2))))))
