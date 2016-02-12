;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection help/context-sensitive->text/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/help/context-sensitive->text/text ()
  (make-projection 'help/context-sensitive->text/text))

;;;;;;
;;; Construction

(def macro help/context-sensitive->text/text ()
  '(make-projection/help/context-sensitive->text/text))

;;;;;;
;;; Printer

(def printer help/context-sensitive->text/text ()
  (bind ((output (text/make-text (iter (for command :in (available-commands-of -input-))
                                       (for gesture = (gesture-of command))
                                       (for modifier-text = (describe-gesture-modifiers gesture))
                                       (for gesture-text = (describe-gesture-keys gesture))
                                       (unless (first-iteration-p)
                                         (collect (text/newline)))
                                       (appending (list
                                                   (text/string (or (domain-of command) "Unspecified") :font *font/default* :font-color *color/solarized/red*)
                                                   (text/string " " :font *font/default* :font-color *color/black*)
                                                   (text/string (string+ modifier-text gesture-text) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)
                                                   (text/string " " :font *font/default* :font-color *color/black*)
                                                   (text/string (or (description-of command) "No description") :font *font/default* :font-color *color/black*)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader help/context-sensitive->text/text ()
  -input-)
