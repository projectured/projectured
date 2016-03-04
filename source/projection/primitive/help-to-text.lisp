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
                                       (for accessible = (accessible-p command))
                                       (for domain-color = (if accessible
                                                               *color/solarized/red*
                                                               (color/lighten *color/solarized/red* 0.75)))
                                       (for gesture-color = (if accessible
                                                                *color/solarized/blue*
                                                                (color/lighten *color/solarized/blue* 0.5)))
                                       (for description-color = (if accessible
                                                                    *color/black*
                                                                    (color/lighten *color/black* 0.75)))
                                       (unless (first-iteration-p)
                                         (collect (text/newline)))
                                       (appending (list
                                                   (text/string (or (domain-of command) "Unspecified") :font *font/ubuntu/regular/18* :font-color domain-color)
                                                   (text/string " " :font *font/default* :font-color *color/black*)
                                                   (text/string (string+ modifier-text gesture-text) :font *font/ubuntu/monospace/regular/18* :font-color gesture-color)
                                                   (text/string " " :font *font/default* :font-color *color/black*)
                                                   (text/string (or (description-of command) "No description") :font *font/ubuntu/regular/18* :font-color description-color)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader help/context-sensitive->text/text ()
  -input-)
