;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection command->text ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/command->text ()
  (make-projection 'command->text))

;;;;;;
;;; Construction

(def (macro e) command->text ()
  '(make-projection/command->text))

;;;;;;
;;; Printer

(def printer command->text (projection recursion input input-reference)
  (bind ((output (make-text/text (make-command-help-text input))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader command->text (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
