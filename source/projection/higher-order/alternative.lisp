;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection alternative ()
  ((alternatives :type list)
   (selection :type positive-integer)))

;;;;;;
;;; Construction

(def function make-projection/alternative (projections &key (selection 0))
  (make-projection 'alternative :alternatives projections :selection selection))

;;;;;;
;;; Construction

(def macro alternative (&body forms)
  `(make-projection/alternative (list ,@forms)))

;;;;;;
;;; Printer

(def printer alternative (projection recursion input input-reference)
  (bind ((selection-element (elt (alternatives-of projection) (selection-of projection))))
    (call-printer selection-element recursion input input-reference)))

;;;;;;
;;; Reader

(def reader alternative (projection recursion input printer-iomap)
  (cond ((and (typep input 'gesture/keyboard/key-press)
              (eq (key-of input) :sdl-key-p)
              (member :control (modifiers-of input)))
         (make-operation/select-next-alternative projection))
        (t
         (bind ((selection-element (elt (alternatives-of projection) (selection-of projection))))
           (call-reader selection-element recursion input printer-iomap)))))
