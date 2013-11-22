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

(def (function e) make-projection/alternative (projections &key (selection 0))
  (make-projection 'alternative :alternatives projections :selection selection))

;;;;;;
;;; Construction

(def (macro e) alternative (&body forms)
  `(make-projection/alternative (list ,@forms)))

;;;;;;
;;; Printer

(def printer alternative (projection recursion iomap input input-reference output-reference)
  (bind ((selection-element (elt (alternatives-of projection) (selection-of projection))))
    (funcall (printer-of selection-element) selection-element recursion iomap input input-reference output-reference)))

;;;;;;
;;; Reader

(def reader alternative (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (bind ((latest-gesture (first (gestures-of gesture-queue))))
    (cond ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-p)
                (member :control (modifiers-of latest-gesture)))
           (make-operation/select-next-alternative projection))
          (t
           (bind ((selection-element (elt (alternatives-of projection) (selection-of projection))))
             (funcall (reader-of selection-element) selection-element recursion printer-iomap projection-iomap gesture-queue operation document))))))
