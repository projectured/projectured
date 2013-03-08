;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) alternative ()
  ((alternatives :type list)
   (selected :type positive-integer)))

;;;;;;
;;; Construction

(def (function e) make-projection/alternative (projections &key (selected 0))
  (make-projection 'alternative :alternatives projections :selected selected))

;;;;;;
;;; Construction

(def (macro e) alternative (&body forms)
  `(make-projection/alternative (list ,@forms)))

;;;;;;
;;; Printer

(def printer alternative (projection recursion iomap input input-reference output-reference)
  (bind ((selected-element (elt (alternatives-of projection) (selected-of projection))))
    (funcall (printer-of selected-element) selected-element recursion iomap input input-reference output-reference)))

;;;;;;
;;; Reader

(def reader alternative (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (bind ((latest-gesture (first (gestures-of gesture-queue))))
    (cond ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (eq (key-of latest-gesture) :sdl-key-p)
                (member :sdl-key-mod-lctrl (modifiers-of latest-gesture)))
           (make-operation/select-next-alternative projection))
          (t
           (bind ((selected-element (elt (alternatives-of projection) (selected-of projection))))
             (funcall (reader-of selected-element) selected-element recursion printer-iomap projection-iomap gesture-queue operation document))))))
