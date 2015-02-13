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
   (selection :type non-negative-integer)))

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
  (bind ((selected-alternative-iomap (as (bind ((selection-element (elt (alternatives-of projection) (selection-of projection))))
                                           (call-printer selection-element recursion input input-reference))))
         (output (as (output-of (va selected-alternative-iomap)))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va selected-alternative-iomap))))))

;;;;;;
;;; Reader

(def reader alternative (projection recursion input printer-iomap)
  (merge-commands (bind ((selection-element (elt (alternatives-of projection) (selection-of projection)))
                         (selected-alternative-iomap (first (child-iomaps-of printer-iomap)))
                         (command (call-reader selection-element recursion input selected-alternative-iomap)))
                    (when (and command (operation-of command))
                      command))
                  (gesture-case (gesture-of input)
                    ((gesture/keyboard/key-press :sdl-key-n '(:shift :control))
                     :domain "Generic" :description "Switches to the next alternative notation"
                     :operation (make-operation/select-next-alternative projection)))
                  (make-command/nothing (gesture-of input))))
