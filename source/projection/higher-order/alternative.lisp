;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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

(def forward-mapper alternative ()
  (funcall (forward-mapper-of (elt (alternatives-of -projection-) (selection-of -projection-))) (content-iomap-of -printer-iomap-) -reference-))

(def backward-mapper alternative ()
  (funcall (backward-mapper-of (elt (alternatives-of -projection-) (selection-of -projection-))) (content-iomap-of -printer-iomap-) -reference-))

;;;;;;
;;; Printer

(def printer alternative ()
  (bind ((selected-alternative-iomap (as (bind ((selection-element (elt (alternatives-of -projection-) (selection-of -projection-))))
                                           (call-printer selection-element -recursion- -input- -input-reference-))))
         (output (as (output-of (va selected-alternative-iomap)))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output selected-alternative-iomap)))

;;;;;;
;;; Reader

(def reader alternative ()
  (merge-commands (bind ((selection-element (elt (alternatives-of -projection-) (selection-of -projection-)))
                         (selected-alternative-iomap (content-iomap-of -printer-iomap-))
                         (command (call-reader selection-element -recursion- -input- selected-alternative-iomap)))
                    (when (and command (operation-of command))
                      command))
                  (gesture-case (gesture-of -input-)
                    ((make-key-press-gesture :scancode-n '(:shift :control))
                     :domain "Generic" :description "Switches to the next alternative notation"
                     :operation (make-operation/select-next-alternative -projection-)))
                  (make-nothing-command -gesture-)))

;;;;;;
;;; Operation

(def operation operation/select-next-alternative ()
  ((projection :type alternatives)))

;;;;;;
;;; Construction

(def function make-operation/select-next-alternative (projection)
  (make-instance 'operation/select-next-alternative :projection projection))

;;;;;;
;;; Evaluator

(def evaluator operation/select-next-alternative ()
  (bind ((projection (projection-of -operation-)))
    (setf (selection-of projection) (mod (1+ (selection-of projection))
                                         (length (alternatives-of projection))))))
