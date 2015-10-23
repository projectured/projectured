;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection focusing ()
  ((part-type :type t)
   (part :type reference)
   (part-evaluator :type function)))

;;;;;;
;;; Construction

(def function make-projection/focusing (part-type part)
  (make-projection 'focusing
                   :part-type part-type
                   :part part
                   :part-evaluator (compile nil `(lambda (document) ,(reference/flatten part)))))

;;;;;;
;;; Construction

(def macro focusing (part-type part)
  `(make-projection/focusing ,part-type ,part))

;;;;;;
;;; Operation

(def operation operation/focusing/replace-part ()
  ((projection :type focusing)
   (part :type reference)))

(def method run-operation ((operation operation/focusing/replace-part))
  (bind ((projection (projection-of operation))
         (part (part-of operation)))
    (setf (part-of projection) part)
    (setf (part-evaluator-of projection) (compile nil `(lambda (document) ,(reference/flatten part))))))

;;;;;;
;;; Printer

(def printer focusing (projection recursion input input-reference)
  (bind ((output (as (funcall (part-evaluator-of projection) input))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def function focusing/read-command (projection input printer-iomap)
  (gesture-case (gesture-of input)
    ((gesture/keyboard/key-press :key :sdl-key-comma :modifiers :control)
     :domain "Focusing" :description "Moves the focus one level up"
     :operation (when (part-of projection)
                  (make-instance 'operation/focusing/replace-part
                                 :projection projection
                                 :part (iter (for selection :on (reverse (butlast (part-of projection))))
                                             (when (subtypep (second (first selection)) (part-type-of projection))
                                               (return (reverse selection)))))))
    ((gesture/keyboard/key-press :key :sdl-key-period :modifiers :control)
     :domain "Focusing" :description "Moves the focus to the selection"
     :operation (make-instance 'operation/focusing/replace-part
                               :projection projection
                               :part (iter (for selection :on (reverse (selection-of (input-of printer-iomap))))
                                           (when (subtypep (second (first selection)) (part-type-of projection))
                                             (return (reverse selection))))))))
(def reader focusing (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (merge-commands (focusing/read-command projection input printer-iomap)
                  (awhen (operation/extend (input-of printer-iomap) (part-of projection) (operation-of input))
                    (make-command/clone input it))
                  (make-command/nothing (gesture-of input))))
