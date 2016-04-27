;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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
                   :part-evaluator (compile nil `(lambda (document) ,(flatten-reference part)))))

;;;;;;
;;; Construction

(def macro focusing (part-type part)
  `(make-projection/focusing ,part-type ,part))

;;;;;;
;;; Printer

(def printer focusing ()
  (bind ((output (as (funcall (part-evaluator-of -projection-) -input-))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader focusing ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-comma :control)
                     :domain "Focusing" :description "Moves the focus one level up"
                     :operation (when (part-of -projection-)
                                  (make-instance 'operation/focusing/replace-part
                                                 :projection -projection-
                                                 :part (iter (for selection :on (reverse (butlast (part-of -projection-))))
                                                             (when (subtypep (second (first selection)) (part-type-of -projection-))
                                                               (return (reverse selection)))))))
                    ((make-key-press-gesture :scancode-period :control)
                     :domain "Focusing" :description "Moves the focus to the selection"
                     :operation (make-instance 'operation/focusing/replace-part
                                               :projection -projection-
                                               :part (iter (for selection :on (reverse-cc (get-selection (input-of -printer-iomap-))))
                                                           (when (subtypep (second (first selection)) (part-type-of -projection-))
                                                             (return (reverse selection)))))))
                  (awhen (operation/extend (input-of -printer-iomap-) (part-of -projection-) (operation-of -input-))
                    (clone-command -input- it))
                  (make-nothing-command (gesture-of -input-))))

;;;;;;
;;; Operation

(def operation operation/focusing/replace-part ()
  ((projection :type focusing)
   (part :type reference)))

;;;;;;
;;; Evaluator

(def evaluator operation/focusing/replace-part ()
  (bind ((projection (projection-of -operation-))
         (part (part-of -operation-)))
    (setf (part-of projection) part)
    (setf (part-evaluator-of projection) (compile nil `(lambda (document) ,(flatten-reference part))))))
