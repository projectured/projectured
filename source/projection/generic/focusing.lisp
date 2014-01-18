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

(def (function e) make-projection/focusing (part-type part)
  (make-projection 'focusing
                   :part-type part-type
                   :part part
                   :part-evaluator (compile nil `(lambda (document) ,(reference/flatten (reverse part))))))

;;;;;;
;;; Construction

(def (macro e) focusing (part-type part)
  `(make-projection/focusing ,part-type ,part))

;;;;;;
;;; Operation

(def operation operation/focusing/replace-part ()
  ((projection :type focusing)
   (part :type reference)))

(def method redo-operation ((operation operation/focusing/replace-part))
  (bind ((projection (projection-of operation))
         (part (part-of operation)))
    (setf (part-of projection) part)
    (setf (part-evaluator-of projection) (compile nil `(lambda (document) ,(reference/flatten (reverse part)))))))

;;;;;;
;;; Printer

(def printer focusing (projection recursion input input-reference)
  (bind ((output (funcall (part-evaluator-of projection) input)))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader focusing (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore recursion))
  (labels ((recurse (operation)
             (typecase operation
               (operation/quit operation)
               (operation/replace-selection
                (make-operation/replace-selection (input-of projection-iomap)
                                                  (append (selection-of operation) (part-of projection))))
               (operation/sequence/replace-element-range
                (make-operation/sequence/replace-element-range (input-of projection-iomap)
                                                               (append (target-of operation) (part-of projection))
                                                               (replacement-of operation)))
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations)))))))
    (bind ((latest-gesture (first-elt (gestures-of gesture-queue))))
      (merge-operations (gesture-case latest-gesture
                          ((gesture/keyboard/key-press #\, :control)
                           :domain "Focusing" :help "Moves the focus one level up"
                           :operation (when (part-of projection)
                                        (make-instance 'operation/focusing/replace-part
                                                       :projection projection
                                                       :part (iter (for selection :on (rest (part-of projection)))
                                                                   (when (subtypep (second (first selection)) (part-type-of projection))
                                                                     (return selection))))))
                          ((gesture/keyboard/key-press #\. :control)
                           :domain "Focusing" :help "Moves the focus to the selection"
                           :operation (make-instance 'operation/focusing/replace-part
                                                     :projection projection
                                                     :part (iter (for selection :on (selection-of (input-of projection-iomap)))
                                                                 (when (subtypep (second (first selection)) (part-type-of projection))
                                                                   (return selection))))))
                        (recurse operation)))))
