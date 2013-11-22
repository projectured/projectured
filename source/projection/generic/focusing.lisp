;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection focusing ()
  ((part :type reference)
   (part-evaluator :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/focusing (part)
  (make-projection 'focusing
                   :part part
                   :part-evaluator (compile nil `(lambda (document) ,part))))

;;;;;;
;;; Construction

(def (macro e) focusing (part)
  `(make-projection/focusing ,part))

;;;;;;
;;; Printer

(def printer focusing (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((part (part-of projection))
         (output (funcall (part-evaluator-of projection) input)))
    (make-iomap/recursive* projection recursion
                           output (tree-replace part 'document input-reference)
                           output `(the ,(form-type output) ,output-reference))))

;;;;;;
;;; Reader

(def operation operation/focusing/replace-part ()
  ((projection :type focusing)
   (part :type reference)))

(def method redo-operation ((operation operation/focusing/replace-part))
  (bind ((projection (projection-of operation))
         (part (part-of operation)))
    (setf (part-of projection) part)
    (setf (part-evaluator-of projection) (compile nil `(lambda (document) ,part)))))

(def reader focusing (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore recursion printer-iomap))
  (bind ((latest-gesture (first-elt (gestures-of gesture-queue)))
         (document (input-of document-iomap))
         (child-operation (operation/read-backward operation projection-iomap document-iomap)))
    (merge-operations (gesture-case latest-gesture
                        ((gesture/keyboard/key-press #\, :control)
                         :domain "Focusing" :help "Moves the focus one level up"
                         :operation (pattern-case (part-of projection)
                                      ((the ?a (?b (the ?c ?d)))
                                       (make-instance 'operation/focusing/replace-part
                                                      :projection projection
                                                      :part `(the ,?c ,?d)))
                                      ((the ?a (elt (the list (?b (the ?c ?d))) ?e))
                                       (make-instance 'operation/focusing/replace-part
                                                      :projection projection
                                                      :part `(the ,?c ,?d)))))
                        ((gesture/keyboard/key-press #\. :control)
                         :domain "Focusing" :help "Moves the focus to the selection"
                         :operation (pattern-case (selection-of document)
                                      ((the sequence-position ?a)
                                       nil)
                                      ((the ?a ?b)
                                       (bind ((new-part (tree-replace `(the ,?a ,?b) '(content-of (the document document)) 'document)))
                                         (unless (equal new-part (part-of projection))
                                           (make-instance 'operation/focusing/replace-part
                                                          :projection projection
                                                          :part new-part)))))))
                      child-operation)))
