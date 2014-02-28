;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection evaluator/evaluator->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/evaluator/evaluator->tree/node ()
  (make-projection 'evaluator/evaluator->tree/node))

;;;;;;
;;; Construction

(def (macro e) evaluator/evaluator->tree/node ()
  '(make-projection/evaluator/evaluator->tree/node))

;;;;;;
;;; Printer

(def printer evaluator/evaluator->tree/node (projection recursion input input-reference)
  (bind ((result (if (on-demand-p input)
                     (result-of input)
                     (bind ((form (printer-output (form-of input) (make-projection/t->form))))
                       (setf (result-of input)
                             (block nil
                               (handler-bind ((warning (lambda (condition)
                                                         (muffle-warning condition)))
                                              (serious-condition (lambda (condition)
                                                                   (return (princ-to-string condition)))))
                                 (funcall (dynamic-environment-provider-of input) (lambda () (eval form)))))))))
         (form-iomap (recurse-printer recursion (form-of input) `((form-of (the evaluator/evaluator document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (result-iomap (recurse-printer recursion result `((result-of (the evaluator/evaluator document))
                                                           ,@(typed-reference (form-type input) input-reference))))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the ?value-type (form-of (the evaluator/evaluator document)))
                               . ?rest)
                              (append (selection-of (output-of form-iomap))
                                      `((the ,(form-type (output-of form-iomap)) (elt (the sequence document) 0))
                                        (the sequence (children-of (the tree/node document))))))
                             (((the tree/node (printer-output (the evaluator/evaluator document) ?projection ?recursion)) . ?rest)
                              (when (and (eq projection ?projection) (eq recursion ?recursion))
                                (reverse ?rest)))))
         (output (tree/node (:selection output-selection)
                   #+nil
                   (tree/leaf (:selection (butlast output-selection 2))
                     (text/text (:selection (butlast output-selection 3))
                       (image/image (asdf:system-relative-pathname :projectured "etc/refresh.png"))
                       (text/string "Evaluate" :font *font/liberation/serif/regular/24* :font-color *color/solarized/green*)))
                   (output-of form-iomap)
                   (output-of result-iomap))))
    #+nil
    (setf (indentation-of (output-of form-iomap)) 0)
    (setf (indentation-of (output-of result-iomap)) 0)
    (set-selection (output-of form-iomap) (butlast output-selection 2))
    (set-selection (output-of result-iomap) (butlast output-selection 2))
    (make-iomap/compound projection recursion input input-reference output (list form-iomap result-iomap))))

;;;;;;
;;; Reader

(def reader evaluator/evaluator->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (labels ((recurse (operation)
                               (typecase operation
                                 (operation/quit operation)
                                 (operation/replace-selection
                                  (make-operation/replace-selection printer-input (append (selection-of operation) (last (selection-of printer-input)))))
                                 (operation/sequence/replace-element-range
                                  (make-operation/sequence/replace-element-range printer-input (append (target-of operation) (last (selection-of printer-input))) (replacement-of operation)))
                                 (operation/number/replace-range
                                  (make-operation/number/replace-range printer-input (append (target-of operation) (last (selection-of printer-input))) (replacement-of operation)))
                                 (operation/replace-target
                                  (make-operation/replace-target printer-input (append (target-of operation) (last (selection-of printer-input))) (replacement-of operation)))
                                 (operation/compound
                                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                                    (unless (some 'null operations)
                                      (make-operation/compound operations)))))))
                      (pattern-case (reverse (selection-of printer-input))
                        (((the ?type (form-of (the evaluator/evaluator document)))
                          . ?rest)
                         (bind ((content-command (recurse-reader recursion (make-command (gesture-of input) nil :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) 0)))
                                (content-operation (recurse (operation-of content-command))))
                           (when content-operation
                             (make-command (gesture-of input)
                                           content-operation
                                           :domain (domain-of content-command)
                                           :description (description-of content-command)))))))
                    (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 0))
                                                    . ?rest)
                                                   (bind ((body (form-of printer-input))
                                                          (input-operation (make-operation/replace-selection body (reverse ?rest)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) 0)))))
                                                     (when (typep output-operation 'operation/replace-selection)
                                                       (append (selection-of output-operation)
                                                               `((the ,(form-type body) (form-of (the evaluator/evaluator document))))))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/node (printer-output (the evaluator/evaluator document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-element-range)
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))
