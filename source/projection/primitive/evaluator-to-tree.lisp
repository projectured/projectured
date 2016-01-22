;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection evaluator/toplevel->tree/node ()
  ())

(def projection evaluator/form->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/evaluator/toplevel->tree/node ()
  (make-projection 'evaluator/toplevel->tree/node))

(def function make-projection/evaluator/form->tree/node ()
  (make-projection 'evaluator/form->tree/node))

;;;;;;
;;; Construction

(def macro evaluator/toplevel->tree/node ()
  '(make-projection/evaluator/toplevel->tree/node))

(def macro evaluator/form->tree/node ()
  '(make-projection/evaluator/form->tree/node))

;;;;;;
;;; Forward mapper

(def function forward-mapper/evaluator/toplevel->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the evaluator/toplevel document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((form-iomap (elt (child-iomaps-of printer-iomap) ?index)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(document-type (output-of form-iomap)) (elt (the sequence document) ,?index)))
                 ?rest
                 form-iomap)))
      (((the tree/node (printer-output (the evaluator/toplevel document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/evaluator/form->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the ?type (form-of (the evaluator/form document)))
        . ?rest)
       (bind ((form-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(document-type (output-of form-iomap)) (elt (the sequence document) 1)))
                 ?rest
                 form-iomap)))
      (((the ?type (result-of (the evaluator/form document)))
        . ?rest)
       (bind ((result-iomap (elt (child-iomaps-of printer-iomap) 1)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(document-type (output-of result-iomap)) (elt (the sequence document) 3)))
                 ?rest
                 result-iomap)))
      (((the tree/node (printer-output (the evaluator/form document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/evaluator/toplevel->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((form-iomap (elt (child-iomaps-of printer-iomap) ?index)))
         (values `((the sequence (elements-of (the evaluator/toplevel document)))
                   (the ,(document-type (input-of form-iomap)) (elt (the sequence document) ,?index)))
                 ?rest
                 form-iomap)))
      (?a
       (append `((the tree/node (printer-output (the evaluator/toplevel document) ,projection ,recursion))) reference)))))

(def function backward-mapper/evaluator/form->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) 1))
        . ?rest)
       (bind ((form-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the ,(document-type (input-of form-iomap)) (form-of (the evaluator/form document))))
                 ?rest
                 form-iomap)))
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) 3))
        . ?rest)
       (bind ((result-iomap (elt (child-iomaps-of printer-iomap) 1)))
         (if result-iomap
             (values `((the ,(document-type (input-of result-iomap)) (result-of (the evaluator/form document))))
                     ?rest
                     result-iomap)
             (append `((the tree/node (printer-output (the evaluator/form document) ,projection ,recursion))) reference))))
      (?a
       (append `((the tree/node (printer-output (the evaluator/form document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer evaluator/toplevel->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (map-ll* (ll (elements-of input)) (lambda (element index)
                                                                 (recurse-printer recursion (value-of element)
                                                                                  `((elt (the sequence document) ,index)
                                                                                    (the sequence (elements-of (the evaluator/toplevel document)))
                                                                                    ,@(typed-reference (document-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/evaluator/toplevel->tree/node)))
         (output (as (make-tree/node (map-ll (va element-iomaps) #'output-of)
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer evaluator/form->tree/node (projection recursion input input-reference)
  (bind ((form-iomap (as (recurse-printer recursion (form-of input)
                                          `((form-of (the evaluator/form document))
                                            ,@(typed-reference (document-type input) input-reference)))))
         (result-iomap (as (when (result-of input)
                             (recurse-printer recursion (result-of input)
                                              `((result-of (the evaluator/form document))
                                                ,@(typed-reference (document-type input) input-reference))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va form-iomap) (va result-iomap))))
                                                (selection-of input)
                                                'forward-mapper/evaluator/form->tree/node)))
         (output (as (make-tree/node (list (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                             (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                               (text/string ">" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                           (output-of (va form-iomap))
                                           (tree/leaf (:indentation 0 :selection (as (nthcdr 2 (va output-selection))))
                                             (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                               (text/string "=" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                           (aif (va result-iomap)
                                                (output-of it)
                                                (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                  (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                    (text/string "?" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))))
                                     :indentation 0
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va form-iomap) (va result-iomap))))))

;;;;;;
;;; Reader

(def reader evaluator/toplevel->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/evaluator/toplevel->tree/node 'backward-mapper/evaluator/toplevel->tree/node)
                    (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-return :control)
                       :domain "Evaluator" :description "Inserts a new form at the selection"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input
                                                                                                          `((the sequence (elements-of (the evaluator/toplevel document)))
                                                                                                            (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                          (list (evaluator/form ()
                                                                                                                  (make-common-lisp/insertion "" 'common-lisp/complete-document
                                                                                                                                              :default-value "enter form"))))
                                                                   (make-operation/replace-selection printer-input
                                                                                                     `((the sequence (elements-of (the evaluator/toplevel document)))
                                                                                                       (the evaluator/form (elt (the sequence document) ,elements-length))
                                                                                                       (the common-lisp/insertion (form-of (the evaluator/form document)))
                                                                                                       (the string (value-of (the common-lisp/insertion document)))
                                                                                                       (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/evaluator/toplevel->tree/node nil)
                    (make-nothing-command (gesture-of input)))))

(def reader evaluator/form->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-e :control)
                       :domain "Evaluator" :description "Evaluates the common lisp form at the selection and outputs the result"
                       :operation (make-operation/functional (lambda ()
                                                               (setf (result-of printer-input)
                                                                     (make-common-lisp/constant (eval (printer-output (form-of printer-input)
                                                                                                                      (sequential
                                                                                                                        (recursive (make-projection/common-lisp->lisp-form))
                                                                                                                        (recursive (make-projection/lisp-form->form)))))))))))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/evaluator/form->tree/node 'backward-mapper/evaluator/form->tree/node)
                    (command/read-backward recursion input printer-iomap 'backward-mapper/evaluator/form->tree/node nil)
                    (make-nothing-command (gesture-of input)))))
