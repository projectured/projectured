;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection evaluator/toplevel->syntax/node ()
  ())

(def projection evaluator/form->syntax/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/evaluator/toplevel->syntax/node ()
  (make-projection 'evaluator/toplevel->syntax/node))

(def function make-projection/evaluator/form->syntax/node ()
  (make-projection 'evaluator/form->syntax/node))

;;;;;;
;;; Construction

(def macro evaluator/toplevel->syntax/node ()
  '(make-projection/evaluator/toplevel->syntax/node))

(def macro evaluator/form->syntax/node ()
  '(make-projection/evaluator/form->syntax/node))

;;;;;;
;;; Forward mapper

(def forward-mapper evaluator/toplevel->syntax/node ()
  (reference-case -reference-
    (((the sequence (elements-of (the evaluator/toplevel document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((form-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of form-iomap)) (elt (the sequence document) ,?index)))
               ?rest
               form-iomap)))))

(def forward-mapper evaluator/form->syntax/node ()
  (reference-case -reference-
    (((the ?type (form-of (the evaluator/form document)))
      . ?rest)
     (bind ((form-iomap (elt (child-iomaps-of -printer-iomap-) 0)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of form-iomap)) (elt (the sequence document) 1)))
               ?rest
               form-iomap)))
    (((the ?type (result-of (the evaluator/form document)))
      . ?rest)
     (bind ((result-iomap (elt (child-iomaps-of -printer-iomap-) 1)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of result-iomap)) (elt (the sequence document) 3)))
               ?rest
               result-iomap)))))

;;;;;;
;;; Backward mapper

(def backward-mapper evaluator/toplevel->syntax/node ()
  (reference-case -reference-
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((form-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (elements-of (the evaluator/toplevel document)))
                 (the ,(document-type (input-of form-iomap)) (elt (the sequence document) ,?index)))
               ?rest
               form-iomap)))))

(def backward-mapper evaluator/form->syntax/node ()
  (reference-case -reference-
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) 1))
      . ?rest)
     (bind ((form-iomap (elt (child-iomaps-of -printer-iomap-) 0)))
       (values `((the ,(document-type (input-of form-iomap)) (form-of (the evaluator/form document))))
               ?rest
               form-iomap)))
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) 3))
      . ?rest)
     (bind ((result-iomap (elt (child-iomaps-of -printer-iomap-) 1)))
       (when result-iomap
         (values `((the ,(document-type (input-of result-iomap)) (result-of (the evaluator/form document))))
                 ?rest
                 result-iomap))))))

;;;;;;
;;; Printer

(def printer evaluator/toplevel->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (ll (elements-of -input-)) (lambda (element index)
                                                                   (recurse-printer -recursion- (value-of element)
                                                                                    `((elt (the sequence document) ,index)
                                                                                      (the sequence (elements-of (the evaluator/toplevel document)))
                                                                                      ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (map-ll (va element-iomaps) #'output-of)
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer evaluator/form->syntax/node ()
  (bind ((form-iomap (as (recurse-printer -recursion- (form-of -input-)
                                          `((form-of (the evaluator/form document))
                                            ,@(typed-reference (document-type -input-) -input-reference-)))))
         (result-iomap (as (when (result-of -input-)
                             (recurse-printer -recursion- (result-of -input-)
                                              `((result-of (the evaluator/form document))
                                                ,@(typed-reference (document-type -input-) -input-reference-))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (list (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                               (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                 (text/string ">" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                             (output-of (va form-iomap))
                                             (syntax/leaf (:indentation 0 :selection (as (nthcdr 2 (va output-selection))))
                                               (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                 (text/string "=" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                             (aif (va result-iomap)
                                                  (output-of it)
                                                  (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                    (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                      (text/string "not evaluated yet" :font *font/ubuntu/monospace/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75))))))
                                       :indentation 0
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va form-iomap) (va result-iomap))))))

;;;;;;
;;; Reader

(def reader evaluator/toplevel->syntax/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-insert)
                     :domain "Evaluator" :description "Starts a generic insertion into the elements of the toplevel"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the evaluator/toplevel document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                         (list (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                 (the string (subseq (the string document) 0 0)))))))))
                    ((make-key-press-gesture :scancode-return :control)
                     :domain "Evaluator" :description "Inserts a new form at the selection"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/compound (list (make-operation/sequence/replace-range -printer-input-
                                                                                                        `((the sequence (elements-of (the evaluator/toplevel document)))
                                                                                                          (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                        (list (evaluator/form ()
                                                                                                                (make-common-lisp/insertion "" (make-instance 'evaluator/completion :toplevel -printer-input-)
                                                                                                                                            :default-value "enter form"))))
                                                                 (make-operation/replace-selection -printer-input-
                                                                                                   `((the sequence (elements-of (the evaluator/toplevel document)))
                                                                                                     (the evaluator/form (elt (the sequence document) ,elements-length))
                                                                                                     (the common-lisp/insertion (form-of (the evaluator/form document)))
                                                                                                     (the string (value-of (the common-lisp/insertion document)))
                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader evaluator/form->syntax/node ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-e :control)
                     :domain "Evaluator" :description "Evaluates the common lisp form at the selection and outputs the result"
                     :operation (make-operation/functional (lambda ()
                                                             (setf (result-of -printer-input-)
                                                                   (make-common-lisp/constant* (eval (printer-output (form-of -printer-input-)
                                                                                                                     (sequential
                                                                                                                       (recursive (make-projection/common-lisp->s-expression))
                                                                                                                       (recursive (make-projection/s-expression->form)))))))))))
                  (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))
