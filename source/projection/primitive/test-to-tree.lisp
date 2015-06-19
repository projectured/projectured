;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection test/check->tree/node ()
  ())

(def projection test/result->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def function make-projection/test/check->tree/node ()
  (make-projection 'test/check->tree/node))

(def function make-projection/test/result->tree/leaf ()
  (make-projection 'test/result->tree/leaf))

;;;;;;
;;; Construction

(def macro test/check->tree/node ()
  '(make-projection/test/check->tree/node))

(def macro test/result->tree/leaf ()
  '(make-projection/test/result->tree/leaf))

;;;;;;
;;; Printer

(def printer test/check->tree/node (projection recursion input input-reference)
  (bind ((evaluator (evaluator-of input))
         (result (when evaluator (result-of evaluator)))
         ;; KLUDGE: should get the original form used during evaluation instead
         (form (list (find-symbol "IS" "HU.DWIM.STEFIL") (printer-output (content-of input) (make-projection/common-lisp->form))))
         ;; KLUDGE: should rather use eq
         (success-count (if result (count form (succeeded-assertions-of result) :test 'equal) 0))
         (failure-count (if result (count form (failed-assertions-of result) :test 'equal) 0))
         (content-iomap (recurse-printer recursion (content-of input) `((content-of (the test/check document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the ?type (content-of (the test/check document))) . ?rest)
                              (append (selection-of (output-of content-iomap))
                                      `((the tree/leaf (elt (the sequence document) 1))
                                        (the sequence (children-of (the tree/node document)))
                                        (the tree/node (elt (the sequence document) 0))
                                        (the sequence (arguments-of (the common-lisp/application document))))))
                             (((the common-lisp/application (printer-output (the test/check document) ?projection ?recursion)) . ?rest)
                              (when (eq projection ?projection)
                                (reverse ?rest)))))
         (output (make-common-lisp/application (make-lisp-form/symbol "IS" "HU.DWIM.STEFIL")
                                               (list (tree/node (:separator (text/text () (text/string " ")) :selection (butlast output-selection 2))
                                                       (tree/leaf (:selection (butlast output-selection 4))
                                                         (text/text (:selection (butlast output-selection 5))
                                                           (image/file () (resource-pathname (cond ((and (zerop failure-count)
                                                                                                          (zerop success-count))
                                                                                                     "image/unknown.png")
                                                                                                    ((not (zerop failure-count))
                                                                                                     "image/error.png")
                                                                                                    ((not (zerop success-count))
                                                                                                     "image/ok.png"))))))
                                                       (output-of content-iomap)))
                                               :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

(def printer test/result->tree/leaf (projection recursion input input-reference)
  (bind ((error (error-of input))
         (failure-count (length (failed-assertions-of input)))
         (success-count (length (succeeded-assertions-of input)))
         (assertion-count (+ failure-count success-count))
         (output-selection nil)
         (output (tree/leaf (:indentation 0 :selection output-selection)
                   (text/text (:selection (butlast output-selection 1))
                     (image/file () (resource-pathname (if (> failure-count 0) "image/error.png" "image/ok.png")))
                     (text/string " ")
                     (text/string (princ-to-string assertion-count) :font *font/liberation/serif/regular/24* :font-color *color/solarized/green*)
                     (text/string " assertions, " :font *font/liberation/serif/regular/24* :font-color *color/solarized/blue*)
                     (text/string (princ-to-string failure-count) :font *font/liberation/serif/regular/24* :font-color (if (> failure-count 0) *color/solarized/red* *color/solarized/green*))
                     (text/string " failures, " :font *font/liberation/serif/regular/24* :font-color *color/solarized/blue*)
                     (text/string (if error "1" "0") :font *font/liberation/serif/regular/24* :font-color (if error *color/solarized/red* *color/solarized/green*))
                     (text/string (string+ " error" (when error ": ")) :font *font/liberation/serif/regular/24* :font-color *color/solarized/blue*)
                     (text/string (if error (princ-to-string error) "") :font *font/liberation/serif/regular/24* :font-color *color/solarized/red*)))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader test/check->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (arguments-of (the common-lisp/application document)))
                                                    (the tree/node (elt (the sequence document) 0))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 1))
                                                    . ?rest)
                                                   (bind ((content (content-of printer-input))
                                                          (input-operation (make-operation/replace-selection (content-of printer-input) (reverse ?rest)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) 0)))))
                                                     (append (selection-of output-operation)
                                                             `((the ,(form-type content) (content-of (the test/check document)))))))
                                                  (?a
                                                   (append (selection-of operation) `((the common-lisp/application (printer-output (the test/check document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-range
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (arguments-of (the common-lisp/application document)))
                                                    (the tree/node (elt (the sequence document) 0))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 1))
                                                    . ?rest)
                                                   (bind ((content (content-of printer-input))
                                                          (input-operation (make-operation/sequence/replace-range (content-of printer-input) (reverse ?rest) (replacement-of operation)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) 0)))))
                                                     (append (selection-of output-operation)
                                                             `((the ,(form-type content) (content-of (the test/check document))))))))
                                           (make-operation/sequence/replace-range printer-input it (replacement-of operation))))
                                        (operation/number/replace-range
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (arguments-of (the common-lisp/application document)))
                                                    (the tree/node (elt (the sequence document) 0))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 1))
                                                    . ?rest)
                                                   (bind ((content (content-of printer-input))
                                                          (input-operation (make-operation/number/replace-range (content-of printer-input) (reverse ?rest) (replacement-of operation)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) 0)))))
                                                     (append (selection-of output-operation)
                                                             `((the ,(form-type content) (content-of (the test/check document))))))))
                                           (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-command/nothing (gesture-of input)))))

(def reader test/result->tree/leaf (projection recursion input printer-iomap)
  (make-command/nothing (gesture-of input)))
