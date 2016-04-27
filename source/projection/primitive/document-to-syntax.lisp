;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/nothing->syntax/leaf ()
  ())

(def projection document/insertion->syntax/leaf ()
  ((factory :type function)))

;;;;;;
;;; Construction

(def function make-projection/document/nothing->syntax/leaf ()
  (make-projection 'document/nothing->syntax/leaf))

(def function make-projection/document/insertion->syntax/leaf (factory)
  (make-projection 'document/insertion->syntax/leaf :factory factory))

;;;;;;
;;; Construction

(def macro document/nothing->syntax/leaf ()
  `(make-projection/document/nothing->syntax/leaf))

(def macro document/insertion->syntax/leaf (factory)
  `(make-projection/document/insertion->syntax/leaf ,factory))

;;;;;;
;;; Printer

(def printer document/nothing->syntax/leaf ()
  (bind ((output-selection (as (reference-case (get-selection -input-)
                                 (((the string (value-of (the document/nothing document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  `((the text/text (content-of (the syntax/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index)))))))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (value-of -input-) :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer document/insertion->syntax/leaf ()
  (bind ((output-selection (as (reference-case (get-selection -input-)
                                 (((the string (value-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  (bind ((character-index (+ (length (prefix-of -input-)) ?character-index)))
                                    `((the text/text (content-of (the syntax/leaf document)))
                                      (the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the string (prefix-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  `((the text/text (content-of (the syntax/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
                                 (((the string (suffix-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  (bind ((character-index (+ (length (prefix-of -input-)) (length (value-of -input-)) ?character-index)))
                                    `((the text/text (content-of (the syntax/leaf document)))
                                      (the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
         (output (syntax/leaf (:selection output-selection)
                   (as (bind (((:values nil completion) (funcall (factory-of -projection-) (value-of -input-)))
                              (commitable? (not (null (funcall (factory-of -projection-) (string+ (value-of -input-) completion)))))
                              (value-color (if commitable? *color/solarized/green* *color/solarized/red*))
                              (font (or (font-of -input-) *font/liberation/serif/regular/24*)))
                         (text/text (:selection (as (nthcdr 1 (va output-selection))))
                           (text/string (prefix-of -input-) :font font :font-color (color/lighten *color/solarized/gray* 0.75))
                           (text/string (value-of -input-) :font font :font-color value-color)
                           (text/string (if completion (if commitable? (string+ completion "?") completion) "") :font font :font-color (color/lighten value-color 0.75))
                           (text/string (suffix-of -input-) :font font :font-color (color/lighten *color/solarized/gray* 0.75))))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader document/nothing->syntax/leaf ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-insert)
                     :domain "Document" :description "Starts a generic insertion into the document"
                     :operation (make-operation/compound (list (make-operation/replace-target nil (document/insertion ()))
                                                               (make-operation/replace-selection `((the string (value-of (the document/insertion document)))
                                                                                                   (the string (subseq (the string document) 0 0))))))))
                  (make-command -gesture-
                                (labels ((recurse (operation)
                                           (typecase operation
                                             (operation/quit operation)
                                             (operation/functional operation)
                                             (operation/replace-selection
                                              (awhen (reference-case (selection-of operation)
                                                       (((the text/text (content-of (the syntax/leaf document)))
                                                         (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                        `((the string (value-of (the document/nothing document)))
                                                          (the string (subseq (the string document) ,?character-index ,?character-index)))))
                                                (make-operation/replace-selection it)))
                                             (operation/show-context-sensitive-help
                                              (make-instance 'operation/show-context-sensitive-help
                                                             :commands (iter (for command :in (commands-of operation))
                                                                             (awhen (recurse (operation-of command))
                                                                               (collect (make-instance 'command
                                                                                                       :gesture (gesture-of command)
                                                                                                       :domain (domain-of command)
                                                                                                       :description (description-of command)
                                                                                                       :operation it))))))
                                             (operation/compound
                                              (bind ((operations (mapcar #'recurse (elements-of operation))))
                                                (unless (some 'null operations)
                                                  (make-operation/compound operations)))))))
                                  (recurse (operation-of -input-)))
                                :domain (domain-of -input-)
                                :description (description-of -input-))
                  (make-nothing-command -gesture-)))

(def reader document/insertion->syntax/leaf ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-tab)
                     :domain "Document" :description "Inserts the suggested completion at the selection"
                     :operation (bind (((:values nil completion) (funcall (factory-of -projection-) (value-of -printer-input-)))
                                       (end (length (value-of -printer-input-))))
                                  (values (when completion
                                            (make-operation/string/replace-range `((the string (value-of (the document/insertion document)))
                                                                                   (the string (subseq (the string document) ,end ,end))) completion))
                                          #t)))
                    ((make-key-press-gesture :scancode-return)
                     :domain "Document" :description "Inserts a new object of the provided type"
                     :operation (bind (((:values immediate-new-instance completion) (funcall (factory-of -projection-) (value-of -printer-input-)))
                                       (new-instance (or immediate-new-instance
                                                         (funcall (factory-of -projection-) (string+ (value-of -printer-input-) completion)))))
                                  (values (when new-instance
                                            (make-operation/compound (list (make-operation/replace-target nil new-instance)
                                                                           #+nil ;; TODO: causes the initial selection to fail
                                                                           (make-operation/replace-selection nil))))
                                          #t)))
                    ((make-key-press-gesture :scancode-escape)
                     :domain "Document" :description "Aborts the object insertion"
                     :operation (make-operation/replace-target nil (document/nothing ()))))
                  (make-command -gesture-
                                (labels ((recurse (operation)
                                           (typecase operation
                                             (operation/quit operation)
                                             (operation/functional operation)
                                             (operation/replace-selection
                                              (make-operation/replace-selection (reference-case (selection-of operation)
                                                                                  (((the text/text (content-of (the syntax/leaf document)))
                                                                                    (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                                                   (bind ((prefix-start-index 0)
                                                                                          (prefix-end-index (+ prefix-start-index (length (prefix-of -printer-input-))))
                                                                                          (value-start-index prefix-end-index)
                                                                                          (value-end-index (+ value-start-index (length (value-of -printer-input-))))
                                                                                          (suffix-start-index value-end-index)
                                                                                          (suffix-end-index (+ suffix-start-index (length (suffix-of -printer-input-)))))
                                                                                     (econd ((<= value-start-index ?character-index value-end-index)
                                                                                             (bind ((character-index (- ?character-index value-start-index)))
                                                                                               `((the string (value-of (the document/insertion document)))
                                                                                                 (the string (subseq (the string document) ,character-index ,character-index)))))
                                                                                            ((<= prefix-start-index ?character-index prefix-end-index)
                                                                                             (bind ((character-index (- ?character-index prefix-start-index)))
                                                                                               `((the string (prefix-of (the document/insertion document)))
                                                                                                 (the string (subseq (the string document) ,character-index ,character-index)))))
                                                                                            ((<= suffix-start-index ?character-index suffix-end-index)
                                                                                             (bind ((character-index (- ?character-index suffix-start-index)))
                                                                                               `((the string (suffix-of (the document/insertion document)))
                                                                                                 (the string (subseq (the string document) ,character-index ,character-index)))))))))))
                                             (operation/text/replace-range
                                              (awhen (bind ((value-start-index (length (prefix-of -printer-input-)))
                                                            (value-end-index (+ value-start-index (length (value-of -printer-input-)))))
                                                       (reference-case (selection-of operation)
                                                         (((the text/text (content-of (the syntax/leaf document)))
                                                           (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                                                          (when (and (<= value-start-index ?start-character-index value-end-index) (<= value-start-index ?end-character-index value-end-index))
                                                            `((the string (value-of (the document/insertion document)))
                                                              (the string (subseq (the string document) ,(- ?start-character-index value-start-index) ,(- ?end-character-index value-start-index))))))))
                                                (make-operation/string/replace-range it (replacement-of operation))))
                                             (operation/show-context-sensitive-help
                                              (make-instance 'operation/show-context-sensitive-help
                                                             :commands (iter (for command :in (commands-of operation))
                                                                             (awhen (recurse (operation-of command))
                                                                               (collect (make-instance 'command
                                                                                                       :gesture (gesture-of command)
                                                                                                       :domain (domain-of command)
                                                                                                       :description (description-of command)
                                                                                                       :operation it))))))
                                             (operation/compound
                                              (bind ((operations (mapcar #'recurse (elements-of operation))))
                                                (unless (some 'null operations)
                                                  (make-operation/compound operations)))))))
                                  (recurse (operation-of -input-)))
                                :domain (domain-of -input-)
                                :description (description-of -input-))
                  (make-nothing-command -gesture-)))
