;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

;; TODO: rename
(def projection document/document->text/text ()
  ())

(def projection document/nothing->text/text ()
  ())

(def projection document/insertion->text/text ()
  ((factory :type function)))

(def projection document/clipboard->t ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/document/document->text/text ()
  (make-projection 'document/document->text/text))

(def (function e) make-projection/document/nothing->text/text ()
  (make-projection 'document/nothing->text/text))

(def (function e) make-projection/document/insertion->text/text (factory)
  (make-projection 'document/insertion->text/text :factory factory))

(def (function e) make-projection/document/clipboard->t ()
  (make-projection 'document/clipboard->t))

;;;;;;
;;; Construction

(def (macro e) document/document->text/text ()
  '(make-projection/document/document->text/text))

(def (macro e) document/nothing->text/text ()
  `(make-projection/document/nothing->text/text))

(def (macro e) document/insertion->text/text (factory)
  `(make-projection/document/insertion->text/text ,factory))

(def (macro e) document/clipboard->t ()
  `(make-projection/document/clipboard->t))

;;;;;;
;;; Printer

(def printer document/document->text/text (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the document/document input))
                                                                        ,@(typed-reference (form-type input) input-reference)))))
    (make-iomap/compound projection recursion input input-reference (output-of content-iomap) (list content-iomap))))

(def printer document/nothing->text/text (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (selection-of input)
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (value-of (the document/nothing document))))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))))))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection))
                     (text/string (value-of input) :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer document/insertion->text/text (projection recursion input input-reference)
  (bind ((output-selection (pattern-case (selection-of input)
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (value-of (the document/insertion document))))
                              (bind ((character-index (+ (length (prefix-of input)) ?character-index)))
                                `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                  (the text/text (content-of (the tree/leaf document))))))
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (prefix-of (the document/insertion document))))
                              `((the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))
                                (the text/text (content-of (the tree/leaf document)))))
                             (((the string (subseq (the string document) ?character-index ?character-index))
                               (the string (suffix-of (the document/insertion document))))
                              (bind ((character-index (+ (length (prefix-of input)) (length (value-of input)) ?character-index)))
                                `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))
                                  (the text/text (content-of (the tree/leaf document))))))))
         ((:values nil completion) (funcall (factory-of projection) (value-of input)))
         (commitable? (not (null (funcall (factory-of projection) (string+ (value-of input) completion)))))
         (value-color (if commitable? *color/solarized/green* *color/solarized/red*))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection))
                     (text/string (prefix-of input) :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75))
                     (text/string (value-of input) :font *font/liberation/serif/regular/24* :font-color value-color)
                     (text/string (if completion (if commitable? (string+ completion "?")
                                                     completion)
                                      "") :font *font/liberation/serif/regular/24* :font-color (color/lighten value-color 0.75))
                     (text/string (suffix-of input) :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer document/clipboard->t (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the document/document input))
                                                                        ,@(typed-reference (form-type input) input-reference)))))
    (make-iomap/compound projection recursion input input-reference (output-of content-iomap) (list content-iomap))))

;;;;;;
;;; Reader

(def reader document/document->text/text (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (labels ((recurse (operation)
                               (typecase operation
                                 (operation/quit operation)
                                 (operation/replace-selection
                                  (make-operation/replace-selection printer-input (append (selection-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document)))))))
                                 (operation/sequence/replace-element-range
                                  (make-operation/sequence/replace-element-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/number/replace-range
                                  (make-operation/number/replace-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/replace-target
                                  (make-operation/replace-target printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/focusing/replace-part
                                  operation)
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
                      (bind ((content-command (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0))))
                        (make-command (gesture-of input)
                                      (recurse (operation-of content-command))
                                      :domain (domain-of content-command)
                                      :description (description-of content-command))))
                    (make-command/nothing (gesture-of input)))))

(def reader document/nothing->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-insert)
                       :domain "Document" :description "Starts an object insertion"
                       :operation (make-operation/compound (list (make-operation/replace-target printer-input nil (document/insertion))
                                                                 (make-operation/replace-selection printer-input `((the string (subseq (the string document) 0 0))
                                                                                                                   (the string (value-of (the document/insertion document)))))))))
                    (make-command (gesture-of input)
                                  (labels ((recurse (operation)
                                             (typecase operation
                                               (operation/quit operation)
                                               (operation/replace-selection
                                                (awhen (pattern-case (selection-of operation)
                                                         (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                                           (the text/text (content-of (the tree/leaf document))))
                                                          `((the string (subseq (the string document) ,?character-index ,?character-index))
                                                            (the string (value-of (the document/nothing document))))))
                                                  (make-operation/replace-selection printer-input it)))
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
                                    (recurse (operation-of input)))
                                  :domain (domain-of input)
                                  :description (description-of input))
                    (make-command/nothing (gesture-of input)))))

(def reader document/insertion->text/text (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-tab)
                       :domain "Document" :description "Inserts the suggested completion at the selection"
                       :operation (bind (((:values nil completion) (funcall (factory-of projection) (value-of printer-input)))
                                         (end (length (value-of printer-input))))
                                    (values (when completion
                                              (make-operation/sequence/replace-element-range printer-input `((the string (subseq (the string document) ,end ,end))
                                                                                                             (the string (value-of (the document/insertion document)))) completion))
                                            #t)))
                      ((gesture/keyboard/key-press :sdl-key-return)
                       :domain "Document" :description "Inserts a new object of the provided type"
                       :operation (bind (((:values immediate-new-instance completion) (funcall (factory-of projection) (value-of printer-input)))
                                         (new-instance (or immediate-new-instance
                                                           (funcall (factory-of projection) (string+ (value-of printer-input) completion)))))
                                    (values (when new-instance
                                              (make-operation/compound (list (make-operation/replace-target printer-input nil new-instance)
                                                                             #+nil ;; TODO: causes the initial selection to fail
                                                                             (make-operation/replace-selection printer-input nil))))
                                            #t)))
                      ((gesture/keyboard/key-press :sdl-key-escape)
                       :domain "Document" :description "Aborts the object insertion"
                       :operation (make-operation/replace-target printer-input nil (document/nothing))))
                    (make-command (gesture-of input)
                                  (labels ((recurse (operation)
                                             (typecase operation
                                               (operation/quit operation)
                                               (operation/replace-selection
                                                (make-operation/replace-selection printer-input
                                                                                  (pattern-case (selection-of operation)
                                                                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index))
                                                                                      (the text/text (content-of (the tree/leaf document))))
                                                                                     (bind ((prefix-start-index 0)
                                                                                            (prefix-end-index (+ prefix-start-index (length (prefix-of printer-input))))
                                                                                            (value-start-index prefix-end-index)
                                                                                            (value-end-index (+ value-start-index (length (value-of printer-input))))
                                                                                            (suffix-start-index value-end-index)
                                                                                            (suffix-end-index (+ suffix-start-index (length (suffix-of printer-input)))))
                                                                                       (econd ((<= value-start-index ?character-index value-end-index)
                                                                                               (bind ((character-index (- ?character-index value-start-index)))
                                                                                                 `((the string (subseq (the string document) ,character-index ,character-index))
                                                                                                   (the string (value-of (the document/insertion document))))))
                                                                                              ((<= prefix-start-index ?character-index prefix-end-index)
                                                                                               (bind ((character-index (- ?character-index prefix-start-index)))
                                                                                                 `((the string (subseq (the string document) ,character-index ,character-index))
                                                                                                   (the string (prefix-of (the document/insertion document))))))
                                                                                              ((<= suffix-start-index ?character-index suffix-end-index)
                                                                                               (bind ((character-index (- ?character-index suffix-start-index)))
                                                                                                 `((the string (subseq (the string document) ,character-index ,character-index))
                                                                                                   (the string (suffix-of (the document/insertion document))))))))))))
                                               (operation/sequence/replace-element-range
                                                (awhen (bind ((value-start-index (length (prefix-of printer-input)))
                                                              (value-end-index (+ value-start-index (length (value-of printer-input)))))
                                                         (pattern-case (target-of operation)
                                                           (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                                             (the text/text (content-of (the tree/leaf document))))
                                                            (when (and (<= value-start-index ?start-character-index value-end-index) (<= value-start-index ?end-character-index value-end-index))
                                                              `((the string (subseq (the string document) ,(- ?start-character-index value-start-index) ,(- ?end-character-index value-start-index)))
                                                                (the string (value-of (the document/insertion document))))))))
                                                  (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
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
                                    (recurse (operation-of input)))
                                  :domain (domain-of input)
                                  :description (description-of input))
                    (make-command/nothing (gesture-of input)))))

(def reader document/clipboard->t (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-c :control)
                       :domain "Document" :description "Copies the selected object to the clipboard"
                       :operation (bind ((slice (deep-copy (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input)))))))
                                    (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)))
                      ((gesture/keyboard/key-press :sdl-key-x :control)
                       :domain "Document" :description "Cuts the selected object and moves it to the clipboard"
                       :operation (bind ((slice (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input))))))
                                    (make-operation/compound (list (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)
                                                                   (make-operation/replace-target printer-input (selection-of printer-input) (document/nothing))))))
                      ((gesture/keyboard/key-press :sdl-key-n :control)
                       :domain "Document" :description "Notes selected object into the clipboard"
                       :operation (bind ((slice (eval-reference printer-input (reference/flatten (reverse (selection-of printer-input))))))
                                    (make-operation/replace-target printer-input `((the ,(form-type slice) (slice-of (the document/clipboard document)))) slice)))
                      ((gesture/keyboard/key-press :sdl-key-v :control)
                       :domain "Document" :description "Pastes the object from the clipboard to the selection"
                       :operation (when (slice-of printer-input)
                                    (make-operation/replace-target printer-input (selection-of printer-input) (slice-of printer-input))))
                      ((gesture/keyboard/key-press :sdl-key-v '(:shift :control))
                       :domain "Document" :description "Pastes a new copy of the object from the clipboard to the selection"
                       :operation (when (slice-of printer-input)
                                    (make-operation/replace-target printer-input (selection-of printer-input) (deep-copy (slice-of printer-input))))))
                    (labels ((recurse (operation)
                               (typecase operation
                                 (operation/quit operation)
                                 (operation/replace-selection
                                  (make-operation/replace-selection printer-input (append (selection-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document)))))))
                                 (operation/sequence/replace-element-range
                                  (make-operation/sequence/replace-element-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/number/replace-range
                                  (make-operation/number/replace-range printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/replace-target
                                  (make-operation/replace-target printer-input (append (target-of operation) `((the ,(form-type (content-of printer-input)) (content-of (the document/document document))))) (replacement-of operation)))
                                 (operation/focusing/replace-part
                                  operation)
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
                      (bind ((content-command (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0))))
                        (make-command (gesture-of input)
                                      (recurse (operation-of content-command))
                                      :domain (domain-of content-command)
                                      :description (description-of content-command))))
                    (make-command/nothing (gesture-of input)))))
