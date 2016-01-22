;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/nothing->tree/leaf ()
  ())

(def projection document/insertion->tree/leaf ()
  ((factory :type function)))

(def projection document/search->tree/node ()
  ((searcher :type function)))

(def projection document/reflection->graphics/canvas ()
  ((display-selection :type boolean)
   (display-last-commands :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/document/nothing->tree/leaf ()
  (make-projection 'document/nothing->tree/leaf))

(def function make-projection/document/insertion->tree/leaf (factory)
  (make-projection 'document/insertion->tree/leaf :factory factory))

(def function make-projection/document/search->tree/node (searcher)
  (make-projection 'document/search->tree/node :searcher searcher))

(def function make-projection/document/reflection->graphics/canvas ()
  (make-projection 'document/reflection->graphics/canvas :display-selection #f :display-last-commands #f))

;;;;;;
;;; Construction

(def macro document/nothing->tree/leaf ()
  `(make-projection/document/nothing->tree/leaf))

(def macro document/insertion->tree/leaf (factory)
  `(make-projection/document/insertion->tree/leaf ,factory))

(def macro document/search->tree/node (searcher)
  `(make-projection/document/search->tree/node ,searcher))

(def macro document/reflection->graphics/canvas ()
  `(make-projection/document/reflection->graphics/canvas))

;;;;;;
;;; IO map

(def iomap iomap/document/search->tree/node ()
  ((result :type sequence)))

;;;;;;
;;; Forward mapper

;;;;;;
;;; Backward mapper

(def function backward-mapper/document/search->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (search-of (the document/search document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?index))
        (the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) 0))
        . ?rest)
       (values `((the ,(document-type (content-of printer-input)) (content-of (the document/search document))))
               ?rest
               ;; TODO:
               nil))
      (?a
       (append `((the tree/node (printer-output (the document/search document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer document/nothing->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (pattern-case (selection-of input)
                                 (((the string (value-of (the document/nothing document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  `((the text/text (content-of (the tree/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index)))))))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (value-of input) :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/gray* 0.75)))))))
    (make-iomap projection recursion input input-reference output)))

(def printer document/insertion->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (pattern-case (selection-of input)
                                 (((the string (value-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  (bind ((character-index (+ (length (prefix-of input)) ?character-index)))
                                    `((the text/text (content-of (the tree/leaf document)))
                                      (the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the string (prefix-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  `((the text/text (content-of (the tree/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ,?character-index ,?character-index))))
                                 (((the string (suffix-of (the document/insertion document)))
                                   (the string (subseq (the string document) ?character-index ?character-index)))
                                  (bind ((character-index (+ (length (prefix-of input)) (length (value-of input)) ?character-index)))
                                    `((the text/text (content-of (the tree/leaf document)))
                                      (the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
         (output (tree/leaf (:selection output-selection)
                   (as (bind (((:values nil completion) (funcall (factory-of projection) (value-of input)))
                              (commitable? (not (null (funcall (factory-of projection) (string+ (value-of input) completion)))))
                              (value-color (if commitable? *color/solarized/green* *color/solarized/red*))
                              (font (or (font-of input) *font/liberation/serif/regular/24*)))
                         (text/text (:selection (as (nthcdr 1 (va output-selection))))
                           (text/string (prefix-of input) :font font :font-color (color/lighten *color/solarized/gray* 0.75))
                           (text/string (value-of input) :font font :font-color value-color)
                           (text/string (if completion (if commitable? (string+ completion "?") completion) "") :font font :font-color (color/lighten value-color 0.75))
                           (text/string (suffix-of input) :font font :font-color (color/lighten *color/solarized/gray* 0.75))))))))
    (make-iomap projection recursion input input-reference output)))

(def printer document/search->tree/node (projection recursion input input-reference)
  (bind ((content (content-of input))
         (search (search-of input))
         (empty-search? (string= search ""))
         (result (unless empty-search?
                   (funcall (searcher-of projection) search content)))
         ((:values output-selection selection-index)
          (pattern-case (selection-of input)
            (((the string (search-of (the document/search document)))
              (the string (subseq (the string document) ?start-index ?end-index)))
             `((the sequence (children-of (the tree/node document)))
               (the tree/leaf (elt (the sequence document) 0))
               (the text/text (content-of (the tree/leaf document)))
               (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
            (((the ?type (content-of (the document/search document)))
              . ?rest)
             (iter (for index :from 0)
                   (for reference :in result)
                   (when (and (<= (length reference) (length ?rest))
                              (equal reference (subseq ?rest 0 (length reference))))
                     (return (values (append `((the sequence (children-of (the tree/node document)))
                                               (the ?type (elt (the sequence document) ,index))
                                               (the sequence (children-of (the tree/node document)))
                                               (the tree/node (elt (the sequence document) 0)))
                                             (subseq ?rest (length reference)))
                                     index)))))
            (((the tree/node (printer-output (the document/search document) ?projection ?recursion)) . ?rest)
             (when (eq projection ?projection)
               ?rest))))
         (output (make-tree/node (iter (for index :from 0)
                                       (for reference :in result)
                                       (collect (tree/node (:indentation 0 :selection (nthcdr 2 output-selection))
                                                  (bind ((content (eval-reference content (flatten-reference reference))))
                                                    (when (typep content 'document)
                                                      ;; KLUDGE:
                                                      (setf (selection-of content) (when (eql index selection-index)
                                                                                     (nthcdr 4 output-selection))))
                                                    content))))
                                 :selection (when selection-index output-selection))))
    (make-instance 'iomap/document/search->tree/node
                   :projection projection :recursion recursion
                   :input input :input-reference input-reference :output output
                   :result result)))

(def printer document/reflection->graphics/canvas (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input)
                                             `((content-of (the document/reflection document))
                                               ,@(typed-reference (document-type input) input-reference)))))
         (selection-iomap (as (recurse-printer recursion (document/reference () (selection-of (content-of input)))
                                               `((selection-of (the document/reflection document))
                                                 ,@(typed-reference (document-type input) input-reference)))))
         (last-commands-iomap (as (awhen (last-commands-of input)
                                    (recurse-printer recursion (make-help/context-sensitive (last-commands-of input))
                                                     `((last-commands-of (the document/reflection document))
                                                       ,@(typed-reference (document-type input) input-reference))))))
         (output (as (make-graphics/canvas (append (list (output-of (va content-iomap)))
                                                   (when (display-selection-p projection)
                                                     (list (make-graphics/canvas (as (when (selection-of (content-of input))
                                                                                       (bind ((height (2d-y (size-of (bounds-of (output-of (va selection-iomap)))))))
                                                                                         (list (make-graphics/rounded-rectangle (make-2d 0 (- 710 height)) (make-2d 1280 (+ 10 height))
                                                                                                                                9
                                                                                                                                :fill-color (color/lighten *color/solarized/yellow* 0.75))
                                                                                               (make-graphics/canvas (list (output-of (va selection-iomap))) (make-2d 5 (- 715 height)))))))
                                                                                 0)))
                                                   (when (display-last-commands-p projection)
                                                     (list (make-graphics/canvas (as (when (va last-commands-iomap)
                                                                                       (list (make-graphics/rounded-rectangle (make-2d 0 631) (make-2d 1280 88)
                                                                                                                              9
                                                                                                                              :fill-color (color/lighten *color/solarized/yellow* 0.75))
                                                                                             (make-graphics/canvas (list (output-of (va last-commands-iomap))) (make-2d 5 640)))))
                                                                                 0)))) 0))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader document/nothing->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Document" :description "Starts a generic insertion into the document"
                       :operation (make-operation/compound (list (make-operation/replace-target printer-input nil (document/insertion ()))
                                                                 (make-operation/replace-selection printer-input `((the string (value-of (the document/insertion document)))
                                                                                                                   (the string (subseq (the string document) 0 0))))))))
                    (make-command (gesture-of input)
                                  (labels ((recurse (operation)
                                             (typecase operation
                                               (operation/quit operation)
                                               (operation/functional operation)
                                               (operation/replace-selection
                                                (awhen (pattern-case (selection-of operation)
                                                         (((the text/text (content-of (the tree/leaf document)))
                                                           (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                          `((the string (value-of (the document/nothing document)))
                                                            (the string (subseq (the string document) ,?character-index ,?character-index)))))
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
                    (make-nothing-command (gesture-of input)))))

(def reader document/insertion->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-tab)
                       :domain "Document" :description "Inserts the suggested completion at the selection"
                       :operation (bind (((:values nil completion) (funcall (factory-of projection) (value-of printer-input)))
                                         (end (length (value-of printer-input))))
                                    (values (when completion
                                              (make-operation/string/replace-range printer-input `((the string (value-of (the document/insertion document)))
                                                                                                   (the string (subseq (the string document) ,end ,end))) completion))
                                            #t)))
                      ((make-key-press-gesture :scancode-return)
                       :domain "Document" :description "Inserts a new object of the provided type"
                       :operation (bind (((:values immediate-new-instance completion) (funcall (factory-of projection) (value-of printer-input)))
                                         (new-instance (or immediate-new-instance
                                                           (funcall (factory-of projection) (string+ (value-of printer-input) completion)))))
                                    (values (when new-instance
                                              (make-operation/compound (list (make-operation/replace-target printer-input nil new-instance)
                                                                             #+nil ;; TODO: causes the initial selection to fail
                                                                             (make-operation/replace-selection printer-input nil))))
                                            #t)))
                      ((make-key-press-gesture :scancode-escape)
                       :domain "Document" :description "Aborts the object insertion"
                       :operation (make-operation/replace-target printer-input nil (document/nothing ()))))
                    (make-command (gesture-of input)
                                  (labels ((recurse (operation)
                                             (typecase operation
                                               (operation/quit operation)
                                               (operation/functional operation)
                                               (operation/replace-selection
                                                (make-operation/replace-selection printer-input
                                                                                  (pattern-case (selection-of operation)
                                                                                    (((the text/text (content-of (the tree/leaf document)))
                                                                                      (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                                                     (bind ((prefix-start-index 0)
                                                                                            (prefix-end-index (+ prefix-start-index (length (prefix-of printer-input))))
                                                                                            (value-start-index prefix-end-index)
                                                                                            (value-end-index (+ value-start-index (length (value-of printer-input))))
                                                                                            (suffix-start-index value-end-index)
                                                                                            (suffix-end-index (+ suffix-start-index (length (suffix-of printer-input)))))
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
                                                (awhen (bind ((value-start-index (length (prefix-of printer-input)))
                                                              (value-end-index (+ value-start-index (length (value-of printer-input)))))
                                                         (pattern-case (selection-of operation)
                                                           (((the text/text (content-of (the tree/leaf document)))
                                                             (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                                                            (when (and (<= value-start-index ?start-character-index value-end-index) (<= value-start-index ?end-character-index value-end-index))
                                                              `((the string (value-of (the document/insertion document)))
                                                                (the string (subseq (the string document) ,(- ?start-character-index value-start-index) ,(- ?end-character-index value-start-index))))))))
                                                  (make-operation/string/replace-range printer-input it (replacement-of operation))))
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
                    (make-nothing-command (gesture-of input)))))

(def reader document/search->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/leaf (elt (the sequence document) 0))
                                                    (the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   `((the string (search-of (the document/search document)))
                                                     (the string (subseq (the string document) ,?start-index ,?end-index))))
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/node (elt (the sequence document) ?index))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 0))
                                                    . ?rest)
                                                   (bind ((index ?index)
                                                          (content (content-of printer-input))
                                                          (reference (elt (result-of printer-iomap) index)))
                                                     (append `((the ,(document-type content) (content-of (the document/search document)))) reference ?rest)))
                                                  (?a
                                                   (append `((the tree/node (printer-output (the document/search document) ,projection ,recursion))) (selection-of operation))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/sequence/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/node (elt (the sequence document) ?index))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 0))
                                                    . ?rest)
                                                   (bind ((index ?index)
                                                          (content (content-of printer-input))
                                                          (reference (elt (result-of printer-iomap) index)))
                                                     (append `((the ,(document-type content) (content-of (the document/search document)))) reference ?rest))))
                                           (make-operation/sequence/replace-range printer-input it (replacement-of operation))))
                                        (operation/text/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/leaf (elt (the sequence document) 0))
                                                    (the text/text (content-of (the tree/leaf document)))
                                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                   (if (zerop (length (search-of  printer-input)))
                                                       `((the string (search-of (the document/search document)))
                                                         (the string (subseq (the string document) 0 0)))
                                                       `((the string (search-of (the document/search document)))
                                                         (the string (subseq (the string document) ,?start-index ,?end-index)))))
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/node (elt (the sequence document) ?index))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 0))
                                                    . ?rest)
                                                   (bind ((index ?index)
                                                          (content (content-of printer-input))
                                                          (reference (elt (result-of printer-iomap) index)))
                                                     (append `((the ,(document-type content) (content-of (the document/search document)))) reference ?rest))))
                                           (make-operation/sequence/replace-range printer-input it (replacement-of operation))))
                                        (operation/number/replace-range
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the tree/node (elt (the sequence document) ?index))
                                                    (the sequence (children-of (the tree/node document)))
                                                    (the ?type (elt (the sequence document) 0))
                                                    . ?rest)
                                                   (bind ((index ?index)
                                                          (content (content-of printer-input))
                                                          (reference (elt (result-of printer-iomap) index)))
                                                     (append `((the ,(document-type content) (content-of (the document/search document)))) reference ?rest))))
                                           (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (make-nothing-command (gesture-of input)))))

(def reader document/reflection->graphics/canvas (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         (last-command (command/extend (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0))
                                       printer-input
                                       `((the ,(document-type (content-of printer-input)) (content-of (the document/reflection document))))))
         (last-commands (last-commands-of printer-input))
         (new-last-commands (subseq (list* last-command last-commands) 0 (min 3 (1+ (length last-commands))))))
    (merge-commands (if (or (not last-command)
                            (not (operation-of last-command))
                            (typep (operation-of last-command) 'operation/show-context-sensitive-help))
                        last-command
                        (make-command (gesture-of input)
                                      (make-operation/compound (optional-list (operation-of last-command)
                                                                              (make-operation/replace-target (input-of printer-iomap)
                                                                                                             '((the sequence (last-commands-of (the document/reflection document))))
                                                                                                             new-last-commands)))
                                      :domain (domain-of last-command)
                                      :description (description-of last-command)))
                    (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-f1)
                       :domain "Document" :description "Toggles displaying current selection"
                       :operation (make-operation/functional (lambda () (notf (display-selection-p projection)))))
                      ((make-key-press-gesture :scancode-f2)
                       :domain "Document" :description "Toggles displaying last commands"
                       :operation (make-operation/functional (lambda () (notf (display-last-commands-p projection))))))
                    (make-nothing-command (gesture-of input)))))
