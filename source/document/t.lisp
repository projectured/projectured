;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Operation

(def operation operation/describe ()
  ((selection :type reference)))

(def operation operation/show-annotation ()
  ((document :type document)
   (selection :type reference)))

(def operation operation/replace-content ()
  ((document :type document)
   (content :type t))
  (:documentation "An operation that replaces the content of a document."))

(def operation operation/replace-selection ()
  ((document :type document)
   (selection :type reference))
  (:documentation "An operation that replaces the selection of a document."))

(def operation operation/replace-target ()
  ((document :type document)
   (selection :type reference)
   (replacement :type t)))

(def operation operation/show-context-sensitive-help ()
  ((commands :type sequence)))

;;;;;;
;;; Construction

(def function make-operation/describe (selection)
  (make-instance 'operation/describe :selection selection))

(def function make-operation/replace-content (document content)
  (make-instance 'operation/replace-content :document document :content content))

(def function make-operation/replace-selection (document selection)
  (make-instance 'operation/replace-selection :document document :selection selection))

(def function make-operation/replace-target (document selection replacement)
  (make-instance 'operation/replace-target :document document :selection selection :replacement replacement))


;;;;;;
;;; Evaluator

(def evaluator operation/replace-content (operation)
  (setf (content-of (document-of operation)) (content-of operation)))

(def evaluator operation/replace-selection (operation)
  (bind ((document (document-of operation))
         (old-selection (when (typep document 'document) (selection-of document)))
         (new-selection (selection-of operation)))
    (remove-selection document old-selection)
    (set-selection document new-selection)
    (editor.debug "Selection of ~A is set to ~A" document new-selection)))

(def evaluator operation/replace-target (operation)
  (setf (eval-reference (document-of operation) (flatten-reference (selection-of operation))) (replacement-of operation)))

(def evaluator operation/describe (operation)
  (values))

(def evaluator operation/show-annotation (operation)
  (values))

(def evaluator operation/show-context-sensitive-help (operation)
  (values))

;;;;;;
;;; API

(def maker pred ()
  (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                    (the string (subseq (the string document) 0 0))))))

(def loader pred (filename)
  (with-input-from-file (input filename :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:deserialize input)))

(def saver pred (filename document)
  (with-output-to-file (output filename :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:serialize document :output output)))

(def function document/read-operation (gesture)
  (gesture-case gesture
    ((make-instance 'gesture/window/quit :modifiers nil)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))
    ((make-key-press-gesture :scancode-escape)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))
    ((make-key-press-gesture :scancode-m :control)
     :domain "Document" :description "Prints memory allocation information"
     :operation (make-operation/functional (lambda () (room))))
    ((make-key-press-gesture :scancode-g :control)
     :domain "Document" :description "Execute garbage collector"
     :operation (make-operation/functional (lambda () (trivial-garbage:gc :full #t))))))

;; TODO: simplify and generalize
(def function operation/extend (printer-input path operation)
  (labels ((recurse (operation)
             (typecase operation
               ;; TODO: base type for these?
               ((or operation/quit operation/functional operation/save-document operation/load-document operation/widget/scroll-pane/scroll operation/widget/tabbed-pane/select-page operation/select-next-alternative operation/focusing/replace-part) operation)
               ;; TODO: base type for these ones with 1 reference?
               (operation/replace-selection
                (make-operation/replace-selection printer-input (append path (selection-of operation))))
               (operation/number/replace-range
                (make-operation/number/replace-range printer-input (append path (selection-of operation)) (replacement-of operation)))
               (operation/string/replace-range
                (make-operation/string/replace-range printer-input (append path (selection-of operation)) (replacement-of operation)))
               (operation/sequence/replace-range
                (make-operation/sequence/replace-range printer-input (append path (selection-of operation)) (replacement-of operation)))
               (operation/sequence/swap-ranges
                (make-operation/sequence/swap-ranges printer-input (append path (selection-1-of operation)) (append path (selection-2-of operation))))
               (operation/text/replace-range
                (make-operation/text/replace-range printer-input (append path (selection-of operation)) (replacement-of operation)))
               (operation/replace-target
                (make-operation/replace-target printer-input (append path (selection-of operation)) (replacement-of operation)))
               (operation/tree/toggle-collapsed
                (make-operation/tree/toggle-collapsed printer-input (append path (selection-of operation))))
               (operation/describe
                (make-operation/describe (append path (selection-of operation))))
               (operation/show-annotation
                (make-instance 'operation/show-annotation :document printer-input :selection (append path (selection-of operation))))
               (operation/show-context-sensitive-help
                (make-instance 'operation/show-context-sensitive-help
                               :commands (iter (for command :in (commands-of operation))
                                               (awhen (command/extend command printer-input path)
                                                 (collect it)))))
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations)))))))
    (recurse operation)))

;; TODO: simplify and generalize
(def function operation/read-backward (recursion input printer-iomap backward-mapper operation-mapper)
  (bind ((printer-input (input-of printer-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 ((or operation/quit operation/functional operation/save-document operation/load-document operation/widget/scroll-pane/scroll operation/widget/tabbed-pane/select-page operation/select-next-alternative operation/focusing/replace-part) operation)
                 (operation/replace-selection
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/replace-selection (input-of child-iomap) child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-selection printer-input selection)))))
                 (operation/replace-target
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/replace-target (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-target printer-input selection (replacement-of operation))))))
                 (operation/text/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/text/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/text/replace-range printer-input selection (replacement-of operation))))))
                 (operation/sequence/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/sequence/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/sequence/replace-range printer-input selection (replacement-of operation))))))
                 (operation/sequence/swap-ranges
                  (bind (((:values selection-1 child-selection-1 child-iomap) (funcall backward-mapper printer-iomap (selection-1-of operation)))
                         ((:values selection-2 child-selection-2 child-iomap) (funcall backward-mapper printer-iomap (selection-2-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection-1 child-selection-1 child-iomap))
                        (if (and child-iomap child-selection-1 child-selection-2)
                            (bind ((input-child-operation (make-operation/sequence/swap-ranges (input-of child-iomap) child-selection-1 child-selection-2))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection-1 output-element-operation))
                            (make-operation/sequence/swap-ranges printer-input selection-1 selection-2)))))
                 (operation/string/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/string/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/string/replace-range printer-input selection (replacement-of operation))))))
                 (operation/number/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/number/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/number/replace-range printer-input selection (replacement-of operation))))))
                 (operation/tree/toggle-collapsed
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/tree/toggle-collapsed (input-of child-iomap) child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/tree/toggle-collapsed printer-input selection)))))
                 (operation/describe
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/describe child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/describe selection)))))
                 (operation/show-annotation
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-instance 'operation/show-annotation :document (output-of child-iomap) :selection child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-instance 'operation/show-annotation :document printer-input :selection selection)))))
                 (operation/show-context-sensitive-help
                  (make-instance 'operation/show-context-sensitive-help
                                 :commands (iter (for command :in (commands-of operation))
                                                 (awhen (command/read-backward recursion command printer-iomap backward-mapper operation-mapper)
                                                   (collect it)))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse (operation-of input)))))

(def function command/extend (input printer-input path)
  (awhen (operation/extend printer-input path (operation-of input))
    (clone-command input it)))

(def function command/read-selection (recursion input printer-iomap forward-mapper backward-mapper)
  (bind ((printer-input (input-of printer-iomap))
         ((:values selection child-selection child-iomap) (funcall forward-mapper printer-iomap (when (typep printer-input 'document) (selection-of printer-input)))))
    (when child-selection
      (bind ((child-input-command (clone-command input))
             (child-output-command (recurse-reader recursion child-input-command child-iomap))
             (child-output-operation (operation-of child-output-command))
             (extended-operation (operation/extend printer-input (funcall backward-mapper printer-iomap selection) child-output-operation)))
        (when extended-operation
          (clone-command child-output-command extended-operation))))))

(def function command/read-backward (recursion input printer-iomap backward-mapper operation-mapper)
  (awhen (operation/read-backward recursion input printer-iomap backward-mapper operation-mapper)
    (clone-command input it)))

(def function print-selection (iomap input-selection forward-mapper)
  (bind (((:values selection nil child-iomap) (funcall forward-mapper iomap input-selection)))
    (if child-iomap
        (append selection (selection-of (output-of child-iomap)))
        selection)))

(def function remove-selection (document selection)
  (labels ((recurse (document selection)
             (when (rest selection)
               (recurse (eval-reference document (first selection)) (rest selection)))
             (when (typep document 'document)
               (setf (selection-of document) nil))))
    ;; KLUDGE: this clears the old selection but the document may have changed meanwhile causing errors
    ;; KLUDGE: this kind of problem would be solved by putting only the relevant parts of the selection to the documents
    (ignore-errors
      (recurse document selection))))

(def function set-selection (document selection)
  (labels ((recurse (document selection)
             (when (rest selection)
               (recurse (eval-reference document (first selection)) (rest selection)))
             (when (typep document 'document)
               (setf (selection-of document) selection))))
    (recurse document selection)))
