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
  ((selection :type reference))
  (:documentation "An operation that replaces the selection of a document."))

(def operation operation/replace-target ()
  ((selection :type reference)
   (replacement :type t)))

(def operation operation/show-context-sensitive-help ()
  ((commands :type sequence)))

;;;;;;
;;; Construction

(def function make-operation/describe (selection)
  (make-instance 'operation/describe :selection selection))

(def function make-operation/replace-content (document content)
  (make-instance 'operation/replace-content :document document :content content))

(def function make-operation/replace-selection (selection)
  (make-instance 'operation/replace-selection :selection selection))

(def function make-operation/replace-target (selection replacement)
  (make-instance 'operation/replace-target :selection selection :replacement replacement))


;;;;;;
;;; Evaluator

(def evaluator operation/replace-content ()
  (setf (content-of (document-of -operation-)) (content-of -operation-)))

(def evaluator operation/replace-selection ()
  (bind ((new-selection (force-cc (selection-of -operation-))))
    (set-selection -document- new-selection)
    (editor.debug "Selection of ~A is set to ~A" -document- new-selection)))

(def evaluator operation/replace-target ()
  (setf (eval-reference -document- (flatten-reference (selection-of -operation-))) (replacement-of -operation-))
  ;; KLUDGE: horrible way to keep the selection up to date?!!
  (when (typep (replacement-of -operation-) 'document)
    (set-selection -document- (append (butlast (selection-of -operation-))
                                      `((the ,(document-type (replacement-of -operation-)) ,(third (first (last (selection-of -operation-))))))
                                      (selection-of (replacement-of -operation-))))))

(def evaluator operation/describe ()
  (values))

(def evaluator operation/show-annotation ()
  (values))

(def evaluator operation/show-context-sensitive-help ()
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
               ;; TODO: base type for these ones with 1 reference?
               (operation/replace-selection
                (make-operation/replace-selection (append-cc path (selection-of operation))))
               (operation/number/replace-range
                (make-operation/number/replace-range (append-cc path (selection-of operation)) (replacement-of operation)))
               (operation/string/replace-range
                (make-operation/string/replace-range (append-cc path (selection-of operation)) (replacement-of operation)))
               (operation/sequence/replace-range
                (make-operation/sequence/replace-range (append-cc path (selection-of operation)) (replacement-of operation)))
               (operation/sequence/swap-ranges
                (make-operation/sequence/swap-ranges (append-cc path (selection-1-of operation)) (append-cc path (selection-2-of operation))))
               (operation/text/replace-range
                (make-operation/text/replace-range (append-cc path (selection-of operation)) (replacement-of operation)))
               (operation/replace-target
                (make-operation/replace-target (append-cc path (selection-of operation)) (replacement-of operation)))
               (operation/syntax/toggle-collapsed
                (make-operation/syntax/toggle-collapsed (append-cc path (selection-of operation))))
               (operation/describe
                (make-operation/describe (append-cc path (selection-of operation))))
               (operation/show-annotation
                (make-instance 'operation/show-annotation :document printer-input :selection (append-cc path (selection-of operation))))
               (operation/show-context-sensitive-help
                (make-instance 'operation/show-context-sensitive-help
                               :commands (iter (for command :in (commands-of operation))
                                               (collect (or (command/extend command printer-input path)
                                                            (make-command (gesture-of command) nil :accessible #f :domain (domain-of command) :description (description-of command)))))))
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations))))
               (t operation))))
    (recurse operation)))

;; TODO: simplify and generalize
(def function operation/read-backward (recursion input printer-iomap operation-mapper)
  (bind ((printer-input (input-of printer-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 (operation/replace-selection
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/replace-selection child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-selection selection)))))
                 (operation/replace-target
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/replace-target child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-target selection (replacement-of operation))))))
                 (operation/text/replace-range
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/text/replace-range child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/text/replace-range selection (replacement-of operation))))))
                 (operation/sequence/replace-range
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/sequence/replace-range child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/sequence/replace-range selection (replacement-of operation))))))
                 (operation/sequence/swap-ranges
                  (bind (((:values selection-1 child-selection-1 child-iomap) (call-backward-mapper printer-iomap (selection-1-of operation)))
                         ((:values selection-2 child-selection-2 child-iomap) (call-backward-mapper printer-iomap (selection-2-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection-1 child-selection-1 child-iomap))
                        (if (and child-iomap child-selection-1 child-selection-2)
                            (bind ((input-child-operation (make-operation/sequence/swap-ranges child-selection-1 child-selection-2))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection-1 output-element-operation))
                            (make-operation/sequence/swap-ranges selection-1 selection-2)))))
                 (operation/string/replace-range
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/string/replace-range child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/string/replace-range selection (replacement-of operation))))))
                 (operation/number/replace-range
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/number/replace-range child-selection (replacement-of operation)))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/number/replace-range selection (replacement-of operation))))))
                 (operation/syntax/toggle-collapsed
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/syntax/toggle-collapsed child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/syntax/toggle-collapsed selection)))))
                 (operation/describe
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if (and child-iomap child-selection)
                            (bind ((input-child-operation (make-operation/describe child-selection))
                                   (input-child-command (clone-command input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/describe selection)))))
                 (operation/show-annotation
                  (bind (((:values selection child-selection child-iomap) (call-backward-mapper printer-iomap (selection-of operation))))
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
                                                 (collect (aif (command/read-backward recursion command printer-iomap operation-mapper)
                                                               it
                                                               (make-command (gesture-of command) nil :accessible #f :domain (domain-of command) :description (description-of command)))))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations))))
                 (t operation))))
      (recurse (operation-of input)))))

(def function command/extend (input printer-input path)
  (awhen (operation/extend printer-input path (operation-of input))
    (clone-command input it)))

(def function command/read-selection (recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap))
         ((:values selection child-selection child-iomap) (call-forward-mapper printer-iomap (when (typep printer-input 'document) (get-selection printer-input)))))
    (when child-iomap
      (bind ((child-input-command (clone-command input))
             (child-output-command (recurse-reader recursion child-input-command child-iomap))
             (child-output-operation (operation-of child-output-command))
             (extended-operation (operation/extend printer-input
                                                   (take-cc (get-selection printer-input) (- (length-cc (get-selection printer-input)) (length-cc child-selection)))
                                                   ;; TODO: this didn't work with syntax->text projections because forward-mapper returns nil as first value
                                                   #+nil(call-backward-mapper printer-iomap selection)
                                                   child-output-operation)))
        (when extended-operation
          (clone-command child-output-command extended-operation))))))

(def function command/read-backward (recursion input printer-iomap &optional operation-mapper)
  (awhen (operation/read-backward recursion input printer-iomap operation-mapper)
    (clone-command input it)))

(def function print-selection (iomap)
  (labels ((recurse (iomap selection)
             (bind (((:values selection child-selection child-iomap transformer) (call-forward-mapper iomap selection)))
               (if child-iomap
                   (append-cc (cc selection) (as (funcall transformer (recurse child-iomap child-selection))))
                   selection))))
    (recurse iomap (get-selection (input-of iomap)))))

(def function chomp-selection (selection part &optional (length 1))
  (bind ((part-length (length part)))
    (when (and (> (length (take-cc selection (1+ part-length))) part-length)
               (equal part (take-cc selection part-length)))
      (subseq (force-cc selection (+ part-length length)) part-length (+ part-length length)))))

(def function printer-output-selection (selection)
  (pattern-case selection
    (((the ?output-type (printer-output (the ?input-type document) ?projection ?recursion))
      . ?rest)
     selection)))

(def function set-selection (document selection)
  (labels ((recurse (document document-part selection selection-part path)
             (bind ((selection-part (append selection-part (list (first selection))))
                    (document-part (eval-reference document-part (first selection))))
               ;;(format t "~%~A ~A ~A ~A" document document-part selection selection-part)
               (cond ((not (rest selection))
                      (unless (equal (selection-of document) selection-part)
                        (setf (selection-of document) selection-part)))
                     ((pattern-case selection
                        (((the ?type (printer-output . ?arguments))
                          . ?rest)
                         #t))
                      (setf (selection-of document) selection))
                     ((typep document-part 'document)
                      (if (member document-part path)
                          (recurse document document-part (rest selection) selection-part (cons document-part path))
                          (progn
                            (unless (equal (selection-of document) selection-part)
                              (setf (selection-of document) selection-part))
                            (recurse document-part document-part (rest selection) nil (cons document-part path)))))
                     (t
                      (recurse document document-part (rest selection) selection-part (cons document-part path)))))))
    (recurse document document selection nil nil)))

(def function get-selection (document)
  (labels ((recurse (document)
             (when (typep document 'document)
               (bind ((selection (selection-of document)))
                 (pattern-case selection
                   (((the ?type (printer-output . ?arguments))
                     . ?rest)
                    selection)
                   (? (append-cc selection
                                 ;; TODO: make this lazy
                                 (as (bind ((document-part (eval-reference document (flatten-reference selection))))
                                       (unless (eq document document-part)
                                         (when (typep document-part 'document)
                                           (recurse document-part))))))))))))
    (recurse document)))
