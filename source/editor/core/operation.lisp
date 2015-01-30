;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Operation API
;;;
;;; An operation represents a change in domain data, selections or any other editor state.

(def generic run-operation (operation)
  (:documentation "Runs all side effects of OPERATION. Has side effects."))

(def (definer :available-flags "e") operation (name supers slots &rest options)
  (bind ((supers (if (or (eq name 'operation) (member 'operation supers))
                     supers
                     (append supers '(operation)))))
    `(progn
       (def class* ,name ,supers ,slots ,@options)
       ,@(when (getf -options- :export) `((export ',name))))))

;;;;;;
;;; Operation classes

(def operation operation ()
  ()
  (:documentation "Base class for operations."))

(def operation operation/compound ()
  ((elements :type sequence))
  (:documentation "A sequence of operations carried out in the order they appear in elements."))

(def operation operation/functional ()
  ((thunk :type function)))

(def operation operation/quit ()
  ()
  (:documentation "An operation that quits the editor."))

(def operation operation/undo ()
  ()
  (:documentation "An operation that undoes the effect of the last operation."))

(def operation operation/describe ()
  ((selection :type reference)))

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

(def operation operation/save-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/load-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/export-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/import-document ()
  ((document :type document)
   (filename :type pathname)))

(def operation operation/select-next-alternative ()
  ((projection :type alternatives)))

(def operation operation/show-context-sensitive-help ()
  ((commands :type sequence)))

;;;;;;
;;; Operation constructors

(def function make-operation/compound (elements)
  (make-instance 'operation/compound :elements elements))

(def function make-operation/functional (thunk)
  (make-instance 'operation/functional :thunk thunk))

(def function make-operation/quit ()
  (make-instance 'operation/quit))

(def function make-operation/undo ()
  (make-instance 'operation/undo))

(def function make-operation/describe (selection)
  (make-instance 'operation/describe :selection selection))

(def function make-operation/replace-content (document content)
  (make-instance 'operation/replace-content :document document :content content))

(def function make-operation/replace-selection (document selection)
  (make-instance 'operation/replace-selection :document document :selection selection))

(def function make-operation/replace-target (document selection replacement)
  (make-instance 'operation/replace-target :document document :selection selection :replacement replacement))

(def function make-operation/save-document (document filename)
  (make-instance 'operation/save-document :document document :filename filename))

(def function make-operation/load-document (document filename)
  (make-instance 'operation/load-document :document document :filename filename))

(def function make-operation/export-document (document filename)
  (make-instance 'operation/export-document :document document :filename filename))

(def function make-operation/import-document (document filename)
  (make-instance 'operation/import-document :document document :filename filename))

(def function make-operation/select-next-alternative (projection)
  (make-instance 'operation/select-next-alternative :projection projection))

;; TODO: rename?
(def function document/read-operation (gesture)
  (gesture-case gesture
    ((gesture/keyboard/key-press :sdl-key-escape)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))
    ((make-instance 'gesture/window/quit :modifiers nil)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))))

;; TODO: rename and move
(def function make-operation/replace (document reference replacement &optional (list-replacement (list replacement)))
  (pattern-case reference
    ((content-of (the document ?a))
     (make-operation/replace-content document replacement))
    ((elt (the sequence ?a) ?b)
     (make-operation/sequence/replace-range document reference list-replacement))
    (?a
     (make-operation/replace-target document reference replacement))))

;;;;;;
;;; Operation API implementation

(def function operation/extend (printer-input path operation)
  (labels ((recurse (operation)
             (typecase operation
               ;; TODO: base type for these?
               ((or operation/quit operation/functional operation/save-document operation/load-document operation/widget/scroll-pane/scroll operation/widget/tabbed-pane/select-page operation/select-next-alternative operation/focusing/replace-part) operation)
               ;; TODO: base type for these ones with 1 reference?
               (operation/replace-selection
                (make-operation/replace-selection printer-input (append (selection-of operation) path)))
               (operation/sequence/replace-range
                (make-operation/sequence/replace-range printer-input (append (selection-of operation) path) (replacement-of operation)))
               (operation/number/replace-range
                (make-operation/number/replace-range printer-input (append (selection-of operation) path) (replacement-of operation)))
               (operation/text/replace-range
                (make-operation/text/replace-range printer-input (append (selection-of operation) path) (replacement-of operation)))
               (operation/replace-target
                (make-operation/replace-target printer-input (append (selection-of operation) path) (replacement-of operation)))
               (operation/describe
                (make-operation/describe (append (selection-of operation) path)))
               (operation/show-context-sensitive-help
                (make-instance 'operation/show-context-sensitive-help
                               :commands (iter (for command :in (commands-of operation))
                                               (awhen (recurse (operation-of command))
                                                 (collect (make-command/clone command it))))))
               (operation/compound
                (bind ((operations (mapcar #'recurse (elements-of operation))))
                  (unless (some 'null operations)
                    (make-operation/compound operations)))))))
    (recurse operation)))

(def function operation/read-backward (recursion input printer-iomap backward-mapper operation-mapper)
  (bind ((printer-input (input-of printer-iomap)))
    (labels ((recurse (operation)
               (typecase operation
                 ((or operation/quit operation/functional operation/save-document operation/load-document operation/widget/scroll-pane/scroll operation/widget/tabbed-pane/select-page operation/select-next-alternative operation/focusing/replace-part) operation)
                 (operation/replace-selection
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/replace-selection (input-of child-iomap) child-selection))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-selection printer-input selection)))))
                 (operation/replace-target
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/replace-target (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/replace-target printer-input selection (replacement-of operation))))))
                 (operation/sequence/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/sequence/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/sequence/replace-range printer-input selection (replacement-of operation))))))
                 (operation/text/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/text/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/text/replace-range printer-input selection (replacement-of operation))))))
                 (operation/number/replace-range
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/number/replace-range (input-of child-iomap) child-selection (replacement-of operation)))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/number/replace-range printer-input selection (replacement-of operation))))))
                 (operation/describe
                  (bind (((:values selection child-selection child-iomap) (funcall backward-mapper printer-iomap (selection-of operation))))
                    (or (when operation-mapper (funcall operation-mapper operation selection child-selection child-iomap))
                        (if child-iomap
                            (bind ((input-child-operation (make-operation/describe child-selection))
                                   (input-child-command (make-command/clone input input-child-operation))
                                   (output-child-command (recurse-reader recursion input-child-command child-iomap))
                                   (output-element-operation (operation-of output-child-command)))
                              (operation/extend printer-input selection output-element-operation))
                            (make-operation/describe selection)))))
                 (operation/show-context-sensitive-help
                  (make-instance 'operation/show-context-sensitive-help
                                 :commands (iter (for command :in (commands-of operation))
                                                 (awhen (recurse (operation-of command))
                                                   (collect (make-command/clone command it))))))
                 (operation/compound
                  (bind ((operations (mapcar #'recurse (elements-of operation))))
                    (unless (some 'null operations)
                      (make-operation/compound operations)))))))
      (recurse (operation-of input)))))

(def function command/read-selection (recursion input printer-iomap forward-mapper backward-mapper)
  (bind ((printer-input (input-of printer-iomap))
         ((:values selection child-selection child-iomap) (funcall forward-mapper printer-iomap (selection-of printer-input))))
    (when child-selection
      (bind ((child-input-command (make-command/clone input))
             (child-output-command (recurse-reader recursion child-input-command child-iomap))
             (child-output-operation (operation-of child-output-command))
             (extended-operation (operation/extend printer-input (funcall backward-mapper printer-iomap selection) child-output-operation)))
        (when extended-operation
          (make-command/clone input extended-operation))))))

(def function command/read-backward (recursion input printer-iomap backward-mapper operation-mapper)
  (awhen (operation/read-backward recursion input printer-iomap backward-mapper operation-mapper)
    (make-command/clone input it)))

(def method run-operation ((operation operation/compound))
  (iter (for element :in-sequence (elements-of operation))
        (run-operation element)))

(def method run-operation ((operation operation/functional))
  (funcall (thunk-of operation)))

(def method run-operation ((operation operation/quit))
  (throw :quit-editor nil))

(def method run-operation ((operation operation/undo))
  (not-yet-implemented))

(def method run-operation ((operation operation/replace-content))
  (setf (content-of (document-of operation)) (content-of operation)))

(def function remove-selection (document selection)
  (labels ((recurse (document selection)
               (when (typep document 'document)
                 (setf (selection-of document) nil))
               (when (rest selection)
                 (recurse (eval-reference document (first selection)) (rest selection)))))
      ;; KLUDGE: this clears the old selection but the document may have changed meanwhile causing errors
      ;; KLUDGE: this kind of problem would be solved by putting only the relevant parts of the selection to the documents
      (ignore-errors
        (recurse document (reverse selection)))))

(def function set-selection (document selection)
  (labels ((recurse (document selection)
             (when (typep document 'document)
               (setf (selection-of document) (reverse selection)))
             (when (rest selection)
               (recurse (eval-reference document (first selection)) (rest selection)))))
    (recurse document (reverse selection))))

(def method run-operation ((operation operation/replace-selection))
  (bind ((document (document-of operation))
         (old-selection (selection-of document))
         (new-selection (selection-of operation)))
    (remove-selection document old-selection)
    (set-selection document new-selection)
    (editor.debug "Selection of ~A is set to ~A" document new-selection)))

(def method run-operation ((operation operation/replace-target))
  (setf (eval-reference (document-of operation) (reference/flatten (reverse (selection-of operation)))) (replacement-of operation)))

(def function deserialize-document (filename)
  (bind ((extension (pathname-type filename)))
    (eswitch (extension :test 'string=)
      ("pred"
       (with-input-from-file (input filename :element-type '(unsigned-byte 8))
         (hu.dwim.serializer:deserialize input)))
      ("json"
       (with-input-from-file (input filename :element-type 'character)
         (json/load-document input)))
      ("xml"
       (with-input-from-file (input filename :element-type 'character)
         (xml/load-document input)))
      ("html"
       (with-input-from-file (input filename :element-type 'character)
         (xml/load-document input)))
      ("txt"
       (text/make-text (list (text/make-string (read-file-into-string filename) :font *font/default* :font-color *color/default*)))))))

(def function serialize-document (filename document)
  (bind ((extension (pathname-type filename)))
    (eswitch (extension :test 'string=)
      ("pred"
       (with-output-to-file (output filename :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
         (hu.dwim.serializer:serialize document :output output)))
      ("txt"
       (not-yet-implemented)))))

(def method run-operation ((operation operation/save-document))
  (serialize-document (filename-of operation) (document-of operation)))

(def method run-operation ((operation operation/load-document))
  (deserialize-document (filename-of operation)))

(def method run-operation ((operation operation/export-document))
  (with-output-to-file (output (filename-of operation) :if-does-not-exist :create :if-exists :overwrite :element-type 'character)
    (print-document (content-of (document-of operation)) output)))

(def method run-operation ((operation operation/import-document))
  (with-input-from-file (input (filename-of operation) :element-type 'character)
    (not-yet-implemented)))

(def method run-operation ((operation operation/select-next-alternative))
  (bind ((projection (projection-of operation)))
    (setf (selection-of projection) (mod (1+ (selection-of projection))
                                         (length (alternatives-of projection))))))

(def method run-operation ((operation operation/describe))
  (values))

(def method run-operation ((operation operation/show-context-sensitive-help))
  (values))
