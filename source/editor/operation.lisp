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
  (:documentation "Redoes all side effects of OPERATION. Has side effects."))

(def (definer :available-flags "e") operation (name supers slots &rest options)
  `(def class* ,name ,supers ,slots ,@options))

;;;;;;
;;; Operation classes

(def operation operation ()
  ()
  (:documentation "Base class for operations."))

(def operation operation/compound (operation)
  ((elements :type sequence))
  (:documentation "A sequence of operations carried out in the order they appear in elements."))

(def operation operation/functional (operation)
  ((thunk :type function)))

(def operation operation/quit (operation)
  ()
  (:documentation "An operation that quits the editor."))

(def operation operation/undo (operation)
  ()
  (:documentation "An operation that undoes the effect of the last operation."))

(def operation operation/describe (operation)
  ((target :type reference)))

(def operation operation/replace-content (operation)
  ((document :type document)
   (content :type t))
  (:documentation "An operation that replaces the content of a document."))

(def operation operation/replace-selection (operation)
  ((document :type document)
   (selection :type reference))
  (:documentation "An operation that replaces the selection of a document."))

(def operation operation/replace-target (operation)
  ((document :type document)
   (target :type reference)
   (replacement :type t)))

(def operation operation/save-document (operation)
  ((document :type document)
   (filename :type pathname)))

(def operation operation/load-document (operation)
  ((document :type document)
   (filename :type pathname)))

(def operation operation/export-document (operation)
  ((document :type document)
   (filename :type pathname)))

(def operation operation/import-document (operation)
  ((document :type document)
   (filename :type pathname)))

(def operation operation/select-next-alternative (operation)
  ((alternatives :type alternatives)))

(def operation operation/show-context-sensitive-help (operation)
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

(def function make-operation/replace-content (document content)
  (make-instance 'operation/replace-content :document document :content content))

(def function make-operation/replace-selection (document selection)
  (make-instance 'operation/replace-selection :document document :selection selection))

(def function make-operation/replace-target (document target replacement)
  (make-instance 'operation/replace-target :document document :target target :replacement replacement))

(def function make-operation/save-document (document filename)
  (make-instance 'operation/save-document :document document :filename filename))

(def function make-operation/load-document (document filename)
  (make-instance 'operation/load-document :document document :filename filename))

(def function make-operation/export-document (document filename)
  (make-instance 'operation/export-document :document document :filename filename))

(def function make-operation/import-document (document filename)
  (make-instance 'operation/import-document :document document :filename filename))

(def function make-operation/select-next-alternative (alternatives)
  (make-instance 'operation/select-next-alternative :alternatives alternatives))

;; TODO: rename?
(def function document/read-operation (gesture)
  (gesture-case gesture
    ((gesture/keyboard/key-press :sdl-key-escape)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))
    ((make-instance 'gesture/window/quit :modifiers nil)
     :domain "Document" :description "Quits from the editor"
     :operation (make-operation/quit))))

;;;;;;
;;; Operation API implementation

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
  (setf (eval-reference (document-of operation) (reference/flatten (reverse (target-of operation)))) (replacement-of operation)))

(def function deserialize-document (filename)
  (bind ((extension (pathname-type filename)))
    (eswitch (extension :test 'string=)
      ("pred"
       (with-input-from-file (input filename :element-type '(unsigned-byte 8))
         (hu.dwim.serializer:deserialize input)))
      ("json"
       (json/load-document (read-file-into-string filename)))
      ("xml"
       ;; TODO:
       (make-text/text (list (make-text/string (read-file-into-string filename) :font *font/default* :font-color *color/default*))))
      ("html"
       ;; TODO:
       (make-text/text (list (make-text/string (read-file-into-string filename) :font *font/default* :font-color *color/default*))))
      ("txt"
       (make-text/text (list (make-text/string (read-file-into-string filename) :font *font/default* :font-color *color/default*)))))))

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
  (bind ((alternatives (alternatives-of operation)))
    (setf (selection-of alternatives) (mod (1+ (selection-of alternatives))
                                           (length (alternatives-of alternatives))))))

(def method run-operation ((operation operation/describe))
  (values))

(def method run-operation ((operation operation/show-context-sensitive-help))
  (values))
