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

(def (generic e) operation? (object)
  (:documentation "Returns TRUE if OBJECT is an operation, otherwise returns FALSE. Purely functional."))

(def (generic e) redo-operation (operation)
  (:documentation "Redoes all side effects of OPERATION. Has side effects."))

(def (generic e) undo-operation (operation)
  (:documentation "Undoes all side effects of OPERATION. Has side effects."))

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

(def (function e) make-operation/compound (elements)
  (make-instance 'operation/compound :elements elements))

(def (function e) make-operation/quit ()
  (make-instance 'operation/quit))

(def (function e) make-operation/undo ()
  (make-instance 'operation/undo))

(def (function e) make-operation/replace-content (document content)
  (make-instance 'operation/replace-content :document document :content content))

(def (function e) make-operation/replace-selection (document selection)
  (make-instance 'operation/replace-selection :document document :selection selection))

(def (function e) make-operation/replace-target (document target replacement)
  (make-instance 'operation/replace-target :document document :target target :replacement replacement))

(def (function e) make-operation/save-document (document)
  (make-instance 'operation/save-document :document document :filename #P"/tmp/document.pred"))

(def (function e) make-operation/load-document (document)
  (make-instance 'operation/load-document :document document :filename #P"/tmp/document.pred"))

(def (function e) make-operation/export-document (document)
  (make-instance 'operation/export-document :document document :filename #P"/tmp/document.txt"))

(def (function e) make-operation/import-document (document)
  (make-instance 'operation/import-document :document document :filename #P"/tmp/document.txt"))

(def (function e) make-operation/select-next-alternative (alternatives)
  (make-instance 'operation/select-next-alternative :alternatives alternatives))

;;;;;;
;;; Operation API implementation

(def method operation? (object)
  (typep object 'operation))

(def method redo-operation ((operation operation/compound))
  (iter (for element :in-sequence (elements-of operation))
        (redo-operation element)))

(def method redo-operation ((operation operation/quit))
  (throw :quit-editor nil))

(def method redo-operation ((operation operation/undo))
  (not-yet-implemented))

(def method redo-operation ((operation operation/replace-content))
  (setf (content-of (document-of operation)) (content-of operation)))

(def method redo-operation ((operation operation/replace-selection))
  (bind ((document (document-of operation))
         (selection (selection-of operation)))
    (labels ((recurse (document selection)
               (when (typep document 'document/base)
                 (setf (selection-of document) (reverse selection)))
               (when (rest selection)
                 (recurse (eval-reference document (first selection)) (rest selection)))))
      (recurse document (reverse selection)))
    (editor.debug "Selection of ~A is set to ~A" document selection)))

(def method redo-operation ((operation operation/replace-target))
  (setf (eval-reference (document-of operation) (reference/flatten (reverse (target-of operation)))) (replacement-of operation)))

(def method redo-operation ((operation operation/save-document))
  (with-output-to-file (output (filename-of operation) :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:serialize (document-of operation) :output output)))

(def method redo-operation ((operation operation/load-document))
  (with-input-from-file (input (filename-of operation) :element-type '(unsigned-byte 8))
    (bind ((document (hu.dwim.serializer:deserialize input)))
      (setf (content-of (document-of operation)) (content-of document))
      (setf (selection-of (document-of operation)) (selection-of document)))))

(def method redo-operation ((operation operation/export-document))
  (with-output-to-file (output (filename-of operation) :if-does-not-exist :create :if-exists :overwrite :element-type 'character)
    (print-document (content-of (document-of operation)) output)))

(def method redo-operation ((operation operation/import-document))
  (with-input-from-file (input (filename-of operation) :element-type 'character)
    (not-yet-implemented)))

(def method redo-operation ((operation operation/select-next-alternative))
  (bind ((alternatives (alternatives-of operation)))
    (setf (selection-of alternatives) (mod (1+ (selection-of alternatives))
                                           (length (alternatives-of alternatives))))))

(def method redo-operation ((operation operation/describe))
  (values))

(def method redo-operation ((operation operation/show-context-sensitive-help))
  (values))
