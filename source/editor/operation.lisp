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

(def (definer :available-flags "e") operation (name supers slots)
  `(def class* ,name ,supers ,slots))

;;;;;;
;;; Operation classes

(def class* operation ()
  ()
  (:documentation "Base class for operations."))

(def class* operation/compound (operation)
  ((elements :type sequence))
  (:documentation "A sequence of operations carried out in the order they appear in elements."))

(def class* operation/quit (operation)
  ()
  (:documentation "An operation that quits the editor."))

(def class* operation/undo (operation)
  ()
  (:documentation "An operation that undoes the effect of the last operation."))

(def class* operation/replace-content (operation)
  ((document :type document)
   (content :type t))
  (:documentation "An operation that replaces the content of a document."))

(def class* operation/replace-selection (operation)
  ((document :type document)
   (selection :type selection))
  (:documentation "An operation that replaces the selection of a document."))

(def class* operation/replace-target (operation)
  ((document :type document)
   (target :type reference)
   (replacement :type t)))

(def class* operation/save-document (operation)
  ((document :type document)))

(def class* operation/load-document (operation)
  ((document :type document)))

(def class* operation/select-next-alternative (operation)
  ((alternatives :type alternatives)))

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
  (make-instance 'operation/save-document :document document))

(def (function e) make-operation/load-document (document)
  (make-instance 'operation/load-document :document document))

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
  (print (setf (selection-of (document-of operation)) (selection-of operation))))

(def method redo-operation ((operation operation/replace-target))
  (setf (eval-reference (document-of operation) (target-of operation)) (replacement-of operation)))

(def method redo-operation ((operation operation/save-document))
  (with-output-to-file (output "/tmp/document.pred" :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (hu.dwim.serializer:serialize (document-of operation) :output output)))

(def method redo-operation ((operation operation/load-document))
  (with-input-from-file (input "/tmp/document.pred" :element-type '(unsigned-byte 8))
    (bind ((document (hu.dwim.serializer:deserialize input)))
      (setf (content-of (document-of operation)) (content-of document))
      (setf (selection-of (document-of operation)) (selection-of document)))))

(def method redo-operation ((operation operation/select-next-alternative))
  (bind ((alternatives (alternatives-of operation)))
    (setf (selection-of alternatives) (mod (1+ (selection-of alternatives))
                                           (length (alternatives-of alternatives))))))
