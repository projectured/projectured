;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Reader API

(def (generic e) read-from-device (document device)
  (:documentation "Reads in the context of DOCUMENT from a single DEVICE. Has side effects on DEVICE."))

(def (namespace e) reader)

(def (definer e) reader (name arguments &body forms)
  `(setf (find-reader ',name) (lambda ,arguments ,@forms)))

;;;;;;
;;; TODO:
;;;
;;; Recurse with:
;;;  - input (event -> gesture-queue -> operation)
;;;  - printer-iomap (top level printer result)
;;;  - printer-current-iomap
;;;  - printer-input (the current input object of the printer)
;;;  - document (holding the current selection)

(def (function e) apply-reader (projection printer-iomap gesture-queue &optional (recursion (make-projection/preserving)))
  (funcall (reader-of projection) projection recursion printer-iomap printer-iomap gesture-queue nil nil))

(def (function e) recurse-reader (recursion printer-iomap projection-iomap gesture-queue operation document)
  (funcall (reader-of recursion) recursion recursion printer-iomap projection-iomap gesture-queue operation document))

(def (function e) reader-output (projection printer-iomap gesture-queue &optional (recursion (make-projection/preserving)))
  (output-of (apply-reader projection printer-iomap gesture-queue recursion)))

;;;;;;
;;; Reader API implementation

(def method read-from-device (object (device device/output))
  ;; NOTE: continue if DEVICE is also an input device, otherwise do nothing
  (if (input-device? device)
      (call-next-method)
      (values)))
