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
  (bind ((function-name (format-symbol (symbol-package name) "READER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-reader ',name) ',function-name))))

(def (function e) apply-reader (projection printer-iomap gesture-queue &optional (recursion (make-projection/preserving)))
  (funcall (reader-of projection) projection recursion printer-iomap gesture-queue nil))

(def (function e) recurse-reader (recursion projection-iomap gesture-queue operation)
  (funcall (reader-of recursion) recursion recursion projection-iomap gesture-queue operation))

(def (function e) reader-output (projection printer-iomap gesture-queue &optional (recursion (make-projection/preserving)))
  (output-of (apply-reader projection printer-iomap gesture-queue recursion)))

;;;;;;
;;; Reader API implementation

(def method read-from-device (object (device device/output))
  ;; NOTE: continue if DEVICE is also an input device, otherwise do nothing
  (if (input-device? device)
      (call-next-method)
      (values)))
