;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Reader API

(def generic read-from-device (document device)
  (:documentation "Reads in the context of DOCUMENT from a single DEVICE. Has side effects on DEVICE."))

(def namespace reader)

(def definer reader (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "READER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-reader ',name) ',function-name))))

(def function reader-projection (projection printer-iomap)
  (bind ((input (input-of printer-iomap)))
    (or (when (typep input 'document)
          (projection-of input))
        projection)))

(def function call-reader (projection recursion input #+nil input-reference printer-iomap)
  (funcall (reader-of projection) projection recursion input #+nil input-reference printer-iomap))

(def function recurse-reader (recursion input #+nil input-reference printer-iomap)
  #+nil(assert (not (eq 'the (first (first input-reference)))))
  (call-reader (reader-projection recursion printer-iomap) recursion input #+nil input-reference printer-iomap))

(def function apply-reader (input projection printer-iomap &optional (recursion (make-projection/preserving)))
  (call-reader (reader-projection projection printer-iomap) recursion input #+nil nil printer-iomap))

(def function reader-output (input projection printer-iomap &optional (recursion (make-projection/preserving)))
  (output-of (call-reader (reader-projection projection printer-iomap) recursion input #+nil nil printer-iomap)))

;;;;;;
;;; Reader API implementation

(def method read-from-device (object (device device/output))
  ;; NOTE: continue if DEVICE is also an input device, otherwise do nothing
  (if (typep device 'device/input)
      (call-next-method)
      (values)))
