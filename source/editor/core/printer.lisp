;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Printer API

(def generic print-to-device (document device)
  (:documentation "Prints DOCUMENT to a single DEVICE. Has side effects on DEVICE."))

(def namespace printer)

(def definer printer (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "PRINTER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-printer ',name) ',function-name))))

(def function printer-projection (projection input)
  (or (when (typep input 'document)
        (projection-of input))
      projection))

(def function call-printer (projection recursion input input-reference #+nil reader-iomap)
  (funcall (printer-of projection) projection recursion input input-reference #+nil reader-iomap))

(def function recurse-printer (recursion input input-reference #+nil reader-iomap)
  (assert (not (eq 'the (first (first input-reference)))))
  (call-printer (printer-projection recursion input) recursion input input-reference #+nil reader-iomap))

(def function apply-printer (input projection #+nil reader-iomap &optional (recursion (make-projection/preserving)))
  (call-printer (printer-projection projection input) recursion input nil #+nil reader-iomap))

(def function printer-output (input projection #+nil reader-iomap &optional (recursion (make-projection/preserving)))
  (output-of (call-printer (printer-projection projection input) recursion input nil #+nil reader-iomap)))

;;;;;;
;;; Printer API implementation

(def method print-to-device (document (device device/input))
  ;; NOTE: continue if DEVICE is also an output device, otherwise do nothing
  (if (typep device 'device/output)
      (call-next-method)
      (values)))
