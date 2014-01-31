;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Printer API

(def (generic e) print-to-device (document device)
  (:documentation "Prints DOCUMENT to a single DEVICE. Has side effects on DEVICE."))

(def (namespace e) printer)

(def (definer e) printer (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "PRINTER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-printer ',name) ',function-name))))

(def (function e) call-printer (projection recursion input input-reference #+nil reader-iomap)
  (funcall (printer-of projection) projection recursion input input-reference #+nil reader-iomap))

(def (function e) recurse-printer (recursion input input-reference #+nil reader-iomap)
  (assert (not (eq 'the (first (first input-reference)))))
  (call-printer recursion recursion input input-reference #+nil reader-iomap))

(def (function e) apply-printer (input projection #+nil reader-iomap &optional (recursion (make-projection/preserving)))
  (call-printer projection recursion input nil #+nil reader-iomap))

(def (function e) printer-output (input projection #+nil reader-iomap &optional (recursion (make-projection/preserving)))
  (output-of (call-printer projection recursion input nil #+nil reader-iomap)))

;;;;;;
;;; Printer API implementation

(def method print-to-device (document (device device/input))
  ;; NOTE: continue if DEVICE is also an output device, otherwise do nothing
  (if (output-device? device)
      (call-next-method)
      (values)))
