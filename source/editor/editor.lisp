;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

(def class* editor ()
  ((document :type document)
   (projection :type projection)
   (backend :type backend)
   (devices :type sequence)
   (reader-iomap :type iomap)
   (printer-iomap :type iomap)
   ;; TODO: merge these into reader-iomap?
   (event-queue :type event-queue)
   (gesture-queue :type gesture-queue)))

;;;;;;
;;; Construction

(def function make-editor (document projection backend devices)
  (make-instance 'editor
                 :document document
                 :projection projection
                 :backend backend
                 :devices devices
                 :event-queue (make-event-queue)
                 :gesture-queue (make-gesture-queue)))

;;;;;;
;;; API

(def function call-read-evaluate-print-loop (editor &rest args &key &allow-other-keys)
  (apply (read-evaluate-print-loop-of (backend-of editor)) editor args))

(def function call-input-from-devices (editor)
  (funcall (input-from-devices-of (backend-of editor)) editor))

(def function call-output-to-devices (editor)
  (funcall (output-to-devices-of (backend-of editor)) editor))

(def function read-evaluate-print-loop (editor &key measure (measure-reader measure) (measure-evaluator measure) (measure-printer measure))
  (catch :quit-editor
    (iter (with document = (document-of editor))
          (with-measuring (:printer measure-printer)
            (editor.debug "Printing ~A to output devices" document)
            (call-output-to-devices editor))
          (for operation = (with-measuring (:reader measure-reader)
                             (editor.debug "Reading from input devices for ~A" document)
                             (call-input-from-devices editor)))
          (with-measuring (:evaluator measure-evaluator)
            (editor.debug "Evaluating ~A on ~A" operation document)
            (call-evaluator document operation)))))
