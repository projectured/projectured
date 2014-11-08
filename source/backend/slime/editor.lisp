;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Editor

(def class* editor/slime (editor)
  ())

;;;;;;
;;; Construction

(def method make-editor (&key &allow-other-keys)
  (make-instance 'editor/slime
                 :devices (list (make-instance 'device/mouse)
                                (make-instance 'device/keyboard)
                                (make-device/display/slime))
                 :event-queue (make-instance 'event-queue :events nil)
                 :gesture-queue (make-instance 'gesture-queue :gestures nil)))

;;;;;;
;;; API

(def method read-event ((devices sequence))
  (make-instance 'event/window/quit))

(def method print-document (document (stream (eql :slime)))
  (print-to-device (output-of (apply-printer document (make-projection/t->text))) (make-instance 'device/display/slime))
  (values))
