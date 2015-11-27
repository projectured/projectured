;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Class

(def class* backend ()
  ((read-evaluate-print-loop :type function)
   (input-from-devices :type function)
   (output-to-devices :type function))
  (:documentation "A backend describes the input and output operations with the external world."))

;;;;;;
;;; Construction

(def function make-backend (read-evaluate-print-loop input-from-devices output-to-devices)
  (make-instance 'backend
                 :read-evaluate-print-loop read-evaluate-print-loop
                 :input-from-devices input-from-devices
                 :output-to-devices output-to-devices))
