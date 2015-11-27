;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; when inspecting a computed slot with a computed state inside it, then display the computed-state details
(def method swank::slot-value-for-inspector ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  ;; we skip svuc to avoid recalculation of invalid slots
  (bind ((slot-value (standard-instance-access-form object slot)))
    (cond ((unbound-slot-marker? slot-value)
           '("#<unbound>"))
          ((computed-state-p slot-value)
           `((:value ,(cs-value slot-value)) ,(if (computed-state-valid-p slot-value) " valid " " invalid ") (:value ,slot-value) " "
             ,(if (computed-state-valid-p slot-value)
                  `(:action "[invalidate]" ,(lambda () (invalidate-computed-state slot-value)))
                  `(:action "[compute]" ,(lambda () (ensure-computed-state-is-valid slot-value))))))
          (t (call-next-method)))))
