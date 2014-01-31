;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/sequential (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)
   (element-iomaps :type list)))

;;;;;;
;;; Construction

(def (function e) make-iomap/sequential (projection recursion input input-reference output output-reference element-iomaps)
  (make-iomap 'iomap/sequential
              :projection projection :recursion recursion
              :input input :input-reference (typed-reference (form-type input) input-reference)
              :output output :output-reference (when output-reference (typed-reference (form-type output) output-reference))
              :element-iomaps element-iomaps))

;;;;;;
;;; Reference applier

#+nil
(def reference-applier iomap/sequential (iomap reference function)
  (iter (for element-iomap :in (element-iomaps-of iomap))
        (thereis (apply-reference element-iomap reference function))))

;;;;;;
;;; Forward mapper

#+nil
(def forward-mapper iomap/sequential (iomap input-reference function)
  (labels ((recurse (reference remaining-iomaps)
             (when remaining-iomaps
               (map-forward (car remaining-iomaps) reference
                            (lambda (iomap output-reference)
                              (funcall function iomap output-reference)
                              (recurse output-reference (cdr remaining-iomaps)))))))
    (recurse input-reference (element-iomaps-of iomap))))

;;;;;;
;;; Backward mapper

#+nil
(def backward-mapper iomap/sequential (iomap output-reference function)
  (labels ((recurse (reference remaining-iomaps)
             (when remaining-iomaps
               (map-backward (car remaining-iomaps) reference
                             (lambda (iomap input-reference)
                               (funcall function iomap input-reference)
                               (recurse input-reference (cdr remaining-iomaps)))))))
    (recurse output-reference (reverse (element-iomaps-of iomap)))))

;;;;;;
;;; Projection

(def projection sequential ()
  ((elements :type list)))

;;;;;;
;;; Construction

(def (function e) make-projection/sequential (projections)
  (make-projection 'sequential :elements projections))

;;;;;;
;;; Construction

(def (macro e) sequential (&body forms)
  `(make-projection/sequential (list ,@forms)))

;;;;;;
;;; Printer

(def printer sequential (projection recursion input input-reference)
  (iter (for output :initially input :then (output-of element-iomap))
        (for element :in (elements-of projection))
        (for element-input-reference :initially input-reference :then element-output-reference)
        (for element-output-reference = `((printer-output (the ,(form-type output) document) ,element ,recursion) ,@(typed-reference (form-type output) element-input-reference)))
        (for element-iomap = (call-printer element recursion output element-input-reference))
        (collect element-iomap :into element-iomaps)
        (finally (return (make-iomap/sequential projection recursion input input-reference output nil element-iomaps)))))

;;;;;;
;;; Reader

(def reader sequential (projection recursion input printer-iomap)
  (iter (for element-iomap :in (reverse (element-iomaps-of printer-iomap)))
        (for element :in (reverse (elements-of projection)))
        (for current-output :initially input :then (call-reader element recursion current-output element-iomap))
        (finally (return current-output))))

#+nil
(def reader sequential (projection recursion input printer-iomap)
  (iter (for index :from 0)
        (for element-iomap :in (element-iomaps-of printer-iomap))
        (for element :in (elements-of projection))
        (for element-output = (call-reader element recursion input element-iomap))
        (when element-output
          (return (iter (for element-iomap :in (reverse (subseq (element-iomaps-of printer-iomap) 0 index)))
                        (for element :in (reverse (subseq (elements-of projection) 0 index)))
                        (for current-output :initially element-output :then (call-reader element recursion current-output element-iomap))
                        (finally (return current-output)))))))
