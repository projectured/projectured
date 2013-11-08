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

(def (function e) make-iomap/sequential (input input-reference output output-reference element-iomaps)
  (make-iomap 'iomap/sequential
              :input input :input-reference input-reference
              :output output :output-reference output-reference
              :element-iomaps element-iomaps))

;;;;;;
;;; Reference applier

(def reference-applier iomap/sequential (iomap reference function)
  (iter (for element-iomap :in (element-iomaps-of iomap))
        (thereis (apply-reference element-iomap reference function))))

;;;;;;
;;; Forward mapper

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

(def (projection e) sequential ()
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

(def printer sequential (projection recursion iomap input input-reference output-reference)
  (iter (for output :initially input :then (output-of element-iomap))
        (for index :from 0)
        (for element :in (elements-of projection))
        (for element-input-reference :initially input-reference :then element-output-reference)
        (for element-output-reference = `(printer-output (the ,(form-type output) ,element-input-reference) ,element ,recursion))
        ;; TODO: KLUDGE: properly recurse with iomap
        (for element-iomap = (bind ((iomap (make-iomap/compound projection recursion input input-reference output output-reference
                                                                 (list iomap (make-iomap/sequential input input-reference output element-output-reference element-iomaps)))))
                               (funcall (printer-of element) element recursion iomap output
                                        element-input-reference
                                        (if (= index (1- (length (elements-of projection))))
                                            output-reference
                                            element-output-reference))))
        (collect element-iomap :into element-iomaps)
        (finally (return (make-iomap/sequential input input-reference output output-reference element-iomaps)))))

;;;;;;
;;; Reader

(def reader sequential (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (iter (for index :from 0)
        (for element-iomap :in (reverse (element-iomaps-of projection-iomap)))
        (for element :in (reverse (elements-of projection)))
        (for current-operation :initially operation :then (funcall (reader-of element) element recursion printer-iomap element-iomap gesture-queue current-operation document))
        (finally (return current-operation))))
