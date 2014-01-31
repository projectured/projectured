;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/object (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)))

;;;;;;
;;; Construction

;; TODO: eventually kill output-reference
(def (function e) make-iomap/object (projection recursion input input-reference output &optional output-reference)
  (make-iomap 'iomap/object
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference (typed-reference (form-type input) input-reference))
              :output output :output-reference (when output-reference (typed-reference (form-type output) output-reference))))

;;;;;;
;;; Reference applier

#+nil
(def reference-applier iomap/object (iomap reference function)
  (declare (ignore function))
  (when (tree-search reference (input-reference-of iomap))
    (bind ((candidate (tree-replace reference (input-reference-of iomap) (input-of iomap))))
      (unless (tree-search candidate 'printer-output)
        (eval candidate)))))

;;;;;;
;;; Forward mapper

#+nil
(def forward-mapper iomap/object (iomap input-reference function)
  (if (equal input-reference (input-reference-of iomap))
      (funcall function iomap (output-reference-of iomap))
      (bind ((start (- (length input-reference) (1+ (length (input-reference-of iomap))))))
        (when (and (>= start 0)
                   (equal `((the ,(form-type (output-of iomap)) (printer-output (the ,(form-type (input-of iomap)) document) ,(projection-of iomap) ,(recursion-of iomap)))
                            ,@(input-reference-of iomap))
                          (subseq input-reference start)))
          (funcall function iomap (print`(,@(subseq input-reference 0 start)
                                            ,@(subseq input-reference 0 (1+ (length (input-reference-of iomap))))
                                            ,@(output-reference-of iomap))))))))

;;;;;;
;;; Backward mapper

#+nil
(def backward-mapper iomap/object (iomap output-reference function)
  (if (equal output-reference (output-reference-of iomap))
      (funcall function iomap (input-reference-of iomap))
      (bind ((start (- (length output-reference) (length (output-reference-of iomap)))))
        (when (and (>= start 0)
                   (equal (output-reference-of iomap) (subseq output-reference start)))
          (funcall function iomap `(,@(subseq output-reference 0 start)
                                    (the ,(form-type (output-of iomap)) (printer-output (the ,(form-type (input-of iomap)) document) ,(projection-of iomap) ,(recursion-of iomap)))
                                    ,@(input-reference-of iomap)))))))
