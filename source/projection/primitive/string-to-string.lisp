;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/string (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)
   (input-offset :type integer)
   (output-offset :type integer)
   (length :type integer)))

;;;;;;
;;; Construction

(def (function e) make-iomap/string (input input-reference input-offset output output-reference output-offset length)
  (make-iomap 'iomap/string
              :input input :input-reference (typed-reference (form-type input) input-reference) :input-offset input-offset
              :output output :output-reference (typed-reference (form-type output) output-reference) :output-offset output-offset
              :length length))

;;;;;;
;;; Reference applier

#+nil
(def reference-applier iomap/string (iomap reference function)
  (declare (ignore function))
  (pattern-case reference
    ((the character (elt (the string ?a) ?b))
     (cond ((equal (input-reference-of iomap) `(the string ,?a))
            (elt (input-of iomap) ?b))
           ((equal (output-reference-of iomap) `(the string ,?a))
            (elt (output-of iomap) ?b))))))

;;;;;;
;;; Forward mapper

#+nil
(def forward-mapper iomap/string (iomap input-reference function)
  (pattern-case input-reference
    ((the character (elt (the string ?a) ?b))
     (when (and (equal `(the string ,?a) (input-reference-of iomap))
                (<= (input-offset-of iomap) ?b (+ -1 (input-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the character (elt ,(output-reference-of iomap)
                                                    ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
    ((the sequence-position (pos (the string ?a) ?b))
     (when (and (equal `(the string ,?a) (input-reference-of iomap))
                (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the sequence-position (pos ,(output-reference-of iomap)
                                                            ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
    ((the sequence (subseq (the string ?a) ?b ?c))
     (when (and (equal `(the string ,?a) (input-reference-of iomap))
                (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap)))
                (<= (input-offset-of iomap) ?c (+ (input-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the sequence (subseq ,(output-reference-of iomap)
                                                      ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))
                                                      ,(+ (- ?c (input-offset-of iomap)) (output-offset-of iomap)))))))))

;;;;;;
;;; Backward mapper

#+nil
(def backward-mapper iomap/string (iomap output-reference function)
  (pattern-case output-reference
    ((the character (elt (the string ?a) ?b))
     (when (and (equal `(the string ,?a) (output-reference-of iomap))
                (<= (output-offset-of iomap) ?b (+ -1 (output-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the character (elt ,(input-reference-of iomap)
                                                    ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
    ((the sequence-position (pos (the string ?a) ?b))
     (when (and (equal `(the string ,?a) (output-reference-of iomap))
                (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the sequence-position (pos ,(input-reference-of iomap)
                                                            ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
    ((the sequence (subseq (the string ?a) ?b ?c))
     (when (and (equal `(the string ,?a) (output-reference-of iomap))
                (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap)))
                (<= (output-offset-of iomap) ?c (+ (output-offset-of iomap) (length-of iomap))))
       (funcall function iomap `(the sequence (subseq ,(input-reference-of iomap)
                                                      ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))
                                                      ,(+ (- ?c (output-offset-of iomap)) (input-offset-of iomap)))))))))
