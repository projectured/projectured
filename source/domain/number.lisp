;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Number domain provides:
;;;;  - number classes provided by the Common Lisp implementation

;;;;;;
;;; Number document classes
;;;
;;; The number document classes are provided by the Common Lisp implementation.

;;;;;;
;;; Number document constructors
;;;
;;; The number document constructores are provided by the Common Lisp implementation.

;;;;;;
;;; Number operation classes

(def operation operation/number/replace-range ()
  ((document :type document)
   (selection :type reference)
   (replacement :type sequence)))

;;;;;;
;;; Number operation constructors

(def function make-operation/number/replace-range (document selection replacement)
  (make-instance 'operation/number/replace-range
                 :document document
                 :selection selection
                 :replacement replacement))

;;;;;;
;;; Number operation API implementation

(def method run-operation ((operation operation/number/replace-range))
  (bind (((:values reference start end)
          (pattern-case (reverse (selection-of operation))
            (((the string (subseq (the string document) ?b ?c))
              (the string (write-to-string (the number document)))
              . ?rest)
             (values `(,@(reverse ?rest) (the sequence document)) ?b ?c)))))
    (bind ((document (document-of operation))
           (flat-reference (reference/flatten reference))
           (old-sequence (aif (eval-reference document flat-reference)
                              (write-to-string it)
                              ""))
           (new-sequence (concatenate (form-type old-sequence)
                                      (subseq old-sequence 0 start) (replacement-of operation) (subseq old-sequence end))))
      (setf (eval-reference document flat-reference) (unless (string= new-sequence "")
                                                       (parse-number:parse-number new-sequence)))
      ;; KLUDGE: can't do this in a separate operation
      (bind ((character-index (+ start (length (replacement-of operation)))))
        (run-operation (make-operation/replace-selection document (append (butlast reference)
                                                                          `((the string (write-to-string (the number document)))
                                                                            (the string (subseq (the string document) ,character-index ,character-index))))))))))
