;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text-aligning ()
  ((alignment-width :type number)))

;;;;;;
;;; Construction

(def function make-projection/text-aligning (alignment-width)
  (make-projection 'text-aligning :alignment-width alignment-width))

;;;;;;
;;; Construction

(def macro text-aligning (alignment-width)
  `(make-projection/text-aligning ,alignment-width))

;;;;;;
;;; Forward mappper

(def function forward-mapper/text-aligning (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the ?type (content-of (the book/paragraph document)))
        . ?rest)
       (values nil
               ?rest
               (elt (child-iomaps-of printer-iomap) 0)))
      (((the text/text (printer-output (the book/paragraph document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mappper

(def function backward-mapper/text-aligning (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      ((?or nil ((the text/text (text/subseq (the text/text document) ?start-index ?end-index))))
       (values `((the ,(form-type (content-of (input-of printer-iomap))) (content-of (the book/paragraph document))))
               reference
               (elt (child-iomaps-of printer-iomap) 0)))
      (?a
       (append `((the text/text (printer-output (the book/paragraph document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer text-aligning (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) `((content-of (the book/paragraph document))
                                                                            ,@(typed-reference (form-type input) input-reference)))))
         (lines (as (bind ((text (output-of (va content-iomap))))
                      (labels ((align-line (line-start-position line-end-position line-alignment line-expansion)
                                 (bind ((parts (iter (for start-position :initially line-start-position :then (text/find text end-position (complement #'whitespace?) :end-position line-end-position))
                                                     (while start-position)
                                                     (when end-position
                                                       (collect (cons :whitespace (text/substring text end-position start-position))))
                                                     (until (text/position<= text line-end-position start-position))
                                                     (for end-position = (or (text/find text start-position #'whitespace? :end-position line-end-position) (text/last-position text)))
                                                     (while end-position)
                                                     (when start-position
                                                       (collect (cons :word (text/substring text start-position end-position))))
                                                     (until (text/position<= text line-end-position end-position))))
                                        (part-count (length parts))
                                        (word-count (count :word parts :key 'car))
                                        (total-width (iter (for part :in-sequence parts)
                                                           (summing (text/width (cdr part)))))
                                        (extra-width (- (alignment-width-of projection) total-width))
                                        (spacing-size (if (> word-count 1)
                                                          (/ (* line-expansion extra-width) (1- word-count))
                                                          0))
                                        (output-elements (iter (for index :from 0)
                                                               (for part :in-sequence parts)
                                                               (for elements = (elements-of (cdr part)))
                                                               (ecase (car part)
                                                                 (:word
                                                                  (cond ((= index 0)
                                                                         (collect (text/clone (first-elt elements) :padding (make-inset :left (* extra-width line-alignment (- 1 line-expansion)))))
                                                                         (appending (subseq elements 1)))
                                                                        ((= index (1- part-count))
                                                                         (appending (subseq elements 0 (1- (length elements))))
                                                                         (collect (text/clone (last-elt elements) :padding (make-inset :right (* extra-width (1- line-alignment) (- 1 line-expansion))))))
                                                                        (t (appending elements))))
                                                                 (:whitespace
                                                                  (cond ((= index 0)
                                                                         (appending (subseq elements 1))
                                                                         (collect (text/clone (first-elt elements) :padding (make-inset :left (* extra-width line-alignment (- 1 line-expansion))))))
                                                                        ((= index (1- part-count))
                                                                         (appending (subseq elements 0 (1- (length elements))))
                                                                         (collect (text/clone (last-elt elements) :padding (make-inset :right (* extra-width (1- line-alignment) (- 1 line-expansion))))))
                                                                        (t
                                                                         (appending (subseq elements 0 (1- (length elements))))
                                                                         (collect (text/clone (last-elt elements) :padding (make-inset :right spacing-size))))))))))
                                   (ll (if (text/last-position? input line-end-position)
                                           output-elements
                                           (append output-elements (list (text/newline)))))))
                               (make-element (start-position)
                                 (bind ((line-start-position (text/line-start-position text start-position :compute-distance #t))
                                        (line-end-position (text/line-end-position text start-position :compute-distance #t))
                                        (last-position? (text/last-position? text line-end-position))
                                        (line-alignment (ecase (alignment-of input)
                                                          (:left 0)
                                                          (:right 1)
                                                          (:center 0.5)
                                                          (:justified 0)))
                                        (line-expansion (ecase (alignment-of input)
                                                          (:left 0)
                                                          (:right 0)
                                                          (:center 0)
                                                          (:justified (if last-position? 0 1))))
                                        (line (align-line line-start-position line-end-position line-alignment line-expansion)))
                                   (make-computed-ll (as line)
                                                     (as (awhen (text/previous-position text line-start-position)
                                                           (make-element it)))
                                                     (as (awhen (text/next-position text line-end-position)
                                                           (make-element it)))))))
                        (when (elements-of text)
                          (make-element (text/origin-position (output-of (va content-iomap)))))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va content-iomap))))
                                                (selection-of input)
                                                'forward-mapper/text-aligning)))
         (output (if (eq (alignment-of input) :left)
                     (as (output-of (va content-iomap)))
                     (text/make-text (as (append-ll (va lines))) :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

;;;;;;
;;; Reader

(def reader text-aligning (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/text-aligning 'backward-mapper/text-aligning)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/text-aligning nil)
                  (make-command/nothing (gesture-of input))))
