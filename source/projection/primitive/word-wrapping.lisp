;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection word-wrapping ()
  ((wrap-width :type number)))

;;;;;;
;;; Construction

(def function make-projection/word-wrapping (wrap-width)
  (make-projection 'word-wrapping :wrap-width wrap-width))

;;;;;;
;;; Construction

(def macro word-wrapping (wrap-width)
  `(make-projection/word-wrapping ,wrap-width))

;;;;;;
;;; IO map

(def iomap iomap/word-wrapping ()
  ((line-iomaps :type sequence)))

(def iomap iomap/word-wrapping/line ()
  ((input-start-index :type integer)
   (input-end-index :type integer)
   (input-newline-insertion-indices :type sequence)
   (output-start-index :type integer)
   (output-end-index :type integer)
   (output-newline-insertion-indices :type sequence)))

;;;;;;
;;; Forward mappper

(def function forward-mapper/word-wrapping (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (labels ((find-output-index (index)
               (iter (for line-iomap-element :initially (line-iomaps-of printer-iomap) :then (next-element-of line-iomap-element))
                     (while line-iomap-element)
                     (for line-iomap = (value-of line-iomap-element))
                     (when (<= (input-start-index-of line-iomap) index (input-end-index-of line-iomap))
                       (return (+ (output-start-index-of line-iomap)
                                  (- index (input-start-index-of line-iomap))
                                  (funcall 'count index (input-newline-insertion-indices-of line-iomap) :test '>)))))))
      (pattern-case reference
        (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
         `((the text/text (text/subseq (the text/text document) ,(find-output-index ?start-index) ,(find-output-index ?end-index)))))
        (((the text/text (printer-output (the text/text document) ?projection ?recursion)) . ?rest)
         (when (and (eq projection ?projection) (eq recursion ?recursion))
           ?rest))))))

;;;;;;
;;; Backward mappper

(def function backward-mapper/word-wrapping (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (labels ((find-input-index (index)
               (iter (for line-iomap-element :initially (line-iomaps-of printer-iomap) :then (next-element-of line-iomap-element))
                     (while line-iomap-element)
                     (for line-iomap = (value-of line-iomap-element))
                     (when (<= (output-start-index-of line-iomap) index (output-end-index-of line-iomap))
                       (return (unless (find (1- index) (output-newline-insertion-indices-of line-iomap))
                                 (+ (input-start-index-of line-iomap)
                                    (- index (output-start-index-of line-iomap))
                                    (- (funcall 'count index (output-newline-insertion-indices-of line-iomap) :test '>)))))))))
      (pattern-case reference
        (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
         (bind ((start-index (find-input-index ?start-index))
                (end-index (find-input-index ?end-index)))
           (if (and start-index end-index)
               `((the text/text (text/subseq (the text/text document) ,start-index ,end-index)))
               (append `((the text/text (printer-output (the text/text document) ,projection ,recursion))) reference))))
        (?a
         (append `((the text/text (printer-output (the text/text document) ,projection ,recursion))) reference))))))

;;;;;;
;;; Printer

(def printer word-wrapping (projection recursion input input-reference)
  (bind ((line-iomaps (as (labels ((wrap-line (line-start-position line-end-position output-start-index output-end-index)
                                     (iter (with x = 0)
                                           (with wrap-width = (wrap-width-of projection))
                                           (with input-newline-insertion-indices = nil)
                                           (with output-newline-insertion-indices = nil)
                                           (with input-character-index = (text/position-distance line-start-position))
                                           (with output-character-index = output-start-index)
                                           (for start-position :initially line-start-position :then (text/find input end-position (complement #'whitespace?)))
                                           (while start-position)
                                           ;; TODO: stop at what?
                                           (for whitespace-elements = (unless (first-iteration-p)
                                                                        (elements-of (text/substring input end-position start-position))))
                                           (for whitespace-width = (iter (with sum = 0)
                                                                         (for element :in-sequence whitespace-elements)
                                                                         (typecase element
                                                                           (text/newline
                                                                            (setf x 0)
                                                                            (setf sum 0))
                                                                           (text/string
                                                                            (incf sum (2d-x (measure-text (content-of element) (font-of element)))))
                                                                           (t
                                                                            ;; KLUDGE:
                                                                            (incf sum 100)))
                                                                         (finally (return sum))))
                                           (incf x whitespace-width)
                                           (incf input-character-index (text/length (text/make-text whitespace-elements)))
                                           (incf output-character-index (text/length (text/make-text whitespace-elements)))
                                           (appending (coerce whitespace-elements 'list) :into output-elements)
                                           ;; TODO: just append the vector
                                           (until (text/position<= input line-end-position start-position))
                                           (for end-position = (or (text/find input start-position #'whitespace?) (text/last-position input)))
                                           (while end-position)
                                           (for word-elements = (elements-of (text/substring input start-position end-position)))
                                           (for word-width = (iter (for element :in-sequence word-elements)
                                                                   (summing
                                                                    (typecase element
                                                                      (text/string (2d-x (measure-text (content-of element) (font-of element))))
                                                                      ;; KLUDGE:
                                                                      (t 100)))))
                                           (incf x word-width)
                                           (when (and output-elements (> x wrap-width))
                                             (setf x word-width)
                                             (push input-character-index input-newline-insertion-indices)
                                             (push output-character-index output-newline-insertion-indices)
                                             (incf output-character-index)
                                             (collect (text/newline) :into output-elements))
                                           (incf input-character-index (text/length (text/make-text word-elements)))
                                           (incf output-character-index (text/length (text/make-text word-elements)))
                                           ;; TODO: just append the vector
                                           (appending (coerce word-elements 'list) :into output-elements)
                                           (until (text/position<= input line-end-position end-position))
                                           (finally (return (bind ((line-start-index (text/position-distance line-start-position))
                                                                   (line-end-index (text/position-distance line-end-position))
                                                                   (line-length (- line-end-index line-start-index))
                                                                   (newline-insertion-count (length input-newline-insertion-indices)))
                                                              (make-instance 'iomap/word-wrapping/line
                                                                             :projection projection :recursion recursion
                                                                             :input input :input-reference input-reference :output (ll (if (text/last-position? input line-end-position)
                                                                                                                                           output-elements
                                                                                                                                           (append output-elements (list (text/newline)))))
                                                                             :input-start-index line-start-index :input-end-index line-end-index
                                                                             :input-newline-insertion-indices (nreverse input-newline-insertion-indices)
                                                                             :output-start-index (or output-start-index (- output-end-index line-length newline-insertion-count)) :output-end-index (or output-end-index (+ output-start-index line-length newline-insertion-count))
                                                                             :output-newline-insertion-indices (nreverse output-newline-insertion-indices)))))))
                                   (make-element (start-position output-start-index output-end-index)
                                     (bind ((line-start-position (text/line-start-position input start-position :compute-distance #t))
                                            (line-end-position (text/line-end-position input start-position :compute-distance #t))
                                            (line-length (1+ (- (text/position-distance line-end-position) (text/position-distance line-start-position))))
                                            (line-iomap (wrap-line line-start-position line-end-position output-start-index output-end-index)))
                                       (make-computed-ll (as line-iomap)
                                                         (as (awhen (text/previous-position input line-start-position)
                                                               (make-element it nil (- output-end-index line-length (length (input-newline-insertion-indices-of line-iomap))))))
                                                         (as (awhen (text/next-position input line-end-position)
                                                               (make-element it (+ output-start-index line-length (length (input-newline-insertion-indices-of line-iomap))) nil)))))))
                            (make-element (text/origin-position input) 0 nil))))
         (output-selection (as (print-selection (make-iomap 'iomap/word-wrapping
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :line-iomaps line-iomaps)
                                                (selection-of input)
                                                'forward-mapper/word-wrapping)))
         (output (text/make-text (as (append-ll (map-ll (va line-iomaps) 'output-of))) :selection output-selection)))
    (make-iomap 'iomap/word-wrapping
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :line-iomaps line-iomaps)))

;;;;;;
;;; Reader

(def reader word-wrapping (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/word-wrapping nil)
                  (make-command/nothing (gesture-of input))))
