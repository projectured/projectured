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
  ((newline-insertion-indices :type sequence)))

;;;;;;
;;; Printer

;; iterative non lazy sequence based
#+nil
(def printer word-wrapping (projection recursion input input-reference)
  (declare (ignore input-reference))
  (bind ((pack (as (iter (with x = 0)
                         (with newline-insertion-indices = nil)
                         (with output-character-index = 0)
                         (with elements = (elements-of input))
                         (with wrap-width = (wrap-width-of projection))
                         (for (values start-element-index start-character-index)
                              :initially (values 0 0)
                              :then (text/find input end-element-index end-character-index (lambda (c) (not (whitespace? c)))))
                         (for whitespace-elements = (unless (first-iteration-p)
                                                      (elements-of (text/substring input end-element-index end-character-index start-element-index start-character-index))))
                         (for whitespace-width = (iter (with sum = 0) (for element :in-sequence whitespace-elements)
                                                       (typecase element
                                                         (text/string
                                                          (when (find #\NewLine (content-of element))
                                                            (setf x 0)
                                                            (setf sum 0))
                                                          (incf sum (2d-x (measure-text (content-of element) (font-of element)))))
                                                         (t
                                                          ;; KLUDGE:
                                                          (incf sum 100)))
                                                       (finally (return sum))))
                         (incf x whitespace-width)
                         (incf output-character-index (text/length (text/make-text whitespace-elements)))
                         ;; TODO: just append the vector
                         (appending (coerce whitespace-elements 'list) :into output-elements)
                         (until (and (= start-element-index (length elements))
                                     (= start-character-index 0)))
                         (for (values end-element-index end-character-index) = (text/find input start-element-index start-character-index 'whitespace?))
                         (for word-elements = (elements-of (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                         (for word-width = (iter (for element :in-sequence word-elements)
                                                 (summing
                                                  (typecase element
                                                    (text/string (2d-x (measure-text (content-of element) (font-of element))))
                                                    ;; KLUDGE:
                                                    (t 100)))))
                         (incf x word-width)
                         (when (> x wrap-width)
                           (setf x word-width)
                           (push output-character-index newline-insertion-indices)
                           (incf output-character-index)
                           (collect (text/make-string (string #\NewLine) :font *font/default* :font-color *color/default*) :into output-elements))
                         (incf output-character-index (text/length (text/make-text word-elements)))
                         ;; TODO: just append the vector
                         (appending (coerce word-elements 'list) :into output-elements)
                         (until (and (= end-element-index (length elements))
                                     (= end-character-index 0)))
                         (finally (return (list (elements-of (text/consolidate (text/make-text output-elements))) (reverse newline-insertion-indices)))))))
         (output-selection (as (bind ((newline-insertion-indices (elt (va pack) 1)))
                                 (pattern-case (selection-of input)
                                   (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                    (bind ((newline-count (count-if (lambda (index) (< index ?character-index)) newline-insertion-indices)))
                                      (bind ((character-index (+ ?character-index newline-count)))
                                        `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))
                                   (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                    (bind ((start-newline-count (count-if (lambda (index) (< index ?start-character-index)) newline-insertion-indices))
                                           (end-newline-count (count-if (lambda (index) (< index ?end-character-index)) newline-insertion-indices)))
                                      `((the text/text (text/subbox (the text/text document)
                                                                    ,(+ ?start-character-index start-newline-count)
                                                                    ,(+ ?end-character-index end-newline-count))))))))))
         (output (as (text/make-text (as (first (va pack))) :selection output-selection))))
    (make-iomap 'iomap/word-wrapping
                :projection projection :recursion recursion
                :input input :output output
                :newline-insertion-indices (as (elt (va pack) 1)))))

;; iterative non-lazy compute-ll based
#+nil
(def printer word-wrapping (projection recursion input input-reference)
  (declare (ignore input-reference))
  (bind ((elements (iter (with x = 0)
                         (with newline-insertion-indices = nil)
                         (with output-character-index = 0)
                         (with elements = (elements-of input))
                         (with wrap-width = (wrap-width-of projection))
                         (for (values start-element start-character-index)
                              :initially (values elements 0)
                              :then (text/find input end-element-index end-character-index (lambda (c) (not (whitespace? c)))))
                         (for whitespace-elements = (unless (first-iteration-p)
                                                      (elements-of (text/substring input end-element-index end-character-index start-element start-character-index))))
                         (for whitespace-width = (iter (with sum = 0) (for element :in-sequence whitespace-elements)
                                                       (typecase element
                                                         (text/string
                                                          (when (find #\NewLine (content-of element))
                                                            (setf x 0)
                                                            (setf sum 0))
                                                          (incf sum (2d-x (measure-text (content-of element) (font-of element)))))
                                                         (t
                                                          ;; KLUDGE:
                                                          (incf sum 100)))
                                                       (finally (return sum))))
                         (incf x whitespace-width)
                         (incf output-character-index (text/length (text/make-text whitespace-elements)))
                         ;; TODO: just append the vector
                         (appending (coerce whitespace-elements 'list) :into output-elements)
                         (while start-element)
                         (for (values end-element-index end-character-index) = (text/find input start-element start-character-index 'whitespace?))
                         (for word-elements = (elements-of (text/substring input start-element start-character-index end-element-index end-character-index)))
                         (for word-width = (iter (for element :in-sequence word-elements)
                                                 (summing
                                                  (typecase element
                                                    (text/string (2d-x (measure-text (content-of element) (font-of element))))
                                                    ;; KLUDGE:
                                                    (t 100)))))
                         (incf x word-width)
                         (when (> x wrap-width)
                           (setf x word-width)
                           (push output-character-index newline-insertion-indices)
                           (incf output-character-index)
                           (collect (text/make-string (string #\NewLine) :font *font/default* :font-color *color/default*) :into output-elements))
                         (incf output-character-index (text/length (text/make-text word-elements)))
                         ;; TODO: just append the vector
                         (appending (coerce word-elements 'list) :into output-elements)
                         (while end-element-index)
                         (finally (return (elements-of (text/consolidate (text/make-text output-elements)))))))
         (output (text/make-text elements)))
    (make-iomap 'iomap/word-wrapping
                :projection projection :recursion recursion
                :input input :output output
                :newline-insertion-indices nil)))

(def printer word-wrapping (projection recursion input input-reference)
  (declare (ignore input-reference))
  (bind ((elements (labels ((find-line-start-element (element)
                              (if (text/newline? (value-of element))
                                  element
                                  (aif (previous-element-of element)
                                       (find-line-start-element it)
                                       element)))
                            (find-line-end-element (element)
                              (if (text/newline? (value-of element))
                                  element
                                  (aif (next-element-of element)
                                       (find-line-end-element it)
                                       element)))
                            (wrap-line (line-start-element line-end-element)
                              (iter (with x = 0)
                                    (with newline-insertion-indices = nil)
                                    (with output-character-index = 0)
                                    (with wrap-width = (wrap-width-of projection))
                                    (for (values start-element start-character-index)
                                         :initially (values line-start-element 0)
                                         :then (text/find input end-element-index end-character-index (lambda (c) (not (whitespace? c)))))
                                    ;; TODO: stop at what?
                                    #+nil(until (eq start-element line-end-element))
                                    (for whitespace-elements = (unless (first-iteration-p)
                                                                 (elements-of (text/substring input end-element-index end-character-index start-element start-character-index))))
                                    (for whitespace-width = (iter (with sum = 0) (for element :in-sequence whitespace-elements)
                                                                  (typecase element
                                                                    (text/string
                                                                     (when (find #\NewLine (content-of element))
                                                                       (setf x 0)
                                                                       (setf sum 0))
                                                                     (incf sum (2d-x (measure-text (content-of element) (font-of element)))))
                                                                    (t
                                                                     ;; KLUDGE:
                                                                     (incf sum 100)))
                                                                  (finally (return sum))))
                                    (incf x whitespace-width)
                                    (incf output-character-index (text/length (text/make-text whitespace-elements)))
                                    ;; TODO: just append the vector
                                    (appending (coerce whitespace-elements 'list) :into output-elements)
                                    (while start-element)
                                    (for (values end-element-index end-character-index) = (text/find input start-element start-character-index 'whitespace?))
                                    (for word-elements = (elements-of (text/substring input start-element start-character-index end-element-index end-character-index)))
                                    (for word-width = (iter (for element :in-sequence word-elements)
                                                            (summing
                                                             (typecase element
                                                               (text/string (2d-x (measure-text (content-of element) (font-of element))))
                                                               ;; KLUDGE:
                                                               (t 100)))))
                                    (incf x word-width)
                                    (when (> x wrap-width)
                                      (setf x word-width)
                                      (push output-character-index newline-insertion-indices)
                                      (incf output-character-index)
                                      (collect (text/make-string (string #\NewLine) :font *font/default* :font-color *color/default*) :into output-elements))
                                    (incf output-character-index (text/length (text/make-text word-elements)))
                                    ;; TODO: just append the vector
                                    (appending (coerce word-elements 'list) :into output-elements)
                                    (while end-element-index)
                                    (finally (return (ll (elements-of (text/consolidate (text/make-text output-elements))))))))
                            (make-element (element)
                              (bind ((line-start-element (find-line-start-element element))
                                     (line-end-element (find-line-end-element element))
                                     (wrapped-elements (wrap-line line-start-element line-end-element)))
                                (make-computed-ll (as wrapped-elements)
                                                  (as (awhen (previous-element-of line-start-element) (make-element (previous-element-of it))))
                                                  (as (awhen (next-element-of line-end-element) (make-element it)))))))
                     (append-ll (make-element (elements-of input)))))
         (output (text/make-text elements)))
    (make-iomap 'iomap/word-wrapping
                :projection projection :recursion recursion
                :input input :output output
                :newline-insertion-indices nil)))

;;;;;;
;;; Reader

(def reader word-wrapping (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (merge-commands (labels ((recurse (operation)
                             (typecase operation
                               (operation/quit operation)
                               (operation/functional operation)
                               (operation/replace-selection
                                (make-operation/replace-selection (input-of printer-iomap)
                                                                  (pattern-case (selection-of operation)
                                                                    (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                                                     (bind ((newline-count (count-if (lambda (index) (< index ?character-index)) (newline-insertion-indices-of printer-iomap)))
                                                                            (character-index (- ?character-index newline-count)))
                                                                       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index))))))))
                               (operation/sequence/replace-range
                                (awhen (pattern-case (selection-of operation)
                                         (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                                          (bind ((start-newline-count (count-if (lambda (index) (< index ?start-character-index)) (newline-insertion-indices-of printer-iomap)))
                                                 (end-newline-count (count-if (lambda (index) (< index ?end-character-index)) (newline-insertion-indices-of printer-iomap))))
                                            `((the text/text (text/subseq (the text/text document) ,(- ?start-character-index start-newline-count) ,(- ?end-character-index end-newline-count)))))))
                                  (make-operation/sequence/replace-range (input-of printer-iomap) it (replacement-of operation))))
                               (operation/show-context-sensitive-help
                                (make-instance 'operation/show-context-sensitive-help
                                               :commands (iter (for command :in (commands-of operation))
                                                               (awhen (recurse (operation-of command))
                                                                 (collect (make-instance 'command
                                                                                         :gesture (gesture-of command)
                                                                                         :domain (domain-of command)
                                                                                         :description (description-of command)
                                                                                         :operation it))))))
                               (operation/compound
                                (bind ((operations (mapcar #'recurse (elements-of operation))))
                                  (unless (some 'null operations)
                                    (make-operation/compound operations)))))))
                    (awhen (recurse (operation-of input))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input))))
                  (make-command/nothing (gesture-of input))))
