;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/document/search->text/text ()
  ((line-iomaps :type sequence)))

(def iomap iomap/document/search->text/text/line ()
  ((input-first-character-index :type integer)
   (output-first-character-index :type integer)
   (length :type integer)))

;;;;;;
;;; Projection

(def projection document/search->text/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/search->text/text ()
  (make-projection 'document/search->text/text))

;;;;;;
;;; Construction

(def macro document/search->text/text ()
  `(make-projection/document/search->text/text))

;;;;;;
;;; Printer

(def function document/search->text/text/map-backward (line-iomaps index)
  (iter (for line-iomap-element :initially line-iomaps :then (if (< index (output-first-character-index-of line-iomap))
                                                                 (previous-element-of line-iomap-element)
                                                                 (next-element-of line-iomap-element)))
        (while line-iomap-element)
        (for line-iomap = (value-of line-iomap-element))
        (when (<= (output-first-character-index-of line-iomap) index (+ (length-of line-iomap) (output-first-character-index-of line-iomap)))
          (return (- (+ index
                        (input-first-character-index-of line-iomap))
                     (output-first-character-index-of line-iomap))))))

(def function document/search->text/text/map-forward (line-iomaps index)
  (iter (for line-iomap-element :initially line-iomaps :then (if (< index (input-first-character-index-of line-iomap))
                                                                 (previous-element-of line-iomap-element)
                                                                 (next-element-of line-iomap-element)))
        (while line-iomap-element)
        (for line-iomap = (value-of line-iomap-element))
        (when (<= (input-first-character-index-of line-iomap) index (+ (length-of line-iomap) (input-first-character-index-of line-iomap)))
          (return (- (+ index
                        (output-first-character-index-of line-iomap))
                     (input-first-character-index-of line-iomap))))))

(def printer document/search->text/text (projection recursion input input-reference)
  (bind ((text (output-of (recurse-printer recursion (content-of input) `((content-of (the document/document document))
                                                                          ,@(typed-reference (form-type input) input-reference)))))
         (search-string (search-of input)))
    (if (zerop (length search-string))
        (make-iomap/object projection recursion input input-reference text)
        (bind ((search-string (search-of input))
               (line-iomaps (labels ((search-line (start-position direction input-start-character-index output-character-index)
                                       (bind ((line-start-position (text/line-start-position text start-position))
                                              (line-end-position (text/line-end-position text start-position))
                                              (line (text/substring text line-start-position line-end-position))
                                              (line-string (text/as-string line))
                                              (line-start-character-index (- input-start-character-index (text/length text line-start-position start-position)))
                                              (line-length (length line-string)))
                                         (if (search search-string line-string)
                                             (make-computed-ll (as (make-iomap 'iomap/document/search->text/text/line
                                                                               :projection projection :recursion recursion
                                                                               :input input :input-reference input-reference
                                                                               :output (ll (append (iter (with first-position = (text/first-position line))
                                                                                                         (for index :initially 0 :then (+ match-index (length search-string)))
                                                                                                         (for match-index = (search search-string line-string :start2 index))
                                                                                                         (for position = (text/relative-position line first-position index))
                                                                                                         (unless match-index
                                                                                                           (appending (elements-of (text/substring line position (text/last-position line)))))
                                                                                                         (while match-index)
                                                                                                         (for match-position = (text/relative-position line first-position match-index))
                                                                                                         (unless (= index match-index)
                                                                                                           (appending (elements-of (text/substring line position match-position))))
                                                                                                         (appending (elements-of (text/replace-style (text/substring line match-position (text/relative-position line match-position (length search-string))) :line-color *color/solarized/background/lighter*))))
                                                                                                   (list (text/newline))))
                                                                               :input-first-character-index line-start-character-index
                                                                               :output-first-character-index output-character-index
                                                                               :length line-length))
                                                               (as (awhen (text/previous-position text line-start-position)
                                                                     (search-line it :backward (1- line-start-character-index) (- output-character-index line-length 1))))
                                                               (as (awhen (text/next-position text line-end-position)
                                                                     (search-line it :forward (+ line-start-character-index line-length 1) (+ output-character-index line-length 1)))))
                                             (awhen (ecase direction
                                                      (:backward (text/previous-position text line-start-position))
                                                      (:forward (text/next-position text line-end-position)))
                                               (search-line it direction
                                                            (ecase direction
                                                              (:backward (1- input-start-character-index))
                                                              (:forward (+ input-start-character-index line-length 1)))
                                                            output-character-index))))))
                              (search-line (text/origin-position text) :forward 0 0)))
               (output-selection (as (pattern-case (selection-of input)
                                       (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index))
                                         (the text/text (content-of (the document/search document))))
                                        (awhen (document/search->text/text/map-forward line-iomaps ?start-character-index)
                                          `((the text/text (text/subseq (the text/text document) ,it ,it))))))))
               (output (text/make-text (append-ll (map-ll line-iomaps 'output-of)) :selection output-selection)))
          (make-iomap 'iomap/document/search->text/text
                      :projection projection :recursion recursion
                      :input input :input-reference input-reference :output output
                      :line-iomaps line-iomaps)))))

;;;;;;
;;; Reader

(def reader document/search->text/text (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap)))
    (if (zerop (length (search-of printer-input)))
        (make-command gesture (operation/extend printer-input `((the ,(form-type (content-of printer-input)) (content-of (the document/search document)))) (operation-of input))
                      :domain (domain-of input)
                      :description (description-of input))
        (merge-commands (awhen (labels ((recurse (operation)
                                          (typecase operation
                                            (operation/quit operation)
                                            (operation/functional operation)
                                            (operation/replace-selection
                                             (awhen (pattern-case (selection-of operation)
                                                      (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                       (awhen (document/search->text/text/map-backward (line-iomaps-of printer-iomap) ?start-index)
                                                         `((the text/text (text/subseq (the text/text document) ,it ,it))
                                                           (the text/text (content-of (the document/search document)))))))
                                               (make-operation/replace-selection printer-input it)))
                                            (operation/sequence/replace-element-range
                                             (awhen (pattern-case (target-of operation)
                                                      (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                       (awhen (document/search->text/text/map-backward (line-iomaps-of printer-iomap) ?start-index)
                                                         `((the text/text (text/subseq (the text/text document) ,it ,it))
                                                           (the text/text (content-of (the document/search document)))))))
                                               (make-operation/sequence/replace-element-range printer-input it (replacement-of operation))))
                                            (operation/compound
                                             (bind ((operations (mapcar #'recurse (elements-of operation))))
                                               (unless (some 'null operations)
                                                 (make-operation/compound operations)))))))
                                 (recurse (operation-of input)))
                          (make-command gesture it
                                        :domain (domain-of input)
                                        :description (description-of input)))
                        (make-command/nothing gesture)))))
