;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection text-searching ()
  ())

;;;;;;
;;; Construction

(def function make-projection/text-searching ()
  (make-projection 'text-searching))

;;;;;;
;;; Construction

(def macro text-searching ()
  `(make-projection/text-searching))

;;;;;;
;;; Printer

(def printer text-searching (projection recursion input input-reference)
  (bind ((text input)
         (output-selection (pattern-case (selection-of input)
                             (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                              ;; TODO:
                              `((the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))))
         (output (labels ((search-line (start-position direction)
                            (bind ((line-start-position (text/line-start-position text start-position))
                                   (line-end-position (text/line-end-position text start-position))
                                   (line (text/substring text line-start-position line-end-position)))
                              ;; TODO: search string
                              (if (search "0" (text/as-string line))
                                  (make-computed-ll (as (ll (append (elements-of line) (list (text/newline)))))
                                                    (as (awhen (text/previous-position text line-start-position)
                                                          (search-line it :backward)))
                                                    (as (awhen (text/next-position text line-end-position)
                                                          (search-line it :forward))))
                                  (awhen (ecase direction
                                           (:backward (text/previous-position text line-start-position))
                                           (:forward (text/next-position text line-end-position)))
                                    (search-line it direction))))))
                   (text/make-text (as (append-ll (search-line (text/origin-position text) :forward))) :selection output-selection))))
    ;; TODO: proper iomap
    (make-iomap/object projection recursion text input-reference output)))

;;;;;;
;;; Reader

(def reader text-searching (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
