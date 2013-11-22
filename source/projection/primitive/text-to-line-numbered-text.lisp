;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/text->line-numbered-text (iomap/text)
  ((input-reference :type reference)
   (output-reference :type reference)
   (line-number-length :type integer)))

;;;;;;
;;; Reference applier

(def reference-applier iomap/text->line-numbered-text (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

;;;;;;
;;; Forward mapper

(def forward-mapper iomap/text->line-numbered-text (iomap input-reference function)
  (labels ((map-character-index (character-index)
             (+ character-index
                (* (1+ (line-number-length-of iomap))
                   (1+ (count #\NewLine (text/as-string (input-of iomap)) :end character-index))))))
    (pattern-case input-reference
      ((the character (text/elt (the text/text ?a) ?b))
       (funcall function iomap `(the character (text/elt ,(output-reference-of iomap) ,(map-character-index ?b)))))
      ((the sequence-position (text/pos (the text/text ?a) ?b))
       (funcall function iomap `(the sequence-position (text/pos ,(output-reference-of iomap) ,(map-character-index ?b)))))
      ((the sequence (text/subseq (the text/text ?a) ?b ?c))
       (funcall function iomap `(the text/text (text/subseq ,(output-reference-of iomap),(map-character-index ?b) ,(map-character-index ?c)))))
      ((the sequence-box (text/subbox (the text/text ?a) ?b ?c))
       (funcall function iomap `(the sequence-box (text/subbox ,(output-reference-of iomap) ,(map-character-index ?b) ,(map-character-index ?c))))))))

;;;;;;
;;; Backward mapper

(def backward-mapper iomap/text->line-numbered-text (iomap output-reference function)
  (labels ((map-character-index (character-index)
             (- character-index
                (* (1+ (line-number-length-of iomap))
                   (1+ (count #\NewLine (text/as-string (output-of iomap)) :end character-index))))))
    (pattern-case output-reference
      ((the character (text/elt (the text/text ?a) ?b))
       (funcall function iomap `(the character (text/elt ,(input-reference-of iomap) ,(map-character-index ?b)))))
      ((the sequence-position (text/pos (the text/text ?a) ?b))
       (funcall function iomap `(the sequence-position (text/pos ,(input-reference-of iomap) ,(map-character-index ?b)))))
      ((the sequence (text/subseq (the text/text ?a) ?b ?c))
       (funcall function iomap `(the text/text (text/subseq ,(input-reference-of iomap),(map-character-index ?b) ,(map-character-index ?c)))))
      ((the sequence-box (text/subbox (the text/text ?a) ?b ?c))
       (funcall function iomap `(the sequence-box (text/subbox ,(input-reference-of iomap) ,(map-character-index ?b) ,(map-character-index ?c))))))))


;;;;;;
;;; Projection

(def projection text->line-numbered-text ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text->line-numbered-text ()
  (make-projection 'text->line-numbered-text))

;;;;;;
;;; Construction

(def (macro e) text->line-numbered-text ()
  '(make-projection/text->line-numbered-text))

;;;;;;
;;; Printer

(def printer text->line-numbered-text (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((line-count (1+ (text/count input #\NewLine)))
         (line-number-length (1+ (floor (log line-count) (log 10))))
         (line-number-format-string (format nil "\~~~A,' D " line-number-length))
         (line-index 0)
         (elements nil)
         (output (labels ((write-element (element)
                            (push element elements)))
                   (text/map-split input #\NewLine
                                   (lambda (start-element-index start-character-index end-element-index end-character-index)
                                     (bind ((line-number (format nil line-number-format-string (1+ line-index)))
                                            (line (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                                       (write-element (make-text/string line-number :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/content/light* :fill-color *color/solarized/background/light*))
                                       (iter (for line-element-index :from 0)
                                             (for line-element :in-sequence (elements-of line))
                                             (for line-content = (content-of line-element))
                                             (write-element line-element))
                                       (write-element (make-text/string (string #\NewLine) :font *font/default* :font-color *color/default*))
                                       (incf line-index))))
                   (make-text/text (nreverse (rest elements))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               (make-iomap 'iomap/text->line-numbered-text
                                           :projection projection :recursion recursion
                                           :input input :input-reference `(the ,(form-type input) ,input-reference)
                                           :output output :output-reference `(the text/text ,output-reference)
                                           :line-number-length line-number-length)))))

;;;;;;
;;; Reader

(def reader text->line-numbered-text (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (bind ((input (input-of projection-iomap))
         (document (input-of document-iomap))
         (latest-gesture (first (gestures-of gesture-queue))))
    (merge-operations (text/read-operation document input latest-gesture)
                      (operation/read-backward operation projection-iomap document-iomap))))
