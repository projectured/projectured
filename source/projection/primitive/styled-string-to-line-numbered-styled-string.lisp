;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) styled-string->line-numbered-styled-string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/styled-string->line-numbered-styled-string ()
  (make-projection 'styled-string->line-numbered-styled-string))

;;;;;;
;;; Construction

(def (macro e) styled-string->line-numbered-styled-string ()
  '(make-projection/styled-string->line-numbered-styled-string))

;;;;;;
;;; Printer

(def printer styled-string->line-numbered-styled-string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (line-count (1+ (text/count input #\NewLine)))
         (line-number-length (1+ (floor (log line-count) (log 10))))
         (element-index 0)
         (string-position 0)
         (elements nil)
         (format-string (format nil "\~~~A,' D " line-number-length))
         (input-offset 0)
         (line-index 0)
         (output (labels ((write-element (element)
                            (push element elements)
                            (incf element-index)
                            (typecase element
                              (text/string
                               (incf string-position (length (content-of element)))))))
                   (text/map-split input #\NewLine
                                   (lambda (start-element-index start-character-index end-element-index end-character-index)
                                     (bind ((line-number (format nil format-string (1+ line-index)))
                                            (line (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                                       (push (make-iomap/string line-number `(line-number ,typed-input-reference ,line-index) 0
                                                                line-number `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                (length line-number))
                                             child-iomaps)
                                       (write-element (make-text/string line-number :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/content/light* :fill-color *color/solarized/background/light*))
                                       (iter (for line-element-index :from 0)
                                             (for line-element :in-sequence (elements-of line))
                                             (typecase line-element
                                               (text/string
                                                (for line-content = (content-of line-element))
                                                (push (make-iomap/string line-content `(content-of (the text/string (elt (the list (elements-of ,typed-input-reference)) ,(+ start-element-index line-element-index)))) 0
                                                                         line-content `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                         (length line-content))
                                                      child-iomaps)))
                                             (write-element line-element))
                                       (write-element (make-text/string (string #\NewLine) :font *font/default* :font-color *color/default*))
                                       (incf input-offset (1+ (text/length line)))
                                       (incf line-index))))
                   (make-text/text (nreverse elements)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader styled-string->line-numbered-styled-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
