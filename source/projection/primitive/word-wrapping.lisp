;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) word-wrapping ()
  ((wrap-width :type number)))

;;;;;;
;;; Construction

(def (function e) make-projection/word-wrapping (&key wrap-width)
  (make-projection 'word-wrapping :wrap-width wrap-width))

;;;;;;
;;; Construction

(def (macro e) word-wrapping (&key wrap-width)
  `(make-projection/word-wrapping :wrap-width ,wrap-width))

;;;;;;
;;; Printer

(def printer word-wrapping (projection recursion input input-reference output-reference)
  (bind ((elements (iter (with x = 0)
                         (with elements = (elements-of input))
                         (with wrap-width = (wrap-width-of projection))
                         (for (values start-element-index start-character-index)
                              :initially (values 0 0)
                              :then (styled-string/find input end-element-index end-character-index (lambda (c) (not (whitespace? c)))))
                         (for whitespace-elements = (unless (first-iteration-p)
                                                      (elements-of (styled-string/substring input end-element-index end-character-index start-element-index start-character-index))))
                         (for whitespace-width = (iter (for element :in whitespace-elements)
                                                       (summing (2d-x (measure-text (content-of element) (font-of element))))))
                         (incf x whitespace-width)
                         (appending whitespace-elements)
                         (until (and (= start-element-index (length elements))
                                     (= start-character-index 0)))
                         (for (values end-element-index end-character-index) = (styled-string/find input start-element-index start-character-index 'whitespace?))
                         (for word-elements = (elements-of (styled-string/substring input start-element-index start-character-index end-element-index end-character-index)))
                         (for word-width = (iter (for element :in word-elements)
                                                 (summing (2d-x (measure-text (content-of element) (font-of element))))))
                         (when (> (+ x word-width) wrap-width)
                           (setf x 0)
                           (collect (make-styled-string/string (string #\NewLine))))
                         (incf x word-width)
                         (appending word-elements)
                         (until (and (= end-element-index (length elements))
                                     (= end-character-index 0)))))
         (output (make-styled-string/document elements)))
    (make-iomap/object projection recursion input input-reference output output-reference)))

;;;;;;
;;; Reader

(def reader word-wrapping (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
