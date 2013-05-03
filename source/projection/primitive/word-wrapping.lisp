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

(def printer word-wrapping (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  ;; TODO: child-iomaps
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (elements (iter (with x = 0)
                         (with output-index = 0)
                         (with elements = (elements-of input))
                         (with wrap-width = (wrap-width-of projection))
                         (for (values start-element-index start-character-index)
                              :initially (values 0 0)
                              :then (text/find input end-element-index end-character-index (lambda (c) (not (whitespace? c)))))
                         (for whitespace-elements = (unless (first-iteration-p)
                                                      (elements-of (text/substring input end-element-index end-character-index start-element-index start-character-index))))
                         (for whitespace-width = (iter (with sum = 0) (for element :in whitespace-elements)
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
                         (iter (for index :from 0)
                               (for element :in-sequence whitespace-elements)
                               (typecase element
                                 (text/string
                                  (for content = (content-of element))
                                  (push (make-iomap/string content `(content-of (the text/string (elt (the list (elements-of ,typed-input-reference)) ,(+ end-element-index index))))
                                                           (if (zerop index) end-character-index 0)
                                                           content `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,(+ output-index index)))) 0
                                                           (length content))
                                        child-iomaps))))
                         (incf output-index (length whitespace-elements))
                         (appending whitespace-elements)
                         (until (and (= start-element-index (length elements))
                                     (= start-character-index 0)))
                         (for (values end-element-index end-character-index) = (text/find input start-element-index start-character-index 'whitespace?))
                         (for word-elements = (elements-of (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                         (for word-width = (iter (for element :in word-elements)
                                                 (summing
                                                  (typecase element
                                                    (text/string (2d-x (measure-text (content-of element) (font-of element))))
                                                    ;; KLUDGE:
                                                    (t 100)))))
                         (incf x word-width)
                         (when (> x wrap-width)
                           (setf x word-width)
                           (incf output-index)
                           (collect (make-text/string (string #\NewLine) :font *font/default* :font-color *color/default*)))
                         (iter (for index :from 0)
                               (for element :in-sequence word-elements)
                               (typecase element
                                 (text/string
                                  (for content = (content-of element))
                                  (push (make-iomap/string content `(content-of (the text/string (elt (the list (elements-of ,typed-input-reference)) ,(+ start-element-index index))))
                                                           (if (zerop index) start-character-index 0)
                                                           content `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,(+ output-index index)))) 0
                                                           (length content))
                                        child-iomaps))))
                         (incf output-index (length word-elements))
                         (appending word-elements)
                         (until (and (= end-element-index (length elements))
                                     (= end-character-index 0)))))
         (output (make-text/text elements)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader word-wrapping (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
