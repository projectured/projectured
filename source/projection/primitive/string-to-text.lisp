;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection string->text ()
  ((font-provider :type function)
   (font-color-provider :type function)
   (fill-color-provider :type function)
   (line-color-provider :type function)))

;;;;;;
;;; Construction

(def function make-projection/string->text (&key font-provider font-color-provider fill-color-provider line-color-provider)
  (make-projection 'string->text
                   :font-provider font-provider
                   :font-color-provider font-color-provider
                   :fill-color-provider fill-color-provider
                   :line-color-provider line-color-provider))

(def function make-color-provider (color)
  (lambda (iomap reference)
    (declare (ignore iomap reference))
    color))

(def function make-font-provider (font)
  (lambda (iomap reference)
    (declare (ignore iomap reference))
    font))

;;;;;;
;;; Construction

(def macro string->text (&key font-provider font-color-provider fill-color-provider line-color-provider)
  `(make-projection/string->text :font-provider ,font-provider
                                 :font-color-provider ,font-color-provider
                                 :fill-color-provider ,fill-color-provider
                                 :line-color-provider ,line-color-provider))

;;;;;;
;;; Printer

(def printer string->text (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (stream (make-string-output-stream))
         (font *font/default*)
         (font-color *color/default*)
         (fill-color nil)
         (line-color nil)
         (input-offset 0)
         (output-index 0)
         (elements (flet ((next-text (force)
                            (bind ((content (get-output-stream-string stream)))
                              (when (or force (not (string= content "")))
                                (incf output-index)
                                (list (text/make-string content :font font :font-color font-color :fill-color fill-color :line-color line-color))))))
                     (iter (with font-provider = (font-provider-of projection))
                           (with font-color-provider = (font-color-provider-of projection))
                           (with fill-color-provider = (fill-color-provider-of projection))
                           (with line-color-provider = (line-color-provider-of projection))
                           (for character-index :from 0)
                           (for character :in-sequence input)
                           (for character-reference = `(the character (elt (the string ,input-reference) ,character-index)))
                           (for character-font = (or (when font-provider (funcall font-provider character-reference))
                                                     *font/default*))
                           (for character-font-color = (or (when font-color-provider (funcall font-color-provider character-reference))
                                                           *color/default*))
                           (for character-fill-color = (or (when fill-color-provider (funcall fill-color-provider character-reference))
                                                           nil))
                           (for character-line-color = (or (when line-color-provider (funcall line-color-provider character-reference))
                                                           nil))
                           (when (or (not (color= font-color character-font-color))
                                     (not (color=* fill-color character-fill-color))
                                     (not (color=* line-color character-line-color))
                                     (not (eq font character-font)))
                             (appending (next-text #f) :into texts)
                             (setf stream (make-string-output-stream))
                             (setf input-offset character-index)
                             (setf font character-font)
                             (setf font-color character-font-color)
                             (setf fill-color character-fill-color)
                             (setf line-color character-line-color))
                           (write-char character stream)
                           (finally
                            (return (if texts
                                        (append texts (next-text #f))
                                        (next-text #t)))))))
         (output (text/make-text elements)))
    (make-iomap/compound projection recursion input input-reference output
                          (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader string->text (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)
