;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

;; TODO: rename these to ->tree/leaf
(def projection lisp-form/comment->tree/node ()
  ())

(def projection lisp-form/number->tree/leaf ()
  ())

(def projection lisp-form/symbol->tree/leaf ()
  ())

(def projection lisp-form/string->tree/leaf ()
  ())

(def projection lisp-form/list->tree/node ()
  ())

(def projection lisp-form/object->tree/leaf ()
  ())

(def projection lisp-form/top-level->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/lisp-form/comment->tree/node ()
  (make-projection 'lisp-form/comment->tree/node))

(def (function e) make-projection/lisp-form/number->tree/leaf ()
  (make-projection 'lisp-form/number->tree/leaf))

(def (function e) make-projection/lisp-form/symbol->tree/leaf ()
  (make-projection 'lisp-form/symbol->tree/leaf))

(def (function e) make-projection/lisp-form/string->tree/leaf ()
  (make-projection 'lisp-form/string->tree/leaf))

(def (function e) make-projection/lisp-form/list->tree/node ()
  (make-projection 'lisp-form/list->tree/node))

(def (function e) make-projection/lisp-form/object->tree/leaf ()
  (make-projection 'lisp-form/object->tree/leaf))

(def (function e) make-projection/lisp-form/top-level->tree/node ()
  (make-projection 'lisp-form/top-level->tree/node))

;;;;;;
;;; Construction

(def (macro e) lisp-form/comment->tree/node ()
  '(make-projection/lisp-form/comment->tree/node))

(def (macro e) lisp-form/number->tree/leaf ()
  '(make-projection/lisp-form/number->tree/leaf))

(def (macro e) lisp-form/symbol->tree/leaf ()
  '(make-projection/lisp-form/symbol->tree/leaf))

(def (macro e) lisp-form/string->tree/leaf ()
  '(make-projection/lisp-form/string->tree/leaf))

(def (macro e) lisp-form/list->tree/node ()
  '(make-projection/lisp-form/list->tree/node))

(def (macro e) lisp-form/object->tree/node ()
  '(make-projection/lisp-form/object->tree/leaf))

(def (macro e) lisp-form/top-level->tree/node ()
  '(make-projection/lisp-form/top-level->tree/node))

;;;;;;
;;; Printer

(def printer lisp-form/comment->tree/node (projection recursion input input-reference)
  (bind ((content (content-of input))
         ;; TODO:
         (output (make-tree/node (list (output-of (recurse-printer recursion content input-reference)))
                                 #+nil(make-text/string content :font *font/ubuntu/regular/18* :font-color *color/solarized/gray*)
                                 :opening-delimiter (text/text () (text/string ";; " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         nil
                         #+nil
                         (list (make-iomap/object projection recursion input `(the string (value-of ,typed-input-reference))
                                                  output `(the string ,output-reference))))))

(def printer lisp-form/number->tree/leaf (projection recursion input input-reference)
  (bind ((output-content (write-to-string (value-of input)))
         (output (tree/leaf () (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input input-reference output)))))

(def printer lisp-form/symbol->tree/leaf (projection recursion input input-reference)
  (bind ((output-content (string-downcase (value-of input)))
         (font-color (or (font-color-of input) *color/solarized/violet*))
         (output (tree/leaf (:opening-delimiter (when (keywordp (value-of input))
                                                  (text/text () (text/string ":" :font *font/ubuntu/monospace/regular/18* :font-color font-color))))
                   (text/text ()
                     (text/string output-content :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color font-color)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input input-reference output)))))

(def printer lisp-form/string->tree/leaf (projection recursion input input-reference)
  (bind ((value (value-of input))
         (output-content value)
         (output (tree/leaf (:opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                             :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                   (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input `(the string (value-of ,typed-input-reference))
                                                  output `(the string ,output-reference))))))

(def printer lisp-form/list->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (deep-list (find-if (of-type 'lisp-form/list) (elements-of input)))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in (elements-of input))
                                       (for iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,index)
                                                                       (the sequence (elements-of (the document list-form/list)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                                       (for element-output = (output-of iomap))
                                       (push iomap child-iomaps)
                                       (setf (indentation-of element-output)
                                             (typecase element
                                               (lisp-form/base (indentation-of element))
                                               (t (when (and deep-list (not (first-iteration-p)))
                                                    2))))
                                       (collect element-output))
                                 :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer lisp-form/object->tree/leaf (projection recursion input input-reference)
  (bind ((output-content (write-to-string input))
         (output (tree/leaf () (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input input-reference output)))))

(def printer lisp-form/top-level->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in (elements-of input))
                                       (for iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence document) ,index)
                                                                       (the sequence (elements-of (the lisp-form/top-level document)))
                                                                       ,@(typed-reference (form-type input) input-reference))))
                                       (for element-output = (output-of iomap))
                                       (push iomap child-iomaps)
                                       (setf (indentation-of element-output)
                                             (typecase element
                                               (lisp-form/base (indentation-of element))
                                               (t 0)))
                                       (collect element-output))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :indentation 2)))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader lisp-form/comment->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/number->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/symbol->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/list->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/object->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader lisp-form/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
