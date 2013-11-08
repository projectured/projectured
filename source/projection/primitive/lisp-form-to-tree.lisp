;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

;; TODO: rename these to ->tree/leaf
(def (projection e) lisp-form/comment->string ()
  ())

(def (projection e) lisp-form/number->string ()
  ())

(def (projection e) lisp-form/symbol->string ()
  ())

(def (projection e) lisp-form/string->string ()
  ())

(def (projection e) lisp-form/list->tree/node ()
  ())

(def (projection e) lisp-form/object->string ()
  ())

(def (projection e) lisp-form/top-level->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/lisp-form/comment->string ()
  (make-projection 'lisp-form/comment->string))

(def (function e) make-projection/lisp-form/number->string ()
  (make-projection 'lisp-form/number->string))

(def (function e) make-projection/lisp-form/symbol->string ()
  (make-projection 'lisp-form/symbol->string))

(def (function e) make-projection/lisp-form/string->string ()
  (make-projection 'lisp-form/string->string))

(def (function e) make-projection/lisp-form/list->tree/node ()
  (make-projection 'lisp-form/list->tree/node))

(def (function e) make-projection/lisp-form/object->string ()
  (make-projection 'lisp-form/object->string))

(def (function e) make-projection/lisp-form/top-level->tree/node ()
  (make-projection 'lisp-form/top-level->tree/node))

;;;;;;
;;; Construction

(def (macro e) lisp-form/comment->string ()
  '(make-projection/lisp-form/comment->string))

(def (macro e) lisp-form/number->string ()
  '(make-projection/lisp-form/number->string))

(def (macro e) lisp-form/symbol->string ()
  '(make-projection/lisp-form/symbol->string))

(def (macro e) lisp-form/string->string ()
  '(make-projection/lisp-form/string->string))

(def (macro e) lisp-form/list->tree/node ()
  '(make-projection/lisp-form/list->tree/node))

(def (macro e) lisp-form/object->tree/node ()
  '(make-projection/lisp-form/object->string))

(def (macro e) lisp-form/top-level->tree/node ()
  '(make-projection/lisp-form/top-level->tree/node))

;;;;;;
;;; Printer

(def printer lisp-form/comment->string (projection recursion iomap input input-reference output-reference)
  (bind ((content (content-of input))
         ;; TODO:
         (output (make-tree/node (list (output-of (recurse-printer recursion iomap content input-reference output-reference)))
                                 #+nil(make-text/string content :font *font/ubuntu/regular/18* :font-color *color/solarized/gray*)
                                 :opening-delimiter (make-text/string ";; " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          nil
                          #+nil
                          (list (make-iomap/string* content `(the string (value-of ,typed-input-reference)) 0
                                                    content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length content))
                                (make-iomap/object* projection recursion input `(the string (value-of ,typed-input-reference))
                                                    output `(the string ,output-reference))))))

(def printer lisp-form/number->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (write-to-string (value-of input)))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (write-to-string (the number (value-of ,typed-input-reference)))) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer lisp-form/symbol->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (string-downcase (value-of input)))
         (font-color (or (font-color-of input) *color/solarized/violet*))
         (output (make-tree/leaf (make-text/string output-content :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color font-color)
                                 :opening-delimiter (when (keywordp (value-of input))
                                                      (make-text/string ":" :font *font/ubuntu/monospace/regular/18* :font-color font-color)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (string-downcase (the symbol (value-of ,typed-input-reference)))) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer lisp-form/string->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (value-of input))
         (output-content value)
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                 :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/string* value `(the string (value-of ,typed-input-reference)) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length value))
                                (make-iomap/object* projection recursion input `(the string (value-of ,typed-input-reference))
                                                    output `(the string ,output-reference))))))

(def printer lisp-form/list->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (deep-list (find-if (of-type 'lisp-form/list) (elements-of input)))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in (elements-of input))
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (for element-output = (output-of iomap))
                                       (push iomap child-iomaps)
                                       (setf (indentation-of element-output)
                                             (typecase element
                                               (lisp-form/base (indentation-of element))
                                               (t (when (and deep-list (not (first-iteration-p)))
                                                    2))))
                                       (collect element-output))
                                 :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer lisp-form/object->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (write-to-string input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (write-to-string (value-of ,typed-input-reference))) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer lisp-form/top-level->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in (elements-of input))
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (for element-output = (output-of iomap))
                                       (push iomap child-iomaps)
                                       (setf (indentation-of element-output)
                                             (typecase element
                                               (lisp-form/base (indentation-of element))
                                               (t 0)))
                                       (collect element-output))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :indentation 2)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader lisp-form/comment->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/number->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/symbol->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/string->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/list->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/object->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader lisp-form/top-level->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
