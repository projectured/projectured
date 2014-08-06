;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sequence->text ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)
   (separator :type string)
   (indentation :type string)))

;;;;;;
;;; Construction

(def (function e) make-projection/sequence->text (&key opening-delimiter closing-delimiter separator indentation)
  (make-projection 'sequence->text
                   :opening-delimiter opening-delimiter
                   :closing-delimiter closing-delimiter
                   :separator separator
                   :indentation indentation))

;;;;;;
;;; Construction

(def (macro e) sequence->text (&key opening-delimiter closing-delimiter separator indentation)
  `(make-projection/sequence->text :opening-delimiter ,opening-delimiter
                                   :closing-delimiter ,closing-delimiter
                                   :separator ,separator
                                   :indentation ,indentation))

;;;;;;
;;; Printer

;; TODO: work with text internally
(def printer sequence->text (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (opening-delimiter (opening-delimiter-of projection))
         (closing-delimiter (closing-delimiter-of projection))
         (separator (separator-of projection))
         (indentation (indentation-of projection))
         (child-iomaps/object nil)
         (child-iomaps/string nil)
         (elements (prog1-bind elements nil
                     (labels ((write-character (character &optional (count 1))
                                (push (make-text/string (make-string count :element-type 'character :initial-element character) :font *font/default* :font-color *color/solarized/gray*) elements))
                              (write-element (element)
                                (push element elements))
                              (write-elements (text)
                                (iter (for element :in-sequence (elements-of text))
                                      (write-element element))))
                       (when opening-delimiter
                         (write-elements opening-delimiter))
                       (iter (for element :in-sequence input)
                             (for index :from 0)
                             (for element-reference = `(elt ,typed-input-reference ,index))
                             (for typed-element-reference = `(the ,(form-type element) ,element-reference))
                             (for previous-typed-element-reference :previous typed-element-reference)
                             (unless (first-iteration-p)
                               (when separator
                                 (write-elements separator))
                               (when (and indentation
                                          (string= separator "
"))
                                 (write-elements indentation)))
                             (bind ((iomap (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                                                ,@(typed-reference (form-type input) input-reference)))))
                               (iter (for line :in (text/split (output-of iomap) #\NewLine))
                                     (unless (first-iteration-p)
                                       (write-element (text/newline))
                                       (when indentation
                                         (write-elements indentation)))
                                     (foreach #'write-element (elements-of line)))))
                       (when closing-delimiter
                         (write-elements closing-delimiter)))))
         (output (make-text/text (nreverse elements))))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output))
                                 (nreverse child-iomaps/object)
                                 (sort child-iomaps/string '< :key 'output-offset-of)))))

;;;;;;
;;; Reader

(def reader sequence->text (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  input)
