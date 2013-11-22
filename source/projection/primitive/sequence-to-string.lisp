;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection sequence->string ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)
   (separator :type string)
   (indentation :type string)))

;;;;;;
;;; Construction

(def (function e) make-projection/sequence->string (&key (opening-delimiter "(") (closing-delimiter ")") (separator "
") (indentation ""))
  (make-projection 'sequence->string
                   :opening-delimiter opening-delimiter
                   :closing-delimiter closing-delimiter
                   :separator separator
                   :indentation indentation))

;;;;;;
;;; Construction

(def (macro e) sequence->string (&key (opening-delimiter "(") (closing-delimiter ")") (separator "
") (indentation " "))
  `(make-projection/sequence->string :opening-delimiter ,opening-delimiter
                                     :closing-delimiter ,closing-delimiter
                                     :separator ,separator
                                     :indentation ,indentation))

;;;;;;
;;; Printer

(def printer sequence->string (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (opening-delimiter (opening-delimiter-of projection))
         (closing-delimiter (closing-delimiter-of projection))
         (separator (separator-of projection))
         (indentation (indentation-of projection))
         (child-iomaps/object nil)
         (child-iomaps/string nil)
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (push (make-iomap/string opening-delimiter `(opening-delimiter ,typed-input-reference) 0
                                               output output-reference (file-position stream)
                                               (length opening-delimiter))
                            child-iomaps/string)
                      (write-string opening-delimiter stream)
                      (iter (for element :in-sequence input)
                            (for index :from 0)
                            (for element-reference = `(elt ,typed-input-reference ,index))
                            (for typed-element-reference = `(the ,(form-type element) ,element-reference))
                            (for previous-typed-element-reference :previous typed-element-reference)
                            (unless (first-iteration-p)
                              (push (make-iomap/string separator `(separator ,previous-typed-element-reference ,typed-element-reference) 0
                                                       output output-reference (file-position stream)
                                                       (length separator))
                                    child-iomaps/string)
                              (write-string separator stream)
                              (when (string= separator "
")
                                (push (make-iomap/string indentation `(indentation ,typed-element-reference) 0
                                                         output output-reference (file-position stream)
                                                         (length indentation))
                                      child-iomaps/string)
                                (write-string indentation stream)))
                            (if (stringp element)
                                (progn
                                  (push (make-iomap/string element element-reference 0
                                                           output output-reference (file-position stream)
                                                           (length element))
                                        child-iomaps/string)
                                  (push (make-iomap/object projection recursion element element-reference output nil) child-iomaps/object)
                                  (write-string element stream))
                                (bind ((iomap (recurse-printer recursion iomap element `(elt ,typed-input-reference ,index) output-reference)))
                                  (labels ((push-iomaps (iomap)
                                             (etypecase iomap
                                               (iomap/string
                                                (incf (output-offset-of iomap)
                                                      (+ (file-position stream)
                                                         (* (length indentation)
                                                            (funcall 'count #\NewLine (output-of iomap) :end (output-offset-of iomap)))))
                                                (setf (output-of iomap) output)
                                                (push iomap child-iomaps/string))
                                               (iomap/object
                                                (setf (output-of iomap) output)
                                                (setf (output-reference-of iomap) nil)
                                                (push iomap child-iomaps/object))
                                               (iomap/compound
                                                (iter (for child-iomap :in (child-iomaps-of iomap))
                                                      (push-iomaps child-iomap)))
                                               (iomap (values)))))
                                    (push-iomaps iomap))
                                  (iter (for line :in (split-sequence #\NewLine (output-of iomap)))
                                        (unless (first-iteration-p)
                                          (terpri stream)
                                          (push (make-iomap/string indentation `(indentation ,typed-element-reference) 0
                                                                   output output-reference (file-position stream)
                                                                   (length indentation))
                                                child-iomaps/string)
                                          (write-string indentation stream))
                                        (write-string line stream)))))
                      (push (make-iomap/string closing-delimiter `(closing-delimiter ,typed-input-reference) 0
                                               output output-reference (file-position stream)
                                               (length closing-delimiter))
                            child-iomaps/string)
                      (write-string closing-delimiter stream))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference))
                                  (nreverse child-iomaps/object)
                                  (sort child-iomaps/string '< :key 'output-offset-of)))))

;;;;;;
;;; Reader

(def reader sequence->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
