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

(def (function e) make-projection/sequence->text (&key (opening-delimiter "(") (closing-delimiter ")") (separator "
") (indentation ""))
  (make-projection 'sequence->text
                   :opening-delimiter opening-delimiter
                   :closing-delimiter closing-delimiter
                   :separator separator
                   :indentation indentation))

;;;;;;
;;; Construction

(def (macro e) sequence->text (&key (opening-delimiter "(") (closing-delimiter ")") (separator "
") (indentation " "))
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
         (output (text/text ()
                   (text/string
                    (with-output-to-string (stream)
                      #+nil
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
                              #+nil
                              (push (make-iomap/string separator `(separator ,previous-typed-element-reference ,typed-element-reference) 0
                                                       output output-reference (file-position stream)
                                                       (length separator))
                                    child-iomaps/string)
                              (write-string separator stream)
                              (when (string= separator "
")
                                #+nil
                                (push (make-iomap/string indentation `(indentation ,typed-element-reference) 0
                                                         output output-reference (file-position stream)
                                                         (length indentation))
                                      child-iomaps/string)
                                (write-string indentation stream)))
                            (if (stringp element)
                                (progn
                                  #+nil
                                  (push (make-iomap/string element element-reference 0
                                                           output output-reference (file-position stream)
                                                           (length element))
                                        child-iomaps/string)
                                  #+nil
                                  (push (make-iomap/object projection recursion element element-reference output nil) child-iomaps/object)
                                  (write-string element stream))
                                (bind ((iomap (recurse-printer recursion element `((elt (the list document) ,index)
                                                                                   ,@(typed-reference (form-type input) input-reference)))))
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
                                    #+nil
                                    (push-iomaps iomap))
                                  (iter (for line :in (text/split (output-of iomap) #\NewLine))
                                        (unless (first-iteration-p)
                                          (terpri stream)
                                          #+nil
                                          (push (make-iomap/string indentation `(indentation ,typed-element-reference) 0
                                                                   output output-reference (file-position stream)
                                                                   (length indentation))
                                                child-iomaps/string)
                                          (write-string indentation stream))
                                        (write-string (text/as-string line) stream)))))
                      #+nil
                      (push (make-iomap/string closing-delimiter `(closing-delimiter ,typed-input-reference) 0
                                               output output-reference (file-position stream)
                                               (length closing-delimiter))
                            child-iomaps/string)
                      (write-string closing-delimiter stream))))))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output nil))
                                 (nreverse child-iomaps/object)
                                 (sort child-iomaps/string '< :key 'output-offset-of)))))

;;;;;;
;;; Reader

(def reader sequence->text (projection recursion projection-iomap gesture-queue operation)
  (declare (ignore projection recursion operation))
  (document/read-operation (input-of projection-iomap) (first (gestures-of gesture-queue))))
