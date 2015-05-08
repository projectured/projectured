;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection write-stream-folding ()
  ())

;;;;;;
;;; Construction

(def function make-projection/write-stream-folding ()
  (make-projection 'write-stream-folding))

;;;;;;
;;; Construction

(def macro write-stream-folding ()
  '(make-projection/write-stream-folding))

;;;;;;
;;; Printer

(def printer write-stream-folding (projection recursion input input-reference)
  (bind ((output (as (if (typep input 'common-lisp/progn)
                         (iter (for element :in-sequence (body-of input))
                               (for element-iomap = (recurse-printer recursion element nil))
                               (for element-output = (output-of element-iomap))
                               (labels ((recurse (instance)
                                          (cond ((and (typep instance 'common-lisp/application)
                                                      (string= (name-of (operator-of instance)) "WRITE-STRING")
                                                      (typep (first-elt (arguments-of instance)) 'common-lisp/constant))
                                                 (collect (value-of (first-elt (arguments-of instance))) :into foldable-strings))
                                                ((typep instance 'common-lisp/progn)
                                                 (foreach #'recurse (body-of instance)))
                                                (t
                                                 (when foldable-strings
                                                   (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (apply 'string+ foldable-strings)))) :into result)
                                                   (setf foldable-strings nil))
                                                 (collect instance :into result)))))
                                 (recurse element-output))
                               (finally
                                (when foldable-strings
                                  (appendf result (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string) (list (make-common-lisp/constant (apply 'string+ foldable-strings)))))))
                                (return (if (length= result 1)
                                            (first-elt result)
                                            (make-common-lisp/progn result)))))
                         input))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader write-stream-folding (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
