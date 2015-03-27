;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection image/file->graphics/image ()
  ())

(def projection image/memory->graphics/image ()
  ())

;;;;;;
;;; Construction

(def function make-projection/image/file->graphics/image ()
  (make-projection 'image/file->graphics/image))

(def function make-projection/image/memory->graphics/image ()
  (make-projection 'image/memory->graphics/image))

;;;;;;
;;; Construction

(def macro image/file->graphics/image ()
  '(make-projection/image/file->graphics/image))

(def macro image/memory->graphics/image ()
  '(make-projection/image/memory->graphics/image))

;;;;;;
;;; Printer

(def printer image/file->graphics/image (projection recursion input input-reference)
  (bind ((output (make-graphics/image (make-2d 0 0) input)))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer image/memory->graphics/image (projection recursion input input-reference)
  (bind ((output (make-graphics/image (make-2d 0 0) input)))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader image/file->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader image/memory->graphics/image (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
