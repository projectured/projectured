;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Definition

(def namespace forward-mapper)

(def namespace backward-mapper)

(def definer forward-mapper (name &body forms)
  (bind ((function-name (format-symbol :projectured "FORWARD-MAPPER/~A" name)))
    `(progn
       (def function ,function-name (-printer-iomap- -reference-)
         (declare (ignorable -printer-iomap- -reference-))
         (bind ((-projection- (projection-of -printer-iomap-))
                (-recursion- (recursion-of -printer-iomap-))
                (-printer-input- (input-of -printer-iomap-)))
           (declare (ignorable -projection- -recursion- -printer-input-))
           ,@forms))
       (setf (find-forward-mapper ',name) ',function-name))))

(def definer backward-mapper (name &body forms)
  (bind ((function-name (format-symbol :projectured "BACKWARD-MAPPER/~A" name)))
    `(progn
       (def function ,function-name (-printer-iomap- -reference-)
         (declare (ignorable -printer-iomap- -reference-))
         (bind ((-projection- (projection-of -printer-iomap-))
                (-recursion- (recursion-of -printer-iomap-))
                (-printer-input- (input-of -printer-iomap-)))
           (declare (ignorable -projection- -recursion- -printer-input-))
           ,@forms))
       (setf (find-backward-mapper ',name) ',function-name))))
