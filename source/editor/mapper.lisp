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

;;;;;;
;;; API

(def function call-forward-mapper (projection recursion printer-iomap reference)
  (declare (ignore recursion))
  (or (funcall (forward-mapper-of projection) printer-iomap reference)
      (pattern-case reference
        (((the ?output-type (printer-output (the ?input-type document) ?projection ?recursion)) . ?rest)
         (bind ((input-type (document-type (input-of printer-iomap)))
                (output-type (document-type (output-of printer-iomap))))
           (when (and (eq projection ?projection)
                      ;; TODO: breaks nesting higher order projection
                      #+nil (eq recursion ?recursion)
                      (eq input-type ?input-type)
                      (eq output-type ?output-type))
             ?rest))))))

(def function call-backward-mapper (projection recursion printer-iomap reference)
  (or (funcall (backward-mapper-of projection) printer-iomap reference)
      (bind ((input-type (document-type (input-of printer-iomap)))
             (output-type (document-type (output-of printer-iomap))))
        (append `((the ,output-type (printer-output (the ,input-type document) ,projection ,recursion))) reference))))
