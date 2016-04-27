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
                (-printer-input- (input-of -printer-iomap-))
                (-printer-output- (output-of -printer-iomap-)))
           (declare (ignorable -projection- -recursion- -printer-input- -printer-output-))
           ,@forms))
       (setf (find-forward-mapper ',name) ',function-name))))

(def definer backward-mapper (name &body forms)
  (bind ((function-name (format-symbol :projectured "BACKWARD-MAPPER/~A" name)))
    `(progn
       (def function ,function-name (-printer-iomap- -reference-)
         (declare (ignorable -printer-iomap- -reference-))
         (bind ((-projection- (projection-of -printer-iomap-))
                (-recursion- (recursion-of -printer-iomap-))
                (-printer-input- (input-of -printer-iomap-))
                (-printer-output- (output-of -printer-iomap-)))
           (declare (ignorable -projection- -recursion- -printer-input- -printer-output-))
           ,@forms))
       (setf (find-backward-mapper ',name) ',function-name))))

;;;;;;
;;; API

(def function call-forward-mapper (printer-iomap input-reference)
  (bind ((projection (projection-of printer-iomap))
         #+nil(recursion (recursion-of printer-iomap))
         ((:values output-reference child-reference child-iomap transformer) (funcall (forward-mapper-of projection) printer-iomap input-reference)))
    (if (or output-reference child-iomap)
        (values output-reference child-reference child-iomap (or transformer 'identity))
        (reference-case input-reference
          (((the ?output-type (printer-output (the ?input-type document) ?projection ?recursion)) . ?rest)
           (bind ((input-type (document-type (input-of printer-iomap)))
                  (output-type (document-type (output-of printer-iomap))))
             (when (and ;; TODO: breaks recursion with type-dispatching, because type-dispatching doesn't map printer-output references for child projections
                        #+nil
                        (eq projection ?projection)
                        ;; TODO: breaks nesting higher order projection
                        #+nil (eq recursion ?recursion)
                        (eq input-type ?input-type)
                        (eq output-type ?output-type))
               (va* ?rest))))))))

(def function call-backward-mapper (printer-iomap output-reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (bind (((:values input-reference child-reference child-iomap) (funcall (backward-mapper-of projection) printer-iomap output-reference)))
      (if (or input-reference child-iomap)
          (values input-reference (va* child-reference) child-iomap)
          (bind ((input-type (document-type (input-of printer-iomap)))
                 (output-type (document-type (output-of printer-iomap))))
            (append-cc `((the ,output-type (printer-output (the ,input-type document) ,projection ,recursion))) output-reference))))))
