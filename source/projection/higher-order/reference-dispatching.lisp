;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection reference-dispatching ()
  ((default-projection :type projection)
   (reference-projection-pairs :type list)))

;;;;;;
;;; Construction

(def function make-projection/reference-dispatching (default-projection reference-projection-pairs)
  (make-projection 'reference-dispatching
                   :default-projection default-projection
                   :reference-projection-pairs reference-projection-pairs))

;;;;;;
;;; Construction

(def macro reference-dispatching (default-projection &body reference-projection-pairs)
  `(make-projection/reference-dispatching
    ,default-projection
    (list ,@(iter (for (reference projection) :in reference-projection-pairs)
                  (collect `(list ',reference ,projection))))))
;;;;;;
;;; Printer

(def printer reference-dispatching (projection recursion input input-reference)
  (declare (ignore recursion))
  (iter (with typed-input-reference = (typed-reference (form-type input) input-reference))
        (with default-projection = (default-projection-of projection))
        (with reference-projection-pairs = (reference-projection-pairs-of projection))
        (for (reference reference-projection) :in-sequence reference-projection-pairs)
        (when (or (eq reference #t) (equal reference typed-input-reference))
          (return (call-printer reference-projection projection input input-reference)))
        (finally (return (call-printer default-projection projection input input-reference)))))

;;;;;;
;;; Reader

(def reader reference-dispatching (projection recursion input printer-iomap)
  (declare (ignore recursion))
  (merge-commands (bind ((command (iter (with input-reference = (input-reference-of printer-iomap))
                                        (with default-projection = (default-projection-of projection))
                                        (with reference-projection-pairs = (reference-projection-pairs-of projection))
                                        (for (reference reference-projection) :in-sequence reference-projection-pairs)
                                        (when (or (eq reference #t) (equal reference input-reference))
                                          (return (call-reader reference-projection projection input printer-iomap)))
                                        (finally (return (call-reader default-projection projection input printer-iomap))))))
                    (when (and command (operation-of command))
                      command))
                  (gesture-case (gesture-of input)
                    ((gesture/keyboard/key-press :sdl-key-c '(:shift :control))
                     :domain "Generic" :description "Toggles notation customization at the current selection"
                     :operation (make-operation/functional (lambda ()
                                                             (bind ((input-reference (input-reference-of printer-iomap))
                                                                    (reference-projection-pairs (reference-projection-pairs-of projection)))
                                                               (setf (reference-projection-pairs-of projection)
                                                                     (if (find input-reference reference-projection-pairs :key 'first :test 'equal)
                                                                         (remove input-reference reference-projection-pairs :key 'first :test 'equal)
                                                                         (list* (list input-reference (deep-copy (default-projection-of projection))) reference-projection-pairs))))))))
                  (make-command/nothing (gesture-of input))))
