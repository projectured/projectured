;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
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

;; KLUDGE: to workaround generated intermediate projections in nesting projection
(def function xxx-equal (instance-1 instance-2)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (cond ((and (consp instance-1) (consp instance-2))
         (and (xxx-equal (car instance-1) (car instance-2))
              (xxx-equal (cdr instance-1) (cdr instance-2))))
        ((and (typep instance-1 'projection) (typep instance-2 'projection))
         #t)
        (t (equal instance-1 instance-2))))

;;;;;;
;;; Printer

(def printer reference-dispatching ()
  (iter (with typed-input-reference = (reverse (typed-reference (document-type -input-) -input-reference-)))
        (with default-projection = (default-projection-of -projection-))
        (with reference-projection-pairs = (reference-projection-pairs-of -projection-))
        (for (reference reference-projection) :in-sequence reference-projection-pairs)
        (when (or (eq reference #t) (xxx-equal reference typed-input-reference))
          (return (call-printer reference-projection -projection- -input- -input-reference-)))
        (finally (return (call-printer default-projection -projection- -input- -input-reference-)))))

;;;;;;
;;; Reader

(def reader reference-dispatching ()
  (merge-commands (bind ((command (iter (with input-reference = (reverse (input-reference-of -printer-iomap-)))
                                        (with default-projection = (default-projection-of -projection-))
                                        (with reference-projection-pairs = (reference-projection-pairs-of -projection-))
                                        (for (reference reference-projection) :in-sequence reference-projection-pairs)
                                        (when (or (eq reference #t) (xxx-equal reference input-reference))
                                          (return (call-reader reference-projection -projection- -input- -printer-iomap-)))
                                        (finally (return (call-reader default-projection -projection- -input- -printer-iomap-))))))
                    (when (and command (operation-of command))
                      command))
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-c '(:shift :control))
                     :domain "Generic" :description "Toggles notation customization at the current selection"
                     :operation (make-operation/functional (lambda ()
                                                             (bind ((input-reference (reverse (input-reference-of -printer-iomap-)))
                                                                    (reference-projection-pairs (reference-projection-pairs-of -projection-)))
                                                               (setf (reference-projection-pairs-of -projection-)
                                                                     (if (find input-reference reference-projection-pairs :key 'first :test 'xxx-equal)
                                                                         (remove input-reference reference-projection-pairs :key 'first :test 'xxx-equal)
                                                                         (list* (list input-reference (deep-copy (default-projection-of -projection-))) reference-projection-pairs))))))))
                  (make-nothing-command -gesture-)))
