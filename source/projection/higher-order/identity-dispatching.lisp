;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection identity-dispatching ()
  ((default-projection :type projection)
   (identity-projection-map :type hash-table)))

;;;;;;
;;; Construction

(def function make-projection/identity-dispatching (default-projection)
  (make-projection 'identity-dispatching
                   :default-projection default-projection
                   :identity-projection-map (make-hash-table)))

;;;;;;
;;; Construction

(def macro identity-dispatching (&body default-projection)
  `(make-projection/identity-dispatching ,(first default-projection)))

;;;;;;
;;; Printer

(def printer identity-dispatching ()
  (bind ((default-projection (default-projection-of -projection-))
         (identity-projection (gethash -input- (identity-projection-map-of -projection-))))
    (call-printer (or identity-projection default-projection) -projection- -input- -input-reference-)))

;;;;;;
;;; Reader

(def reader identity-dispatching ()
  (merge-commands (bind ((command (bind ((default-projection (default-projection-of -projection-))
                                         (identity-projection (gethash -printer-input- (identity-projection-map-of -projection-))))
                                    (call-reader (or identity-projection default-projection) -projection- -input- -printer-iomap-))))
                    (when (and command (operation-of command))
                      command))
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-c '(:shift :control))
                     :domain "Generic" :description "Toggles notation customization at the current selection"
                     :operation (make-operation/functional (lambda ()
                                                             (bind ((identity-projection-map (identity-projection-map-of -projection-)))
                                                               (if (gethash -printer-input- identity-projection-map)
                                                                   (remhash -printer-input- identity-projection-map)
                                                                   (setf (gethash -printer-input- identity-projection-map) (deep-copy (default-projection-of -projection-))))
                                                               (invalidate-computed-slot -projection- 'identity-projection-map))))))
                  (make-nothing-command -gesture-)))
