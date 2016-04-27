;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection error-handling ()
  ((default-projection :type projection)
   (error-projection :type projection)))

;;;;;;
;;; Construction

(def function make-projection/error-handling (default-projection error-projection)
  (make-projection 'error-handling :default-projection default-projection :error-projection error-projection))

;;;;;;
;;; Construction

(def macro error-handling (default-projection error-projection)
  `(make-projection/error-handling ,default-projection ,error-projection))

;;;;;;
;;; Printer

(def printer error-handling ()
  (bind ((content-iomap (as (call-printer (default-projection-of -projection-) -recursion- -input- -input-reference-)))
         (output (as* (:handler #t)
                   (block nil
                     (handler-bind ((error (lambda (error)
                                             (return
                                               (output-of (call-printer (error-projection-of -projection-) -recursion- (cons -input- error) nil))))))
                       (output-of (va content-iomap)))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

;;;;;;
;;; Reader

(def reader error-handling ()
  (call-reader (default-projection-of -projection-) -recursion- -input- (content-iomap-of -printer-iomap-)))
