;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def (definer :available-flags "e") iomap (name supers slots &rest options)
  (if *use-computed-class*
      `(def computed-class* ,name ,supers
         ,(iter (for slot :in slots)
                (collect (append slot (list :computed-in 'projectured))))
         ,@options)
      `(def class* ,name ,supers ,slots ,@options)))

(def iomap iomap ()
  ((projection :type projection)
   (recursion :type projection)
   (input :type t)
   (output :type t)
   (reference-applier :type function)
   (forward-mapper :type function)
   (backward-mapper :type function))
  (:documentation "An IOMAP provides a bidirectional mapping between INPUT and OUTPUT."))

(def (function e) make-iomap (type &rest args &key reference-applier forward-mapper backward-mapper &allow-other-keys)
  (apply #'make-instance type
;;         :reference-applier (or reference-applier (find-reference-applier type))
;;         :forward-mapper (or forward-mapper (find-forward-mapper type))
;;         :backward-mapper (or backward-mapper (find-backward-mapper type))
         args))

;;;;;;
;;; Forward mapper

(def (namespace e) forward-mapper)

(def (definer e) forward-mapper (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "FORWARD-MAPPER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-forward-mapper ',name) ',function-name))))

(def (function e) map-forward (iomap input-reference function)
  (assert (every (lambda (element) (eq 'the (first element))) input-reference))
  (error "Obsolete")
  (funcall (forward-mapper-of iomap) iomap input-reference function))

;; TODO: delme
(def (function e) project-forward (iomap input-reference)
  (error "Obsolete")
  (bind ((result-output-reference nil))
    (map-forward iomap input-reference
                 (lambda (iomap output-reference)
                   (declare (ignore iomap))
                   (setf result-output-reference output-reference)))
    result-output-reference))

;;;;;;
;;; Backward mapper

(def (namespace e) backward-mapper)

(def (definer e) backward-mapper (name arguments &body forms)
  (bind ((function-name (format-symbol (symbol-package name) "BACKWARD-MAPPER/~A" name)))
    `(progn
       (def function ,function-name ,arguments ,@forms)
       (setf (find-backward-mapper ',name) ',function-name))))

(def (function e) map-backward (iomap output-reference function)
  (error "Obsolete")
  (funcall (backward-mapper-of iomap) iomap output-reference function))

;; TODO: delme
(def (function e) project-backward (iomap output-reference)
  (error "Obsolete")
  (bind ((result-input-reference nil))
    (map-backward iomap output-reference
                  (lambda (iomap input-reference)
                    (declare (ignore iomap))
                    (setf result-input-reference input-reference)))
    result-input-reference))
