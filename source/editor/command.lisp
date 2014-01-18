;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Data structure

(def class* command ()
  ((domain :type string)
   (icon :type t)
   (description :type string)
   (gesture :type gesture)
   (operation :type operation)))

;;;;;;
;;; API

;; TODO: rename?
(def macro gesture-case (gesture &body cases)
  (with-unique-names (gesture-variable)
    `(bind ((,gesture-variable ,gesture))
       (or (and (key-press? ,gesture-variable :key :sdl-key-h :modifier :control)
                (make-instance 'operation/show-context-sensitive-help
                               :commands (optional-list ,@(iter (for case :in cases)
                                                                (for operation = (getf (rest case) :operation))
                                                                (collect `(bind ((operation ,(getf (rest case) :operation)))
                                                                            (when operation
                                                                              (make-instance 'command
                                                                                             :gesture ,(first case)
                                                                                             :domain ,(getf (rest case) :domain)
                                                                                             ;; TODO: make consistent
                                                                                             :description ,(getf (rest case) :help)
                                                                                             :operation ,operation))))))))
           ,@(iter (for case :in cases)
                   (collect `(and (gesture= ,gesture-variable ,(first case))
                                  ,(getf (rest case) :operation))))))))

(def function merge-operations (operation-1 operation-2)
  (cond ((and (typep operation-1 'operation/show-context-sensitive-help)
              (typep operation-2 'operation/show-context-sensitive-help))
         (make-instance 'operation/show-context-sensitive-help
                        :commands (append (commands-of operation-1)
                                          (set-difference (commands-of operation-2) (commands-of operation-1) :key 'gesture-of :test 'gesture=))))
        (t
         (or operation-1 operation-2))))
