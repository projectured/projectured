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
;;; Construction

(def function make-command (gesture operation &key domain icon description)
  (assert domain)
;;  (assert (not (and operation (string= "Does nothing" description))))
  (make-instance 'command
                 :gesture gesture :operation operation
                 :domain domain :icon icon :description description))

(def function make-command/nothing (gesture)
  (make-command gesture nil :domain "Default" :description "Does nothing"))

(def function make-command/clone (command &optional operation)
  (assert (not (and operation (string= "Does nothing" (description-of command)))))
  (make-command (gesture-of command) operation :domain (domain-of command) :description (description-of command)))

;;;;;;
;;; API

(def method print-object ((instance command) stream)
  (print-unreadable-object (instance stream :type #t :identity #f)
    (princ (gesture-of instance) stream)
    (princ " " stream)
    (princ (operation-of instance) stream)))

;; TODO: rename?
(def macro gesture-case (gesture &body cases)
  (with-unique-names (gesture-variable)
    `(bind ((,gesture-variable ,gesture))
       (or (and (key-press? ,gesture-variable :key :sdl-key-h :modifier :control)
                (make-command ,gesture-variable
                              (make-instance 'operation/show-context-sensitive-help
                                             :commands (optional-list ,@(iter (for case :in cases)
                                                                              (collect `(bind ((operation ,(getf (rest case) :operation)))
                                                                                          (when operation
                                                                                            (make-instance 'command
                                                                                                           :gesture ,(first case)
                                                                                                           :domain ,(getf (rest case) :domain)
                                                                                                           :description ,(getf (rest case) :description)
                                                                                                           :operation operation)))))))
                              :domain "Default"
                              :description "Shows context sensitive help"))
           ,@(iter (for case :in cases)
                   (collect `(and (gesture= ,gesture-variable ,(first case))
                                  (bind (((:values operation processed?) ,(getf (rest case) :operation)))
                                    (when (or operation processed?)
                                      (make-instance 'command
                                                     :gesture ,gesture-variable
                                                     :domain ,(getf (rest case) :domain)
                                                     :description ,(getf (rest case) :description)
                                                     :operation operation))))))))))

(def function merge-commands (&rest commands)
  (reduce (lambda (command-1 command-2)
            (if (and command-1 command-2
                     (typep (operation-of command-1) 'operation/show-context-sensitive-help)
                     (typep (operation-of command-2) 'operation/show-context-sensitive-help))
                (make-command (gesture-of command-1)
                              (make-instance 'operation/show-context-sensitive-help
                                             :commands (append (commands-of (operation-of command-1))
                                                               (remove-if (lambda (command) (find (gesture-of command) (commands-of (operation-of command-1)) :key 'gesture-of :test 'gesture=))
                                                                          (commands-of (operation-of command-2)))))
                              :domain "Default"
                              :description "Shows context sensitive help")
                (or command-1 command-2)))
          commands))
