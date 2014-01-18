;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; TODO: rename and move?
(def function operation/read-backward (operation projection-iomap)
  (labels ((recurse (child-operation)
             (cond ((typep child-operation 'operation/quit)
                    child-operation)
                   ((typep child-operation 'operation/save-document)
                    (make-instance 'operation/save-document :document (input-of projection-iomap) :filename (filename-of child-operation)))
                   ((typep child-operation 'operation/load-document)
                    (make-instance 'operation/load-document :document (input-of projection-iomap) :filename (filename-of child-operation)))
                   ((typep child-operation 'operation/export-document)
                    (make-instance 'operation/export-document :document (input-of projection-iomap) :filename (filename-of child-operation)))
                   ((typep child-operation 'operation/import-document)
                    (make-instance 'operation/import-document :document (input-of projection-iomap) :filename (filename-of child-operation)))
                   #+nil
                   ((typep child-operation 'operation/replace-selection)
                    (awhen (project-backward projection-iomap (selection-of child-operation))
                      (make-operation/replace-selection (input-of projection-iomap) it)))
                   #+nil
                   ((typep child-operation 'operation/sequence/replace-element-range)
                    (awhen (project-backward projection-iomap (target-of child-operation))
                      (make-operation/sequence/replace-element-range (input-of projection-iomap) it (replacement-of child-operation))))
                   #+nil
                   ((typep child-operation 'operation/number/replace-range)
                    (awhen (project-backward projection-iomap (target-of child-operation))
                      (make-operation/number/replace-range (input-of projection-iomap) it (replacement-of child-operation))))
                   ((typep child-operation 'operation/show-context-sensitive-help)
                    (make-instance 'operation/show-context-sensitive-help
                                   :commands (iter (for command :in (commands-of child-operation))
                                                   (for operation = (recurse (operation-of command)))
                                                   (when operation
                                                     (collect (make-instance 'command
                                                                             :gesture (gesture-of command)
                                                                             :domain (domain-of command)
                                                                             :description (description-of command)
                                                                             :operation operation))))))
                   ((typep child-operation 'operation/compound)
                    (bind ((child-operations (mapcar #'recurse (elements-of child-operation))))
                      (unless (some 'null child-operations)
                        (make-operation/compound child-operations)))))))
    (recurse operation)))
