;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection common-lisp/constant-form->lisp-form/string ()
  ())

(def projection common-lisp/variable-reference-form->lisp-form/string ()
  ())

(def projection common-lisp/if-form->lisp-form/list ()
  ())

(def projection common-lisp/the-form->lisp-form/list ()
  ())

(def projection common-lisp/progn-form->lisp-form/list ()
  ())

(def projection common-lisp/lexical-variable-binding-form->lisp-form/list ()
  ())

(def projection common-lisp/let-form->lisp-form/list ()
  ())

(def projection common-lisp/application-form->lisp-form/list ()
  ())

(def projection common-lisp/function-definition-form->lisp-form/list ()
  ())

(def projection common-lisp/lambda-function-form->lisp-form/list ()
  ())

(def projection common-lisp/function-argument-form->lisp-form/string ()
  ())

(def projection common-lisp/comment->lisp-form/comment ()
  ())

(def projection common-lisp/top-level->lisp-form/top-level ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/common-lisp/constant-form->lisp-form/string ()
  (make-projection 'common-lisp/constant-form->lisp-form/string))

(def (function e) make-projection/common-lisp/variable-reference-form->lisp-form/string ()
  (make-projection 'common-lisp/variable-reference-form->lisp-form/string))

(def (function e) make-projection/common-lisp/if-form->lisp-form/list ()
  (make-projection 'common-lisp/if-form->lisp-form/list))

(def (function e) make-projection/common-lisp/the-form->lisp-form/list ()
  (make-projection 'common-lisp/the-form->lisp-form/list))

(def (function e) make-projection/common-lisp/progn-form->lisp-form/list ()
  (make-projection 'common-lisp/progn-form->lisp-form/list))

(def (function e) make-projection/common-lisp/lexical-variable-binding-form->lisp-form/list ()
  (make-projection 'common-lisp/lexical-variable-binding-form->lisp-form/list))

(def (function e) make-projection/common-lisp/let-form->lisp-form/list ()
  (make-projection 'common-lisp/let-form->lisp-form/list))

(def (function e) make-projection/common-lisp/application-form->lisp-form/list ()
  (make-projection 'common-lisp/application-form->lisp-form/list))

(def (function e) make-projection/common-lisp/function-definition-form->lisp-form/list ()
  (make-projection 'common-lisp/function-definition-form->lisp-form/list))

(def (function e) make-projection/common-lisp/lambda-function-form->lisp-form/list ()
  (make-projection 'common-lisp/lambda-function-form->lisp-form/list))

(def (function e) make-projection/common-lisp/function-argument-form->lisp-form/string ()
  (make-projection 'common-lisp/function-argument-form->lisp-form/string))

(def (function e) make-projection/common-lisp/comment->lisp-form/comment ()
  (make-projection 'common-lisp/comment->lisp-form/comment))

(def (function e) make-projection/common-lisp/top-level->lisp-form/top-level ()
  (make-projection 'common-lisp/top-level->lisp-form/top-level))

;;;;;;
;;; Construction

(def (macro e) common-lisp/constant-form->lisp-form/string ()
  '(make-projection/common-lisp/constant-form->lisp-form/string))

(def (macro e) common-lisp/variable-reference-form->lisp-form/string ()
  '(make-projection/common-lisp/variable-reference-form->lisp-form/string))

(def (macro e) common-lisp/if-form->lisp-form/list ()
  '(make-projection/common-lisp/if-form->lisp-form/list))

(def (macro e) common-lisp/the-form->lisp-form/list ()
  '(make-projection/common-lisp/the-form->lisp-form/list))

(def (macro e) common-lisp/progn-form->lisp-form/list ()
  '(make-projection/common-lisp/progn-form->lisp-form/list))

(def (macro e) common-lisp/lexical-variable-binding-form->lisp-form/list ()
  '(make-projection/common-lisp/lexical-variable-binding-form->lisp-form/list))

(def (macro e) common-lisp/let-form->lisp-form/list ()
  '(make-projection/common-lisp/let-form->lisp-form/list))

(def (macro e) common-lisp/application-form->lisp-form/list ()
  '(make-projection/common-lisp/application-form->lisp-form/list))

(def (macro e) common-lisp/function-definition-form->lisp-form/list ()
  '(make-projection/common-lisp/function-definition-form->lisp-form/list))

(def (macro e) common-lisp/lambda-function-form->lisp-form/list ()
  '(make-projection/common-lisp/lambda-function-form->lisp-form/list))

(def (macro e) common-lisp/function-argument-form->lisp-form/string ()
  '(make-projection/common-lisp/function-argument-form->lisp-form/string))

(def (macro e) common-lisp/comment->lisp-form/comment ()
  '(make-projection/common-lisp/comment->lisp-form/comment))

(def (macro e) common-lisp/top-level->lisp-form/top-level ()
  '(make-projection/common-lisp/top-level->lisp-form/top-level))

;;;;;;
;;; Printer

(def function recurse/slot (recursion input slot input-reference)
  (bind ((value (slot-value input slot))
         (reader (find-slot-reader (class-of input) (find-slot (class-of input) slot))))
    (if (listp value)
        (loop for element :in (slot-value input slot)
              for index :from 0
              collect (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                           (the sequence (,reader (the ,(form-type input) document)))
                                                           ,@(typed-reference (form-type input) input-reference))))
        (recurse-printer recursion value `((,reader (the ,(form-type input) document))
                                           ,@(typed-reference (form-type input) input-reference))))))

(def function recurse/ordinary-lambda-list (recursion input input-reference)
  (bind ((arguments (bindings-of input))
         (optional-seen? nil)
         (rest-seen? nil)
         (keyword-seen? nil)
         (allow-other-keys-seen? nil)
         (auxiliary-seen? nil))
    (labels ((ensure-&key ()
               (unless keyword-seen?
                 (assert (not auxiliary-seen?))
                 (setq keyword-seen? t)
                 (list '&key)))
             (ensure-&allow-other-keys ()
               (when (and (not allow-other-keys-seen?)
                          (allow-other-keys-p input))
                 (setf allow-other-keys-seen? t)
                 (nconc (ensure-&key)
                        (list '&allow-other-keys)))))
      (loop
        :for index :from 0
        :for argument :in arguments
        :appending (etypecase argument
                     (common-lisp/required-function-argument
                      (assert (not (or optional-seen? rest-seen? keyword-seen? auxiliary-seen?))))
                     (common-lisp/optional-function-argument
                      (unless optional-seen?
                        (assert (not (or rest-seen? keyword-seen? auxiliary-seen?)))
                        (setq optional-seen? t)
                        (list '&optional)))
                     (common-lisp/rest-function-argument
                      (unless rest-seen?
                        (assert (not (or keyword-seen? auxiliary-seen?)))
                        (setq rest-seen? t)
                        (list '&rest)))
                     (common-lisp/keyword-function-argument
                      (ensure-&key))
                     (common-lisp/auxiliary-function-argument
                      (unless auxiliary-seen?
                        (setq auxiliary-seen? t)
                        (nconc (ensure-&allow-other-keys)
                               (list '&aux))))) :into result
        :collect (recurse-printer recursion argument `((elt (the sequence document) ,index)
                                                       ,(typed-reference (form-type input) input-reference))) :into result
        :finally (return (nconc result (ensure-&allow-other-keys)))))))

(def printer common-lisp/constant-form->lisp-form/string (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (value-of input))
         ((:values output value-iomap)
          (if (or (eq value t)
                  (eq value nil)
                  (keywordp value))
              (values (make-lisp-form/symbol value :font-color *color/solarized/magenta*))
              (etypecase value
                (number
                 (values (make-lisp-form/number value)))
                (string
                 (values (make-lisp-form/string value)))
                (symbol
                 (values (make-lisp-form/symbol value :font-color *color/solarized/magenta*)))))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output) #+nil value-iomap))))

(def printer common-lisp/variable-reference-form->lisp-form/string (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-lisp-form/symbol (name-of input) :font-color *color/solarized/orange*)))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input input-reference output)))))

(def printer common-lisp/if-form->lisp-form/list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (condition-iomap (recurse/slot recursion input 'condition input-reference))
         (then-iomap (recurse/slot recursion input 'then input-reference))
         (else-iomap (recurse/slot recursion input 'else input-reference))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'if :font-color *color/solarized/blue*)
                                             (output-of condition-iomap)
                                             (bind ((then-output (output-of then-iomap)))
                                               (setf (indentation-of then-output) 4)
                                               then-output)
                                             (awhen (output-of else-iomap)
                                               (setf (indentation-of it) 4)
                                               (list it)))))
         (if-iomap))
    (make-iomap/compound projection recursion input input-reference output
                         (list #+nil
                               (make-iomap/object projection recursion input input-reference output)
                               #+nil if-iomap
                               #+nil condition-iomap
                               #+nil then-iomap
                               #+nil else-iomap))))

(def printer common-lisp/the-form->lisp-form/list (projection recursion input input-reference)
  (bind ((output (make-lisp-form/list (list (make-lisp-form/symbol 'the :font-color *color/solarized/blue*)
                                            (make-lisp-form/symbol (declared-type-of input) :font-color *color/solarized/violet*)
                                            (output-of (recurse/slot recursion input 'value input-reference))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer common-lisp/progn-form->lisp-form/list (projection recursion input input-reference)
  (bind ((body-iomaps (recurse/slot recursion input 'body input-reference))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'progn :font-color *color/solarized/blue*)
                                             (iter (for body-iomap :in-sequence body-iomaps)
                                                   (for body-output = (output-of body-iomap))
                                                   ;; KLUDGE:
                                                   (setf (indentation-of body-output) 2)
                                                   (collect body-output))
                                             ))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer common-lisp/lexical-variable-binding-form->lisp-form/list (projection recursion input input-reference)
  (bind ((output (make-lisp-form/list (list (make-lisp-form/symbol (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*)
                                            (output-of (recurse/slot recursion input 'initial-value input-reference))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer common-lisp/let-form->lisp-form/list (projection recursion input input-reference)
  (bind ((binding-iomaps (recurse/slot recursion input 'bindings input-reference))
         (body-iomaps (recurse/slot recursion input 'body input-reference))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'let :font-color *color/solarized/blue*)
                                             (make-lisp-form/list (iter (for binding-iomap :in-sequence binding-iomaps)
                                                                        (for binding-output = (output-of binding-iomap))
                                                                        ;; KLUDGE:
                                                                        (setf (indentation-of binding-output) 2)
                                                                        (collect binding-output)))
                                             (iter (for body-iomap :in-sequence body-iomaps)
                                                   (for body-output = (output-of body-iomap))
                                                   ;; KLUDGE:
                                                   (setf (indentation-of body-output) 2)
                                                   (collect body-output))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer common-lisp/application-form->lisp-form/list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (argument-iomaps (when (arguments-of input)
                            (recurse/slot recursion input 'arguments input-reference)))
         (output (make-lisp-form/list (cons (make-lisp-form/symbol (operator-of input) :font-color *color/solarized/violet*)
                                            (iter (for argument-iomap :in-sequence argument-iomaps)
                                                  (for argument-output = (output-of argument-iomap))
                                                  ;; KLUDGE:
                                                  (unless (first-iteration-p)
                                                    (setf (indentation-of argument-output) (+ 2 (length (symbol-name (operator-of input))))))
                                                  (collect argument-output))))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                argument-iomaps))))

(def printer common-lisp/function-definition-form->lisp-form/list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (documentation (documentation-of input))
         (binding-iomaps (recurse/ordinary-lambda-list recursion input input-reference))
         (body-iomaps (recurse/slot recursion input 'body input-reference))
         (output (make-lisp-form/list (append (list (make-lisp-form/symbol 'defun :font-color *color/solarized/blue*)
                                                    (make-lisp-form/symbol (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/violet*)
                                                    (make-lisp-form/list (mapcar 'output-of binding-iomaps)))
                                              (when documentation (list (make-lisp-form/string documentation :indentation 2)))
                                              (iter (for body-iomap :in-sequence body-iomaps)
                                                    (for body-output = (output-of body-iomap))
                                                    ;; KLUDGE:
                                                    (setf (indentation-of body-output) 2)
                                                    (collect body-output)))))
         (defun-iomap)
         (name-iomap)
         (documentation-iomaps (when documentation
                                 (list (make-iomap/object projection recursion input `(the string (slot-value ,typed-input-reference 'documentation)) output))))
         (bindings-iomap (make-iomap/object projection recursion input `(the sequence (slot-value ,typed-input-reference 'bindings)) output)))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output)
                                       defun-iomap
                                       name-iomap
                                       bindings-iomap)
                                 documentation-iomaps
                                 binding-iomaps
                                 body-iomaps))))

(def printer common-lisp/lambda-function-form->lisp-form/list (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (binding-iomaps (recurse/ordinary-lambda-list recursion input input-reference))
         (body-iomaps (recurse/slot recursion input 'body input-reference))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'lambda :font-color *color/solarized/blue*)
                                             (make-lisp-form/list (mapcar 'output-of binding-iomaps))
                                             (mapcar 'output-of body-iomaps))))
         (lambda-iomap)
         (bindings-iomap (make-iomap/object projection recursion input `(the sequence (slot-value ,typed-input-reference 'bindings)) output)))
    (make-iomap/compound projection recursion input input-reference output
                         (append (list (make-iomap/object projection recursion input input-reference output)
                                       lambda-iomap
                                       bindings-iomap)
                                 binding-iomaps
                                 body-iomaps))))

(def printer common-lisp/function-argument-form->lisp-form/string (projection recursion input input-reference)
  (bind ((name (name-of input))
         (output (make-lisp-form/symbol name :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*)))
    (make-iomap/object projection recursion input input-reference output)))

(def printer common-lisp/comment->lisp-form/comment (projection recursion input input-reference)
  (bind ((output (make-lisp-form/comment (output-of (recurse-printer recursion (content-of input) input-reference)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer common-lisp/top-level->lisp-form/top-level (projection recursion input input-reference)
  (bind ((body-iomaps (recurse/slot recursion input 'body input-reference))
         (output (make-lisp-form/top-level (iter (for body-iomap :in-sequence body-iomaps)
                                                 (for body-output = (output-of body-iomap))
                                                 ;; KLUDGE:
                                                 (setf (indentation-of body-output) 0)
                                                 (collect body-output)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader common-lisp/constant-form->lisp-form/string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/variable-reference-form->lisp-form/string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/if-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/the-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/progn-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/lexical-variable-binding-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/let-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/application-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/function-definition-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/lambda-function-form->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/function-argument-form->lisp-form/string (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/comment->lisp-form/comment (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader common-lisp/top-level->lisp-form/top-level (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
