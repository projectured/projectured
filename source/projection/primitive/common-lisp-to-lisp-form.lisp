;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) common-lisp/constant-form->lisp-form/string ()
  ())

(def (projection e) common-lisp/variable-reference-form->lisp-form/string ()
  ())

(def (projection e) common-lisp/if-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/the-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/progn-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/lexical-variable-binding-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/let-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/application-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/function-definition-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/lambda-function-form->lisp-form/list ()
  ())

(def (projection e) common-lisp/function-argument-form->lisp-form/string ()
  ())

(def (projection e) common-lisp/comment->lisp-form/comment ()
  ())

(def (projection e) common-lisp/top-level->lisp-form/top-level ()
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

(def function recurse/slot (recursion iomap input slot input-reference output-reference &optional base-index)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (slot-value input slot))
         (slot-path `(slot-value ,typed-input-reference ',slot)))
    (if (listp value)
        (loop for element :in (slot-value input slot)
              for index :from 0
              collect (recurse-printer recursion iomap element
                                       `(elt (the ,(form-type value) ,slot-path) ,index)
                                       (if base-index
                                           `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,(+ base-index index))
                                           output-reference)))
        (recurse-printer recursion iomap value slot-path output-reference))))

(def function recurse/ordinary-lambda-list (recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (arguments (bindings-of input))
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
        :collect (recurse-printer recursion iomap argument
                                  `(elt ,typed-input-reference ,index)
                                  `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,index)) :into result
        :finally (return (nconc result
                                (ensure-&allow-other-keys)))))))

(def printer common-lisp/constant-form->lisp-form/string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (value-of input))
         ((:values output value-iomap)
          (if (or (eq value t)
                  (eq value nil)
                  (keywordp value))
              (values (make-lisp-form/symbol value :font-color *color/solarized/magenta*)
                      (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'value)))) 0
                                          value `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                                          (length (write-to-string value))))
              (etypecase value
                (number
                 (values (make-lisp-form/number value)
                         (make-iomap/string* input `(the string (write-to-string (the number (slot-value ,typed-input-reference 'value)))) 0
                                             value `(the string (write-to-string (the number (value-of (the lisp-form/number ,output-reference))))) 0
                                             (length (write-to-string value)))))
                (string
                 (values (make-lisp-form/string value)
                         (make-iomap/string* input `(the string (slot-value ,typed-input-reference 'value)) 0
                                             value `(the string (value-of (the lisp-form/string ,output-reference))) 0
                                             (length (write-to-string value)))))
                (symbol
                 (values (make-lisp-form/symbol value :font-color *color/solarized/magenta*)
                         (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'value)))) 0
                                             value `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                                             (length (write-to-string value)))))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference) value-iomap))))

(def printer common-lisp/variable-reference-form->lisp-form/string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-lisp-form/symbol (name-of input) :font-color *color/solarized/orange*)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'name)))) 0
                                                    output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                                                    (length (symbol-name (name-of input))))))))

(def printer common-lisp/if-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (condition-iomap (recurse/slot recursion iomap input 'condition input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))
         (then-iomap (recurse/slot recursion iomap input 'then input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (else-iomap (recurse/slot recursion iomap input 'else input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 3)))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'if :font-color *color/solarized/blue*)
                                             (output-of condition-iomap)
                                             (bind ((then-output (output-of then-iomap)))
                                               (setf (indentation-of then-output) 4)
                                               then-output)
                                             (awhen (output-of else-iomap)
                                               (setf (indentation-of it) 4)
                                               (list it)))))
         (if-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'form-name)))) 0
                                       output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                       2)))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                if-iomap
                                condition-iomap
                                then-iomap
                                else-iomap))))

(def printer common-lisp/the-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((output (make-lisp-form/list (list (make-lisp-form/symbol 'the :font-color *color/solarized/blue*)
                                            (make-lisp-form/symbol (declared-type-of input) :font-color *color/solarized/violet*)
                                            (output-of (recurse/slot recursion iomap input 'value input-reference `(elt (the list ,output-reference) 2)))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer common-lisp/progn-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((body-iomaps (recurse/slot recursion iomap input 'body input-reference `(elt (the list ,output-reference) 1)))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'progn :font-color *color/solarized/blue*)
                                             (iter (for body-iomap :in-sequence body-iomaps)
                                                   (for body-output = (output-of body-iomap))
                                                   ;; KLUDGE:
                                                   (setf (indentation-of body-output) 2)
                                                   (collect body-output))
                                             ))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer common-lisp/lexical-variable-binding-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((output (make-lisp-form/list (list (make-lisp-form/symbol (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*)
                                            (output-of (recurse/slot recursion iomap input 'initial-value
                                                                     input-reference
                                                                     output-reference))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer common-lisp/let-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((binding-iomaps (recurse/slot recursion iomap input 'bindings
                                       input-reference
                                       `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))
         (body-iomaps (recurse/slot recursion iomap input 'body
                                    input-reference
                                    `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))
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
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer common-lisp/application-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (argument-iomaps (when (arguments-of input)
                            (recurse/slot recursion iomap input 'arguments input-reference output-reference 1)))
         (output (make-lisp-form/list (cons (make-lisp-form/symbol (operator-of input) :font-color *color/solarized/violet*)
                                            (iter (for argument-iomap :in-sequence argument-iomaps)
                                                  (for argument-output = (output-of argument-iomap))
                                                  ;; KLUDGE:
                                                  (unless (first-iteration-p)
                                                    (setf (indentation-of argument-output) (+ 2 (length (symbol-name (operator-of input))))))
                                                  (collect argument-output))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'operator)))) 0
                                                     output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                                     (length (symbol-name (operator-of input))))
                                 argument-iomaps))))

(def printer common-lisp/function-definition-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (documentation (documentation-of input))
         (binding-iomaps (recurse/ordinary-lambda-list recursion iomap input
                                                       input-reference
                                                       `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (body-iomaps (recurse/slot recursion iomap input 'body
                                    input-reference
                                    `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,(if documentation 4 3))))
         (output (make-lisp-form/list (append (list (make-lisp-form/symbol 'defun :font-color *color/solarized/blue*)
                                                    (make-lisp-form/symbol (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/violet*)
                                                    (make-lisp-form/list (mapcar 'output-of binding-iomaps)))
                                              (when documentation (list (make-lisp-form/string documentation :indentation 2)))
                                              (iter (for body-iomap :in-sequence body-iomaps)
                                                    (for body-output = (output-of body-iomap))
                                                    ;; KLUDGE:
                                                    (setf (indentation-of body-output) 2)
                                                    (collect body-output)))))
         (defun-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'form-name)))) 0
                                          output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                          5))
         (name-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'name)))) 0
                                         output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))))) 0
                                         (length (symbol-name (name-of input)))))
         (documentation-iomaps (when documentation
                                 (list (make-iomap/string* input `(the string (slot-value ,typed-input-reference 'documentation)) 0
                                                           output `(the string (value-of (the lisp-form/string (elt (the list (elements-of (the lisp-form/list ,output-reference))) 3)))) 0
                                                           (length documentation))
                                       (make-iomap/object* projection recursion input `(the string (slot-value ,typed-input-reference 'documentation))
                                                           output `(the string (value-of (the lisp-form/string (elt (the list (elements-of (the lisp-form/list ,output-reference))) 3))))))))
         (bindings-iomap (make-iomap/object* projection recursion input `(the list (slot-value ,typed-input-reference 'bindings))
                                             output `(the lisp-form/list (elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference)
                                        defun-iomap
                                        name-iomap
                                        bindings-iomap)
                                  documentation-iomaps
                                  binding-iomaps
                                  body-iomaps))))

(def printer common-lisp/lambda-function-form->lisp-form/list (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (binding-iomaps (recurse/ordinary-lambda-list recursion iomap input
                                                       input-reference
                                                       `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))
         (body-iomaps (recurse/slot recursion iomap input 'body
                                    input-reference
                                    `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'lambda :font-color *color/solarized/blue*)
                                             (make-lisp-form/list (mapcar 'output-of binding-iomaps))
                                             (mapcar 'output-of body-iomaps))))
         (lambda-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'form-name)))) 0
                                           output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                           6))
         (bindings-iomap (make-iomap/object* projection recursion input `(the list (slot-value ,typed-input-reference 'bindings))
                                             output `(the lisp-form/list (elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference)
                                        lambda-iomap
                                        bindings-iomap)
                                  binding-iomaps
                                  body-iomaps))))

(def printer common-lisp/function-argument-form->lisp-form/string (projection recursion iomap input input-reference output-reference)
  (declare (ignore projection recursion iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (name (name-of input))
         (output (make-lisp-form/symbol name :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*)))
    (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'name)))) 0
                        output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                        (length (string-downcase name)))))

(def printer common-lisp/comment->lisp-form/comment (projection recursion iomap input input-reference output-reference)
  (bind ((output (make-lisp-form/comment (output-of (recurse-printer recursion iomap (content-of input) input-reference output-reference)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)))))

(def printer common-lisp/top-level->lisp-form/top-level (projection recursion iomap input input-reference output-reference)
  (bind ((body-iomaps (recurse/slot recursion iomap input 'body input-reference `(elt (the list ,output-reference) 1)))
         (output (make-lisp-form/top-level (iter (for body-iomap :in-sequence body-iomaps)
                                                 (for body-output = (output-of body-iomap))
                                                 ;; KLUDGE:
                                                 (setf (indentation-of body-output) 0)
                                                 (collect body-output)))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)))))

;;;;;;
;;; Reader

(def reader common-lisp/constant-form->lisp-form/string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/variable-reference-form->lisp-form/string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/if-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/the-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/progn-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/lexical-variable-binding-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/let-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/application-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/function-definition-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/lambda-function-form->lisp-form/list (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/function-argument-form->lisp-form/string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/comment->lisp-form/comment (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader common-lisp/top-level->lisp-form/top-level (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
