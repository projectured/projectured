;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) walked-lisp-form/constant-form->lisp-form/string ()
  ())

(def (projection e) walked-lisp-form/variable-reference-form->lisp-form/string ()
  ())

(def (projection e) walked-lisp-form/if-form->lisp-form/list ()
  ())

(def (projection e) walked-lisp-form/the-form->lisp-form/list ()
  ())

(def (projection e) walked-lisp-form/application-form->lisp-form/list ()
  ())

(def (projection e) walked-lisp-form/function-definition-form->lisp-form/list ()
  ())

(def (projection e) walked-lisp-form/lambda-function-form->lisp-form/list ()
  ())

(def (projection e) walked-lisp-form/function-argument-form->lisp-form/string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/walked-lisp-form/constant-form->lisp-form/string ()
  (make-projection 'walked-lisp-form/constant-form->lisp-form/string))

(def (function e) make-projection/walked-lisp-form/variable-reference-form->lisp-form/string ()
  (make-projection 'walked-lisp-form/variable-reference-form->lisp-form/string))

(def (function e) make-projection/walked-lisp-form/if-form->lisp-form/list ()
  (make-projection 'walked-lisp-form/if-form->lisp-form/list))

(def (function e) make-projection/walked-lisp-form/the-form->lisp-form/list ()
  (make-projection 'walked-lisp-form/the-form->lisp-form/list))

(def (function e) make-projection/walked-lisp-form/application-form->lisp-form/list ()
  (make-projection 'walked-lisp-form/application-form->lisp-form/list))

(def (function e) make-projection/walked-lisp-form/function-definition-form->lisp-form/list ()
  (make-projection 'walked-lisp-form/function-definition-form->lisp-form/list))

(def (function e) make-projection/walked-lisp-form/lambda-function-form->lisp-form/list ()
  (make-projection 'walked-lisp-form/lambda-function-form->lisp-form/list))

(def (function e) make-projection/walked-lisp-form/function-argument-form->lisp-form/string ()
  (make-projection 'walked-lisp-form/function-argument-form->lisp-form/string))

;;;;;;
;;; Construction

(def (macro e) walked-lisp-form/constant-form->lisp-form/string ()
  '(make-projection/walked-lisp-form/constant-form->lisp-form/string))

(def (macro e) walked-lisp-form/variable-reference-form->lisp-form/string ()
  '(make-projection/walked-lisp-form/variable-reference-form->lisp-form/string))

(def (macro e) walked-lisp-form/if-form->lisp-form/list ()
  '(make-projection/walked-lisp-form/if-form->lisp-form/list))

(def (macro e) walked-lisp-form/the-form->lisp-form/list ()
  '(make-projection/walked-lisp-form/the-form->lisp-form/list))

(def (macro e) walked-lisp-form/application-form->lisp-form/list ()
  '(make-projection/walked-lisp-form/application-form->lisp-form/list))

(def (macro e) walked-lisp-form/function-definition-form->lisp-form/list ()
  '(make-projection/walked-lisp-form/function-definition-form->lisp-form/list))

(def (macro e) walked-lisp-form/lambda-function-form->lisp-form/list ()
  '(make-projection/walked-lisp-form/lambda-function-form->lisp-form/list))

(def (macro e) walked-lisp-form/function-argument-form->lisp-form/string ()
  '(make-projection/walked-lisp-form/function-argument-form->lisp-form/string))

;;;;;;
;;; Printer

(def function recurse/slot (recursion input slot input-reference output-reference &optional base-index)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (slot-value input slot))
         (slot-path `(slot-value ,typed-input-reference ',slot)))
    (if (consp value)
        (loop for element :in (slot-value input slot)
              for index :from 0
              collect (recurse-printer recursion element
                                       `(elt (the ,(form-type value) ,slot-path) ,index)
                                       (if base-index
                                           `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,(+ base-index index))
                                           output-reference)))
        (recurse-printer recursion value slot-path output-reference))))

(def function recurse/ordinary-lambda-list (recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (arguments (hu.dwim.walker:bindings-of input))
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
                          (hu.dwim.walker:allow-other-keys? input))
                 (setf allow-other-keys-seen? t)
                 (nconc (ensure-&key)
                        (list '&allow-other-keys)))))
      (loop
        :for index :from 0
        :for argument :in arguments
        :appending (etypecase argument
                     (hu.dwim.walker:required-function-argument-form
                      (assert (not (or optional-seen? rest-seen? keyword-seen? auxiliary-seen?))))
                     (hu.dwim.walker:optional-function-argument-form
                      (unless optional-seen?
                        (assert (not (or rest-seen? keyword-seen? auxiliary-seen?)))
                        (setq optional-seen? t)
                        (list '&optional)))
                     (hu.dwim.walker:rest-function-argument-form
                      (unless rest-seen?
                        (assert (not (or keyword-seen? auxiliary-seen?)))
                        (setq rest-seen? t)
                        (list '&rest)))
                     (hu.dwim.walker:keyword-function-argument-form
                      (ensure-&key))
                     (hu.dwim.walker:auxiliary-function-argument-form
                      (unless auxiliary-seen?
                        (setq auxiliary-seen? t)
                        (nconc (ensure-&allow-other-keys)
                               (list '&aux))))) :into result
        :collect (recurse-printer recursion argument
                                  `(elt ,typed-input-reference ,index)
                                  `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,index)) :into result
        :finally (return (nconc result
                                (ensure-&allow-other-keys)))))))

(def printer walked-lisp-form/constant-form->lisp-form/string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (value (hu.dwim.walker:value-of input))
         ((:values output value-iomap)
          (if (or (eq value t)
                  (eq value nil)
                  (keywordp value))
              (make-lisp-form/symbol value)
              (etypecase value
                (number
                 (values (make-lisp-form/number value)
                         (make-iomap/string* input `(the string (write-to-string (the number (slot-value ,typed-input-reference 'hu.dwim.walker::value)))) 0
                                             value `(the string (write-to-string (the number (value-of (the lisp-form/number ,output-reference))))) 0
                                             (length (write-to-string value)))))
                (string
                 (values (make-lisp-form/string value)
                         (make-iomap/string* input `(the string (slot-value ,typed-input-reference 'hu.dwim.walker::value)) 0
                                             value `(the string (value-of (the lisp-form/string ,output-reference))) 0
                                             (length (write-to-string value)))))
                (symbol
                 (values (make-lisp-form/symbol value)
                         (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::value)))) 0
                                             value `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                                             (length (write-to-string value)))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference) value-iomap))))

(def printer walked-lisp-form/variable-reference-form->lisp-form/string (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-lisp-form/symbol (hu.dwim.walker:name-of input))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::name)))) 0
                                                    output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                                                    (length (symbol-name (hu.dwim.walker:name-of input))))))))

(def printer walked-lisp-form/if-form->lisp-form/list (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (condition-iomap (recurse/slot recursion input 'hu.dwim.walker::condition input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))
         (then-iomap (recurse/slot recursion input 'hu.dwim.walker::then input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (else-iomap (recurse/slot recursion input 'hu.dwim.walker::else input-reference `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 3)))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'if)
                                             (output-of condition-iomap)
                                             (output-of then-iomap)
                                             (awhen (output-of else-iomap) (list it)))))
         (if-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::form-name)))) 0
                                       output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                       2)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                if-iomap
                                condition-iomap
                                then-iomap
                                else-iomap))))

(def printer walked-lisp-form/the-form->lisp-form/list (projection recursion input input-reference output-reference)
  (bind ((output (make-lisp-form/list (list (make-lisp-form/symbol 'the)
                                            (make-lisp-form/symbol (hu.dwim.walker::declared-type-of input))
                                            (output-of (recurse/slot recursion input 'hu.dwim.walker::value input-reference `(elt (the list ,output-reference) 2)))))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer walked-lisp-form/application-form->lisp-form/list (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (argument-iomaps (recurse/slot recursion input 'hu.dwim.walker::arguments input-reference output-reference 1))
         (output (make-lisp-form/list (cons (make-lisp-form/symbol (hu.dwim.walker:operator-of input))
                                            (mapcar 'output-of argument-iomaps)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::operator)))) 0
                                                     output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                                     (length (symbol-name (hu.dwim.walker:operator-of input))))
                                 argument-iomaps))))

(def printer walked-lisp-form/function-definition-form->lisp-form/list (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (docstring (hu.dwim.walker:docstring-of input))
         (binding-iomaps (recurse/ordinary-lambda-list recursion input
                                                       input-reference
                                                       `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (body-iomaps (recurse/slot recursion input 'hu.dwim.walker::body
                                    input-reference
                                    `(elt (the list (elements-of (the lisp-form/list ,output-reference))) ,(if docstring 4 3))))
         (output (make-lisp-form/list (append (list (make-lisp-form/symbol 'defun)
                                                    (make-lisp-form/symbol (hu.dwim.walker:name-of input))
                                                    (make-lisp-form/list (mapcar 'output-of binding-iomaps)))
                                              (when docstring (list (make-lisp-form/string docstring)))
                                              (mapcar 'output-of body-iomaps))))
         (defun-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::form-name)))) 0
                                          output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                          5))
         (name-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::name)))) 0
                                         output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))))) 0
                                         (length (symbol-name (hu.dwim.walker:name-of input)))))
         (docstring-iomaps (when docstring
                             (list (make-iomap/string* input `(the string (slot-value ,typed-input-reference 'hu.dwim.walker::docstring)) 0
                                                       output `(the string (value-of (the lisp-form/string (elt (the list (elements-of (the lisp-form/list ,output-reference))) 3)))) 0
                                                       (length docstring))
                                   (make-iomap/object* projection recursion input `(the string (slot-value ,typed-input-reference 'hu.dwim.walker::docstring))
                                                       output `(the string (value-of (the lisp-form/string (elt (the list (elements-of (the lisp-form/list ,output-reference))) 3))))))))
         (bindings-iomap (make-iomap/object* projection recursion input `(the list (slot-value ,typed-input-reference 'hu.dwim.walker::bindings))
                                             output `(the lisp-form/list (elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference)
                                        defun-iomap
                                        name-iomap
                                        bindings-iomap)
                                  docstring-iomaps
                                  binding-iomaps
                                  body-iomaps))))

(def printer walked-lisp-form/lambda-function-form->lisp-form/list (projection recursion input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (binding-iomaps (recurse/ordinary-lambda-list recursion input
                                                       input-reference
                                                       `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))
         (body-iomaps (recurse/slot recursion input 'hu.dwim.walker::body
                                    input-reference
                                    `(elt (the list (elements-of (the lisp-form/list ,output-reference))) 2)))
         (output (make-lisp-form/list (list* (make-lisp-form/symbol 'lambda)
                                             (make-lisp-form/list (mapcar 'output-of binding-iomaps))
                                             (mapcar 'output-of body-iomaps))))
         (lambda-iomap (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::form-name)))) 0
                                           output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol (elt (the list (elements-of (the lisp-form/list ,output-reference))) 0)))))) 0
                                           6))
         (bindings-iomap (make-iomap/object* projection recursion input `(the list (slot-value ,typed-input-reference 'hu.dwim.walker::bindings))
                                             output `(the lisp-form/list (elt (the list (elements-of (the lisp-form/list ,output-reference))) 1)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference)
                                        lambda-iomap
                                        bindings-iomap)
                                  binding-iomaps
                                  body-iomaps))))

(def printer walked-lisp-form/function-argument-form->lisp-form/string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (name (hu.dwim.walker:name-of input))
         (output (make-lisp-form/symbol name)))
    (make-iomap/string* input `(the string (string-downcase (the symbol (slot-value ,typed-input-reference 'hu.dwim.walker::name)))) 0
                        output `(the string (string-downcase (the symbol (value-of (the lisp-form/symbol ,output-reference))))) 0
                        (length (string-downcase name)))))

;;;;;;
;;; Reader

(def reader walked-lisp-form/constant-form->lisp-form/string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/variable-reference-form->lisp-form/string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/if-form->lisp-form/list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/the-form->lisp-form/list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/application-form->lisp-form/list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/function-definition-form->lisp-form/list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/lambda-function-form->lisp-form/list (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader walked-lisp-form/function-argument-form->lisp-form/string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
