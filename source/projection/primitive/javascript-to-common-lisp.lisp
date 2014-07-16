;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection javascript/statement/block->common-lisp/progn ()
  ())

(def projection javascript/statement/top-level->common-lisp/progn ()
  ())

(def projection javascript/expression/variable-reference->common-lisp/application ()
  ())

(def projection javascript/expression/property-access->common-lisp/progn ()
  ())

(def projection javascript/expression/constructor-invocation->common-lisp/progn ()
  ())

(def projection javascript/expression/method-invocation->common-lisp/progn ()
  ())

(def projection javascript/literal/string->common-lisp/application ()
  ())

(def projection javascript/definition/variable->common-lisp/progn ()
  ())

(def projection javascript/definition/function->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/javascript/statement/block->common-lisp/progn ()
  (make-projection 'javascript/statement/block->common-lisp/progn))

(def (function e) make-projection/javascript/statement/top-level->common-lisp/progn ()
  (make-projection 'javascript/statement/top-level->common-lisp/progn))

(def (function e) make-projection/javascript/expression/variable-reference->common-lisp/application ()
  (make-projection 'javascript/expression/variable-reference->common-lisp/application))

(def (function e) make-projection/javascript/expression/property-access->common-lisp/progn ()
  (make-projection 'javascript/expression/property-access->common-lisp/progn))

(def (function e) make-projection/javascript/expression/constructor-invocation->common-lisp/progn ()
  (make-projection 'javascript/expression/constructor-invocation->common-lisp/progn))

(def (function e) make-projection/javascript/expression/method-invocation->common-lisp/progn ()
  (make-projection 'javascript/expression/method-invocation->common-lisp/progn))

(def (function e) make-projection/javascript/literal/string->common-lisp/application ()
  (make-projection 'javascript/literal/string->common-lisp/application))

(def (function e) make-projection/javascript/definition/variable->common-lisp/progn ()
  (make-projection 'javascript/definition/variable->common-lisp/progn))

(def (function e) make-projection/javascript/definition/function->common-lisp/progn ()
  (make-projection 'javascript/definition/function->common-lisp/progn))

;;;;;;
;;; Printer

(def printer javascript/statement/block->common-lisp/progn (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion element `((elt (elements-of (the java/statement/block document)) ,index)
                                                                             ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (iter (for element-iomap :in element-iomaps)
                                               (collect (output-of element-iomap))
                                               (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                      (list (make-common-lisp/constant ";"))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/statement/top-level->common-lisp/progn (projection recursion input input-reference)
  (bind ((element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (collect (recurse-printer recursion element `((elt (elements-of (the javascript/statement/top-level document)) ,index)
                                                                             ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (iter (for element-iomap :in element-iomaps)
                                               (collect (output-of element-iomap))
                                               (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                      (list (make-common-lisp/constant ";"))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/expression/variable-reference->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                               (list (make-common-lisp/constant (name-of input))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/expression/property-access->common-lisp/progn (projection recursion input input-reference)
  (bind ((object-iomap (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/property-access document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (output (make-common-lisp/progn (list (output-of object-iomap)
                                               (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                             (list (make-common-lisp/constant (string+ "." (property-of input)))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/expression/constructor-invocation->common-lisp/progn (projection recursion input input-reference)
  (bind ((object-iomap (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/constructor-invocation document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (argument-iomaps (iter (for index :from 0)
                                (for argument :in-sequence (arguments-of input))
                                (collect (recurse-printer recursion argument `((elt (arguments-of (the javascript/definition/function document)) ,index)
                                                                               ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant (string+ "new ")))))
                                                 (list (output-of object-iomap)
                                                       (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant "("))))
                                                 (iter (for argument-iomap :in argument-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                (list (make-common-lisp/constant ", ")))))
                                                       (collect (output-of argument-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant ")"))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/expression/method-invocation->common-lisp/progn (projection recursion input input-reference)
  (bind ((object-iomap (recurse-printer recursion (object-of input) `((object-of (the java/expression/method-invocation document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (argument-iomaps (iter (for index :from 0)
                                (for argument :in-sequence (arguments-of input))
                                (collect (recurse-printer recursion argument `((elt (arguments-of (the javascript/definition/function document)) ,index)
                                                                               ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (append (list (output-of object-iomap)
                                                       (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant (string+ "." (method-of input) "(")))))
                                                 (iter (for argument-iomap :in argument-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                (list (make-common-lisp/constant ", ")))))
                                                       (collect (output-of argument-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant ")"))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/literal/string->common-lisp/application (projection recursion input input-reference)
  (bind ((output (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                               (list (make-common-lisp/constant (string+ "\"" (value-of input) "\""))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/definition/variable->common-lisp/progn (projection recursion input input-reference)
  (bind ((body-iomap (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/variable document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (output (make-common-lisp/progn (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                             (list (make-common-lisp/constant (string+ "var " (name-of input) " = "))))
                                               (output-of body-iomap)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/definition/function->common-lisp/progn (projection recursion input input-reference)
  (bind ((body-iomap (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/function document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (argument-iomaps (iter (for index :from 0)
                                (for argument :in-sequence (arguments-of input))
                                (collect (recurse-printer recursion argument `((elt (arguments-of (the javascript/definition/function document)) ,index)
                                                                               ,@(typed-reference (form-type input) input-reference))))))
         (output (make-common-lisp/progn (append (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant (string+ "function " (name-of input) "(")))))
                                                 (iter (for argument-iomap :in argument-iomaps)
                                                       (unless (first-iteration-p)
                                                         (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                (list (make-common-lisp/constant ", ")))))
                                                       (collect (output-of argument-iomap)))
                                                 (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant ") {")))
                                                       (output-of body-iomap)
                                                       (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                     (list (make-common-lisp/constant "}"))))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader javascript/statement/block->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/statement/top-level->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/variable-reference->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/property-access->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/constructor-invocation->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/method-invocation->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/literal/string->common-lisp/application (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/definition/variable->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/definition/function->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)
