;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection java/statement/block->tree/node ()
  ())

(def projection java/statement/if->tree/node ()
  ())

(def projection java/statement/return->tree/node ()
  ())

(def projection java/expression/variable-reference->string ()
  ())

(def projection java/expression/method-invocation->tree/node  ()
  ())

(def projection java/expression/infix-operator->tree/node ()
  ())

(def projection java/literal/null->string ()
  ())

(def projection java/literal/number->string ()
  ())

(def projection java/literal/character->string ()
  ())

(def projection java/literal/string->string  ()
  ())

(def projection java/definition/method->tree/node ()
  ())

(def projection java/definition/argument->tree/node ()
  ())

(def projection java/definition/qualifier->string ()
  ())

(def projection java/definition/type->string ()
  ())

;;;;;;
;;; Construction

(def function make-projection/java/statement/block->tree/node ()
  (make-projection 'java/statement/block->tree/node))

(def function make-projection/java/statement/if->tree/node ()
  (make-projection 'java/statement/if->tree/node))

(def function make-projection/java/statement/return->tree/node ()
  (make-projection 'java/statement/return->tree/node))

(def function make-projection/java/expression/variable-reference->string ()
  (make-projection 'java/expression/variable-reference->string))

(def function make-projection/java/expression/method-invocation->tree/node  ()
  (make-projection 'java/expression/method-invocation->tree/node))

(def function make-projection/java/expression/infix-operator->tree/node ()
  (make-projection 'java/expression/infix-operator->tree/node))

(def function make-projection/java/literal/null->string ()
  (make-projection 'java/literal/null->string))

(def function make-projection/java/literal/number->string ()
  (make-projection 'java/literal/number->string))

(def function make-projection/java/literal/character->string ()
  (make-projection 'java/literal/character->string))

(def function make-projection/java/literal/string->string  ()
  (make-projection 'java/literal/string->string))

(def function make-projection/java/definition/method->tree/node ()
  (make-projection 'java/definition/method->tree/node))

(def function make-projection/java/definition/argument->tree/node ()
  (make-projection 'java/definition/argument->tree/node))

(def function make-projection/java/definition/qualifier->string ()
  (make-projection 'java/definition/qualifier->string))

(def function make-projection/java/definition/type->string ()
  (make-projection 'java/definition/type->string))

;;;;;;
;;; Construction

(def macro java/statement/block->tree/node ()
  '(make-projection/java/statement/block->tree/node))

(def macro java/statement/if->tree/node ()
  '(make-projection/java/statement/if->tree/node))

(def macro java/statement/return->tree/node ()
  '(make-projection/java/statement/return->tree/node))

(def macro java/expression/variable-reference->string ()
  '(make-projection/java/expression/variable-reference->string))

(def macro java/expression/method-invocation->tree/node  ()
  '(make-projection/java/expression/method-invocation->tree/node))

(def macro java/expression/infix-operator->tree/node ()
  '(make-projection/java/expression/infix-operator->tree/node))

(def macro java/literal/null->string ()
  '(make-projection/java/literal/null->string))

(def macro java/literal/number->string ()
  '(make-projection/java/literal/number->string))

(def macro java/literal/character->string ()
  '(make-projection/java/literal/character->string))

(def macro java/literal/string->string  ()
  '(make-projection/java/literal/string->string))

(def macro java/definition/method->tree/node ()
  '(make-projection/java/definition/method->tree/node))

(def macro java/definition/argument->tree/node ()
  '(make-projection/java/definition/argument->tree/node))

(def macro java/definition/qualifier->string ()
  '(make-projection/java/definition/qualifier->string))

(def macro java/definition/type->string ()
  '(make-projection/java/definition/type->string))

;;;;;;
;;; Printer

(def printer java/statement/block->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion element `((elt (elements-of (the sequence document)) ,index)
                                                                                         ,@(typed-reference (form-type input) input-reference))))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 2)
                                       (collect (output-of iomap)))
                                 :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 ;; KLUDGE:
                                 :closing-delimiter (text/text () (text/string "
}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer java/statement/if->tree/node (projection recursion input input-reference)
  (bind ((condition-iomap (recurse-printer recursion (condition-of input) `((condition-of (the java/statement/if document))
                                                                            ,@(typed-reference (form-type input) input-reference))))
         (then-branch-iomap (recurse-printer recursion (then-of input) `((then-of (the java/statement/if document))
                                                                         ,@(typed-reference (form-type input) input-reference))))
         (else-branch-iomap (recurse-printer recursion (else-of input) `((else-of (the java/statement/if document))
                                                                         ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list* (make-tree/leaf (text/text () (text/string "if" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                        (make-tree/node (children-of (output-of condition-iomap))
                                                        :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                        :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                        :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                        (output-of then-branch-iomap)
                                        (make-tree/leaf (text/text () (text/string "else" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)) :indentation 0)
                                        (when (else-of input)
                                          (list (output-of else-branch-iomap))))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    ;; KLUDGE:
    (setf (indentation-of (output-of then-branch-iomap)) 4)
    (setf (indentation-of (output-of else-branch-iomap)) 4)
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)
                               condition-iomap
                               then-branch-iomap
                               else-branch-iomap))))

(def printer java/statement/return->tree/node (projection recursion input input-reference)
  (bind ((iomap (recurse-printer recursion (value-of input) `((value-of (the java/statement/return document))
                                                              ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (make-tree/leaf (text/text () (text/string "return" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))) (output-of iomap))
                                 :closing-delimiter (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)
                               iomap))))

(def printer java/expression/variable-reference->string (projection recursion input input-reference)
  (bind ((output-content (name-of input))
         (output (make-tree/leaf (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/orange*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer java/expression/method-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (list (make-tree/leaf (text/text () (text/string (method-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*)))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion argument `((elt (the sequence (the sequence document)) ,index)
                                                                                                                (the sequence (arguments-of (the java/expression/method-invocation document)))
                                                                                                                ,@(typed-reference (form-type input) input-reference))))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap)))
                                                       :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                #+nil
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference) output)
                                (nreverse child-iomaps)))))

(def printer java/expression/infix-operator->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for argument :in-sequence (arguments-of input))
                                       (for iomap = (recurse-printer recursion argument `((elt (the sequence (the sequence document)) ,index)
                                                                                          (the sequence (arguments-of (the java/expression/infix-operator document)))
                                                                                          ,@(typed-reference (form-type input) input-reference))))
                                       (unless (first-iteration-p)
                                         (collect (make-tree/leaf (text/text () (text/string (operator-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*)))))
                                       (push iomap child-iomaps)
                                       (collect (output-of iomap)))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                (nreverse child-iomaps)))))

(def printer java/literal/null->string (projection recursion input input-reference)
  (bind ((output "null"))
    (make-iomap/object projection recursion input input-reference output)))

(def printer java/literal/number->string (projection recursion input input-reference)
  (bind ((output-content (write-to-string (value-of input)))
         (output (make-tree/leaf (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer java/literal/character->string (projection recursion input input-reference)
  (bind ((output (format nil "'~A'" (value-of input))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer java/literal/string->string (projection recursion input input-reference)
  (bind ((output (value-of input)))
    (make-iomap/object projection recursion input input-reference output)))

(def printer java/definition/method->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (qualifier-iomap (recurse-printer recursion (qualifier-of input) `((qualifier-of (the java/definition/method document))
                                                                            ,@(typed-reference (form-type input) input-reference))))
         (return-type-iomap (recurse-printer recursion (return-type-of input) `((return-type-of (the java/definition/method document))
                                                                                ,@(typed-reference (form-type input) input-reference))))
         (body-iomap (recurse-printer recursion (body-of input) `((body-of (the java/definition/method document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (output-of qualifier-iomap)
                                       (output-of return-type-iomap)
                                       (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion argument `((elt (the sequence document) ,index)
                                                                                                                (the sequence (arguments-of (the java/definition/method document)))
                                                                                                                ,@(typed-reference (form-type input) input-reference))))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap)))
                                                       :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                       :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                       (output-of body-iomap))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    ;; KLUDGE:
    (setf (indentation-of (output-of body-iomap)) 0)
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                qualifier-iomap
                                return-type-iomap
                                body-iomap
                                #+nil
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference) output)
                                (nreverse child-iomaps)))))

(def printer java/definition/argument->tree/node (projection recursion input input-reference)
  (bind ((type-iomap (recurse-printer recursion (slot-value input 'type) `((slot-value (the java/definition/argument document) 'type)
                                                                           ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (output-of type-iomap)
                                       (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)
                               type-iomap))))

(def printer java/definition/qualifier->string (projection recursion input input-reference)
  (bind ((output-content (name-of input))
         (output (make-tree/leaf (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer java/definition/type->string (projection recursion input input-reference)
  (bind ((output-content (name-of input))
         (output (make-tree/leaf (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

;;;;;;
;;; Reader

(def reader java/statement/block->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/statement/if->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/statement/return->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/expression/variable-reference->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/expression/method-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/expression/infix-operator->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/literal/null->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/literal/number->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/literal/character->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/literal/string->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/definition/method->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/definition/argument->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/definition/qualifier->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader java/definition/type->string (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)
