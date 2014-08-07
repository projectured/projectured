;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection javascript/statement/block->tree/node ()
  ())

(def projection javascript/statement/top-level->tree/node ()
  ())

(def projection javascript/expression/variable-reference->tree/leaf ()
  ())

(def projection javascript/expression/property-access->tree/node ()
  ())

(def projection javascript/expression/constructor-invocation->tree/node ()
  ())

(def projection javascript/expression/method-invocation->tree/node ()
  ())

(def projection javascript/literal/string->tree/leaf ()
  ())

(def projection javascript/definition/variable->tree/node ()
  ())

(def projection javascript/definition/function->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/javascript/statement/block->tree/node ()
  (make-projection 'javascript/statement/block->tree/node))

(def function make-projection/javascript/statement/top-level->tree/node ()
  (make-projection 'javascript/statement/top-level->tree/node))

(def function make-projection/javascript/expression/variable-reference->tree/leaf ()
  (make-projection 'javascript/expression/variable-reference->tree/leaf))

(def function make-projection/javascript/expression/property-access->tree/node ()
  (make-projection 'javascript/expression/property-access->tree/node))

(def function make-projection/javascript/expression/constructor-invocation->tree/node ()
  (make-projection 'javascript/expression/constructor-invocation->tree/node))

(def function make-projection/javascript/expression/method-invocation->tree/node ()
  (make-projection 'javascript/expression/method-invocation->tree/node))

(def function make-projection/javascript/literal/string->tree/leaf ()
  (make-projection 'javascript/literal/string->tree/leaf))

(def function make-projection/javascript/definition/variable->tree/node ()
  (make-projection 'javascript/definition/variable->tree/node))

(def function make-projection/javascript/definition/function->tree/node ()
  (make-projection 'javascript/definition/function->tree/node))

;;;;;;
;;; Printer

(def printer javascript/statement/block->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion element `((elt (elements-of (the java/statement/block document)) ,index)
                                                                                         ,@(typed-reference (form-type input) input-reference))))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 2)
                                       (collect (output-of iomap))
                                       (collect (tree/leaf () (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
                                 :indentation 2
                                 :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 ;; KLUDGE:
                                 :closing-delimiter (text/text () (text/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer javascript/statement/top-level->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion element `((elt (elements-of (the javascript/statement/top-level document)) ,index)
                                                                                         ,@(typed-reference (form-type input) input-reference))))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 0)
                                       (collect (output-of iomap))
                                       (collect (tree/leaf () (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
                                 :indentation 2)))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer javascript/expression/variable-reference->tree/leaf (projection recursion input input-reference)
  (bind ((output-content (name-of input))
         (output (make-tree/leaf (text/text () (text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/orange*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)))))

(def printer javascript/expression/property-access->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (object-iomap (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/property-access document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (output-of object-iomap)
                                       (make-tree/node (list (make-tree/leaf (text/text () (text/string (property-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*))))))
                                 :separator (text/text () (text/string "." :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                (nreverse child-iomaps)))))

(def printer javascript/expression/constructor-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (object-iomap (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/constructor-invocation document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (make-tree/leaf (text/text () (text/string "new " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (output-of object-iomap)
                                       (make-tree/node (list (make-tree/node (iter (for index :from 0)
                                                                                   (for argument :in-sequence (arguments-of input))
                                                                                   (for child-iomap = (recurse-printer recursion argument `((elt (the sequence (arguments-of (the javascript/expression/constructor-invocation document))) ,index)
                                                                                                                                            ,@(typed-reference (form-type input) input-reference))))
                                                                                   (push child-iomap child-iomaps)
                                                                                   (collect (output-of child-iomap)))
                                                                             :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                             :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                             :separator (text/text () (text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                #+nil
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 1))
                                (nreverse child-iomaps)))))

(def printer javascript/expression/method-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (object-iomap (recurse-printer recursion (object-of input) `((object-of (the java/expression/method-invocation document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (output-of object-iomap)
                                       (make-tree/node (list (make-tree/leaf (text/text () (text/string (method-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*)))
                                                             (make-tree/node (iter (for index :from 0)
                                                                                   (for argument :in-sequence (arguments-of input))
                                                                                   (for child-iomap = (recurse-printer recursion argument `((elt (the sequence (arguments-of (the java/expression/method-invocation document))) ,index)
                                                                                                                                            ,@(typed-reference (form-type input) input-reference))))
                                                                                   (push child-iomap child-iomaps)
                                                                                   (collect (output-of child-iomap)))
                                                                             :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                             :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                                                             :separator (text/text () (text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))))
                                 :separator (text/text () (text/string "." :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                #+nil
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 1))
                                (nreverse child-iomaps)))))

(def printer javascript/literal/string->tree/leaf (projection recursion input input-reference)
  (bind ((output (make-tree/leaf (text/text () (text/string (value-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*))
                                 :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/definition/variable->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (body-iomap (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/variable document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (make-tree/leaf (text/text () (text/string "var" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*)))
                                       (make-tree/leaf (text/text () (text/string "=" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                       (output-of body-iomap))
                                 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                body-iomap
                                (nreverse child-iomaps)))))

(def printer javascript/definition/function->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (body-iomap (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/function document))
                                                                  ,@(typed-reference (form-type input) input-reference))))
         (output (make-tree/node (list (make-tree/leaf (text/text () (text/string "function" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/violet*)))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion argument `((elt (arguments-of (the javascript/definition/function document)) ,index)
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
                                body-iomap
                                #+nil
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 3))
                                (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader javascript/statement/block->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/statement/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/variable-reference->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/property-access->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/constructor-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/expression/method-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/literal/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/definition/variable->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)

(def reader javascript/definition/function->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  input)
