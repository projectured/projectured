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

(def projection javascript/declaration/variable->tree/node ()
  ())

(def projection javascript/declaration/function->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/javascript/statement/block->tree/node ()
  (make-projection 'javascript/statement/block->tree/node))

(def (function e) make-projection/javascript/statement/top-level->tree/node ()
  (make-projection 'javascript/statement/top-level->tree/node))

(def (function e) make-projection/javascript/expression/variable-reference->tree/leaf ()
  (make-projection 'javascript/expression/variable-reference->tree/leaf))

(def (function e) make-projection/javascript/expression/property-access->tree/node ()
  (make-projection 'javascript/expression/property-access->tree/node))

(def (function e) make-projection/javascript/expression/constructor-invocation->tree/node ()
  (make-projection 'javascript/expression/constructor-invocation->tree/node))

(def (function e) make-projection/javascript/expression/method-invocation->tree/node ()
  (make-projection 'javascript/expression/method-invocation->tree/node))

(def (function e) make-projection/javascript/literal/string->tree/leaf ()
  (make-projection 'javascript/literal/string->tree/leaf))

(def (function e) make-projection/javascript/declaration/variable->tree/node ()
  (make-projection 'javascript/declaration/variable->tree/node))

(def (function e) make-projection/javascript/declaration/function->tree/node ()
  (make-projection 'javascript/declaration/function->tree/node))

;;;;;;
;;; Printer

(def printer javascript/statement/block->tree/node (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt (elements-of ,typed-input-reference) ,index)))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 2)
                                       (collect (output-of iomap)))
                                 :indentation 2
                                 :opening-delimiter (make-text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 ;; KLUDGE:
                                 :closing-delimiter (make-text/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer javascript/statement/top-level->tree/node (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt (elements-of ,typed-input-reference) ,index)))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 0)
                                       (collect (output-of iomap)))
                                 :indentation 2)))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer javascript/expression/variable-reference->tree/leaf (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (name-of input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/orange*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list (make-iomap/object projection recursion input input-reference output)
                               (make-iomap/string input `(the string (name-of ,typed-input-reference)) 0
                                                  output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                  (length output-content))))))

(def printer javascript/expression/property-access->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (object-iomap (recurse-printer recursion (object-of input) `(object-of ,typed-input-reference)))
         (output (make-tree/node (list (output-of object-iomap)
                                       (make-tree/node (list (make-tree/leaf (make-text/string (property-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*)))))
                                 :separator (make-text/string "." :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                (make-iomap/string input `(the string (property-of ,typed-input-reference)) 0
                                                   output `(the string (content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 0)))) 0
                                                   (length (property-of input)))
                                (nreverse child-iomaps)))))

(def printer javascript/expression/constructor-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (object-iomap (recurse-printer recursion (object-of input) `(object-of ,typed-input-reference)))
         (output (make-tree/node (list (make-tree/leaf (make-text/string "new" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))
                                       (output-of object-iomap)
                                       (make-tree/node (list (make-tree/node (iter (for index :from 0)
                                                                                   (for argument :in-sequence (arguments-of input))
                                                                                   (for child-iomap = (recurse-printer recursion argument
                                                                                                                       `(elt (the sequence (arguments-of ,typed-input-reference)) ,index)))
                                                                                   (push child-iomap child-iomaps)
                                                                                   (collect (output-of child-iomap)))
                                                                             :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                             :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                             :separator (make-text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 1))
                                (nreverse child-iomaps)))))

(def printer javascript/expression/method-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (object-iomap (recurse-printer recursion (object-of input) `(object-of ,typed-input-reference)))
         (output (make-tree/node (list (output-of object-iomap)
                                       (make-tree/node (list (make-tree/leaf (make-text/string (method-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*))
                                                             (make-tree/node (iter (for index :from 0)
                                                                                   (for argument :in-sequence (arguments-of input))
                                                                                   (for child-iomap = (recurse-printer recursion argument
                                                                                                                       `(elt (the sequence (arguments-of ,typed-input-reference)) ,index)))
                                                                                   (push child-iomap child-iomaps)
                                                                                   (collect (output-of child-iomap)))
                                                                             :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                             :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                                             :separator (make-text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))
                                 :separator (make-text/string "." :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                (make-iomap/string input `(the string (method-of ,typed-input-reference)) 0
                                                   output `(the string (content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 0)))) 0
                                                   (length (method-of input)))
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 1))
                                (nreverse child-iomaps)))))

(def printer javascript/literal/string->tree/leaf (projection recursion input input-reference)
  (bind ((output (make-tree/leaf (make-text/string (value-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                 :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/declaration/variable->tree/node (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (body-iomap (recurse-printer recursion (body-of input) `(body-of ,typed-input-reference)))
         (output (make-tree/node (list (make-tree/leaf (make-text/string "var" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))
                                       (make-tree/leaf (make-text/string (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/red*))
                                       (make-tree/leaf (make-text/string "=" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                       (output-of body-iomap))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                body-iomap
                                (make-iomap/string input `(the string (name-of ,typed-input-reference)) 0
                                                   output `(the string (content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 0
                                                   (length (name-of input)))
                                (nreverse child-iomaps)))))

(def printer javascript/declaration/function->tree/node (projection recursion input input-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (body-iomap (recurse-printer recursion (body-of input) `(body-of ,typed-input-reference)))
         (output (make-tree/node (list (make-tree/leaf (make-text/string "function" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))
                                       (make-tree/leaf (make-text/string (name-of input) :font *font/ubuntu/monospace/italic/18* :font-color *color/solarized/violet*))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion iomap argument
                                                                                           `(elt (arguments-of ,typed-input-reference) ,index)))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap)))
                                                       :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                       (output-of body-iomap))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    ;; KLUDGE:
    (setf (indentation-of (output-of body-iomap)) 0)
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output)
                                body-iomap
                                (make-iomap/string input `(the string (name-of ,typed-input-reference)) 0
                                                   output `(the string (content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 0
                                                   (length (name-of input)))
                                (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                   output `(elt (the sequence (children-of (the tree/node ,output-reference))) 3))
                                (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader javascript/statement/block->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/statement/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/expression/variable-reference->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/expression/property-access->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/expression/constructor-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/expression/method-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/literal/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/declaration/variable->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)

(def reader javascript/declaration/function->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion input printer-iomap))
  nil)
