;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) java/statement/block->tree/node ()
  ())

(def (projection e) java/statement/if->tree/node ()
  ())

(def (projection e) java/statement/return->tree/node ()
  ())

(def (projection e) java/expression/variable-reference->string ()
  ())

(def (projection e) java/expression/method-invocation->tree/node  ()
  ())

(def (projection e) java/expression/infix-operator->tree/node ()
  ())

(def (projection e) java/literal/null->string ()
  ())

(def (projection e) java/literal/number->string ()
  ())

(def (projection e) java/literal/character->string ()
  ())

(def (projection e) java/literal/string->string  ()
  ())

(def (projection e) java/declaration/method->tree/node ()
  ())

(def (projection e) java/declaration/argument->tree/node ()
  ())

(def (projection e) java/declaration/qualifier->string ()
  ())

(def (projection e) java/declaration/type->string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/java/statement/block->tree/node ()
  (make-projection 'java/statement/block->tree/node))

(def (function e) make-projection/java/statement/if->tree/node ()
  (make-projection 'java/statement/if->tree/node))

(def (function e) make-projection/java/statement/return->tree/node ()
  (make-projection 'java/statement/return->tree/node))

(def (function e) make-projection/java/expression/variable-reference->string ()
  (make-projection 'java/expression/variable-reference->string))

(def (function e) make-projection/java/expression/method-invocation->tree/node  ()
  (make-projection 'java/expression/method-invocation->tree/node))

(def (function e) make-projection/java/expression/infix-operator->tree/node ()
  (make-projection 'java/expression/infix-operator->tree/node))

(def (function e) make-projection/java/literal/null->string ()
  (make-projection 'java/literal/null->string))

(def (function e) make-projection/java/literal/number->string ()
  (make-projection 'java/literal/number->string))

(def (function e) make-projection/java/literal/character->string ()
  (make-projection 'java/literal/character->string))

(def (function e) make-projection/java/literal/string->string  ()
  (make-projection 'java/literal/string->string))

(def (function e) make-projection/java/declaration/method->tree/node ()
  (make-projection 'java/declaration/method->tree/node))

(def (function e) make-projection/java/declaration/argument->tree/node ()
  (make-projection 'java/declaration/argument->tree/node))

(def (function e) make-projection/java/declaration/qualifier->string ()
  (make-projection 'java/declaration/qualifier->string))

(def (function e) make-projection/java/declaration/type->string ()
  (make-projection 'java/declaration/type->string))

;;;;;;
;;; Construction

(def (macro e) java/statement/block->tree/node ()
  '(make-projection/java/statement/block->tree/node))

(def (macro e) java/statement/if->tree/node ()
  '(make-projection/java/statement/if->tree/node))

(def (macro e) java/statement/return->tree/node ()
  '(make-projection/java/statement/return->tree/node))

(def (macro e) java/expression/variable-reference->string ()
  '(make-projection/java/expression/variable-reference->string))

(def (macro e) java/expression/method-invocation->tree/node  ()
  '(make-projection/java/expression/method-invocation->tree/node))

(def (macro e) java/expression/infix-operator->tree/node ()
  '(make-projection/java/expression/infix-operator->tree/node))

(def (macro e) java/literal/null->string ()
  '(make-projection/java/literal/null->string))

(def (macro e) java/literal/number->string ()
  '(make-projection/java/literal/number->string))

(def (macro e) java/literal/character->string ()
  '(make-projection/java/literal/character->string))

(def (macro e) java/literal/string->string  ()
  '(make-projection/java/literal/string->string))

(def (macro e) java/declaration/method->tree/node ()
  '(make-projection/java/declaration/method->tree/node))

(def (macro e) java/declaration/argument->tree/node ()
  '(make-projection/java/declaration/argument->tree/node))

(def (macro e) java/declaration/qualifier->string ()
  '(make-projection/java/declaration/qualifier->string))

(def (macro e) java/declaration/type->string ()
  '(make-projection/java/declaration/type->string))

;;;;;;
;;; Printer

(def printer java/statement/block->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for iomap = (recurse-printer recursion iomap element
                                                                     `(elt (elements-of ,typed-input-reference) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of iomap)) 2)
                                       (collect (output-of iomap)))
                                 :opening-delimiter (make-text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 ;; KLUDGE:
                                 :closing-delimiter (make-text/string "
}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer java/statement/if->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (condition-iomap (recurse-printer recursion iomap (condition-of input) `(condition-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
         (then-branch-iomap (recurse-printer recursion iomap (then-of input) `(then-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 2)))
         (else-branch-iomap (recurse-printer recursion iomap (else-of input) `(else-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 4)))
         (output (make-tree/node (list* (make-tree/leaf (make-text/string "if" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))
                                        (make-tree/node (children-of (output-of condition-iomap))
                                                        :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                        :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                        :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                        (output-of then-branch-iomap)
                                        (make-tree/leaf (make-text/string "else" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*) :indentation 0)
                                        (when (else-of input)
                                          (list (output-of else-branch-iomap))))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    ;; KLUDGE:
    (setf (indentation-of (output-of then-branch-iomap)) 4)
    (setf (indentation-of (output-of else-branch-iomap)) 4)
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                condition-iomap
                                then-branch-iomap
                                else-branch-iomap
                                (make-iomap/string* input `(the string (form-name ,typed-input-reference)) 0
                                                    output `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))) 0
                                                    2)
                                (make-iomap/string* input `(the string (form-name ,typed-input-reference)) 0
                                                    output `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 3)))) 0
                                                    4)))))

(def printer java/statement/return->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (iomap (recurse-printer recursion iomap (value-of input) `(value-of  ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
         (output (make-tree/node (list (make-tree/leaf (make-text/string "return" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)) (output-of iomap))
                                 :closing-delimiter (make-text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (form-name ,typed-input-reference)) 0
                                                    output `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))) 0
                                                    6)
                                iomap))))

(def printer java/expression/variable-reference->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (name-of input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/orange*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (name-of ,typed-input-reference)) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer java/expression/method-invocation->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (list (make-tree/leaf (make-text/string (method-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion iomap argument
                                                                                           `(elt (the list (arguments-of ,typed-input-reference)) ,index)
                                                                                           `(elt (the list (children-of (the tree/node (elt (the list (children-of (the tree/node ,output-reference))) 1)))) ,index)))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap)))
                                                       :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (make-iomap/string* input `(the string (method-of ,typed-input-reference)) 0
                                                     output `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0)))) 0
                                                     (length (method-of input)))
                                 (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                    output `(elt (the list (children-of (the tree/node ,output-reference))) 1))
                                 (nreverse child-iomaps)))))

(def printer java/expression/infix-operator->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for argument :in-sequence (arguments-of input))
                                       (for iomap = (recurse-printer recursion iomap argument
                                                                     `(elt (the list (arguments-of ,typed-input-reference)) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,(* 2 index))))
                                       (unless (first-iteration-p)
                                         (push (make-iomap/string* input `(the string (operator-of ,typed-input-reference)) 0
                                                                   nil `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) ,(1- (* 2 index)))))) 0
                                                                   (length (operator-of input)))
                                               child-iomaps)
                                         (collect (make-tree/leaf (make-text/string (operator-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*))))
                                       (push iomap child-iomaps)
                                       (collect (output-of iomap)))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (nreverse child-iomaps)))))

(def printer java/literal/null->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output "null"))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer java/literal/number->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (write-to-string (value-of input)))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (write-to-string (the number (value-of ,typed-input-reference)))) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer java/literal/character->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (format nil "'~A'" (value-of input))))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer java/literal/string->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (value-of input)))
    (make-iomap/object projection recursion input input-reference output output-reference)))

(def printer java/declaration/method->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (qualifier-iomap (recurse-printer recursion iomap (qualifier-of input) `(qualifier-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 0)))
         (return-type-iomap (recurse-printer recursion iomap (return-type-of input) `(return-type-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
         (body-iomap (recurse-printer recursion iomap (body-of input) `(body-of ,typed-input-reference) `(elt (the list (children-of (the tree/node ,output-reference))) 4)))
         (output (make-tree/node (list (output-of qualifier-iomap)
                                       (output-of return-type-iomap)
                                       (make-tree/leaf (make-text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))
                                       (make-tree/node (iter (for index :from 0)
                                                             (for argument :in-sequence (arguments-of input))
                                                             (for iomap = (recurse-printer recursion iomap argument
                                                                                           `(elt (arguments-of ,typed-input-reference) ,index)
                                                                                           `(elt (the list (children-of (the tree/node (elt (the list (children-of (the tree/node ,output-reference))) 3)))) ,index)))
                                                             (push iomap child-iomaps)
                                                             (collect (output-of iomap)))
                                                       :opening-delimiter (make-text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :closing-delimiter (make-text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                       (output-of body-iomap))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    ;; KLUDGE:
    (setf (indentation-of (output-of body-iomap)) 0)
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 qualifier-iomap
                                 return-type-iomap
                                 body-iomap
                                 (make-iomap/string* input `(the string (name-of ,typed-input-reference)) 0
                                                     output `(the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 2)))) 0
                                                     (length (name-of input)))
                                 (make-iomap/object projection recursion (arguments-of input) `(arguments-of ,typed-input-reference)
                                                    output `(elt (the list (children-of (the tree/node ,output-reference))) 3))
                                 (nreverse child-iomaps)))))

(def printer java/declaration/argument->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (type-iomap (recurse-printer recursion iomap (slot-value input 'type) `(slot-value ,typed-input-reference 'type) `(elt (the list (children-of (the tree/node ,output-reference))) 0)))
         (output (make-tree/node (list (output-of type-iomap)
                                       (make-tree/leaf (make-text/string (name-of input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                type-iomap
                                (make-iomap/string* input `(the string (name-of ,typed-input-reference)) 0
                                                    output `(the string (content-of (the tree/node (elt (the list (children-of (the tree/node ,output-reference))) 1)))) 0
                                                    (length (name-of input)))))))

(def printer java/declaration/qualifier->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (name-of input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (name-of ,typed-input-reference)) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

(def printer java/declaration/type->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (output-content (name-of input))
         (output (make-tree/leaf (make-text/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/violet*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string* input `(the string (name-of ,typed-input-reference)) 0
                                                    output-content `(the string (content-of (the tree/leaf ,output-reference))) 0
                                                    (length output-content))))))

;;;;;;
;;; Reader

(def reader java/statement/block->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/statement/if->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/statement/return->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/expression/variable-reference->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/expression/method-invocation->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/expression/infix-operator->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/literal/null->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/literal/number->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/literal/character->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/literal/string->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/declaration/method->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/declaration/argument->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/declaration/qualifier->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader java/declaration/type->string (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
