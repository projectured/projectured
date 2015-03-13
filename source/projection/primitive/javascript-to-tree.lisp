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
;;; Forward mapper

(def function forward-mapper/javascript/statement/block->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node (printer-output (the javascript/statement/block document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/statement/top-level->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the javascript/statement/top-level document)))
        (the ?type (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((element-index (* 2 ?element-index))
              (element-iomap (elt (child-iomaps-of printer-iomap) ?element-index))
              (element-output (output-of element-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(form-type element-output) (elt (the sequence document) ,element-index)))
                 ?rest
                 element-iomap)))
      (((the tree/node (printer-output (the javascript/statement/top-level document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/expression/variable-reference->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf (printer-output (the javascript/expression/variable-reference document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/expression/property-access->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node (printer-output (the javascript/expression/property-access document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/expression/constructor-invocation->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node (printer-output (the javascript/expression/constructor-invocation document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/expression/method-invocation->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (method-of (the javascript/expression/method-invocation document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/node (elt (the sequence document) 1))
         (the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the ?type (object-of (the javascript/expression/method-invocation document)))
        . ?rest)
       (bind ((object-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(form-type (output-of object-iomap)) (elt (the sequence document) 0)))
                 ?rest
                 object-iomap)))
      (((the tree/node (printer-output (the javascript/expression/method-invocation document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/literal/string->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf (printer-output (the javascript/literal/string document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/definition/variable->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node (printer-output (the javascript/definition/variable document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/javascript/definition/function->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node (printer-output (the javascript/definition/function document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

;;;;;;
;;; Forward mapper

(def function backward-mapper/javascript/statement/block->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/node (printer-output (the javascript/statement/block document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/statement/top-level->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((element-index (floor ?element-index 2))
              (element-iomap (elt (child-iomaps-of printer-iomap) element-index))
              (element (elt (elements-of printer-input) element-index)))
         (values `((the sequence (elements-of (the javascript/statement/top-level document)))
                   (the ,(form-type element) (elt (the sequence document) ,element-index)))
                 ?rest
                 element-iomap)))
      (?a
       (append `((the tree/node (printer-output (the javascript/statement/top-level document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/expression/variable-reference->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/leaf (printer-output (the javascript/expression/variable-reference document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/expression/property-access->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/node (printer-output (the javascript/expression/property-access document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/expression/constructor-invocation->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/node (printer-output (the javascript/expression/constructor-invocation document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/expression/method-invocation->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) 1))
        (the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (method-of (the javascript/expression/method-invocation document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) 0))
        . ?rest)
       (bind ((object-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the ,(form-type (object-of printer-input)) (object-of (the javascript/expression/method-invocation document))))
                 ?rest
                 object-iomap)))
      (?a
       (append `((the tree/node (printer-output (the javascript/expression/method-invocation document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/literal/string->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/leaf (printer-output (the javascript/literal/string document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/definition/variable->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/node (printer-output (the javascript/definition/variable document) ,projection ,recursion))) reference)))))

(def function backward-mapper/javascript/definition/function->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the tree/node (printer-output (the javascript/definition/function document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer javascript/statement/block->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil child-iomaps)
                                                (selection-of input)
                                                'forward-mapper/javascript/statement/block->tree/node)))
         (output (as (make-tree/node (iter (for index :from 0)
                                           (for element :in-sequence (elements-of input))
                                           (for element-iomap = (recurse-printer recursion element `((elt (elements-of (the javascript/statement/block document)) ,index)
                                                                                                     ,@(typed-reference (form-type input) input-reference))))
                                           (push element-iomap child-iomaps)
                                           (setf (indentation-of (output-of element-iomap)) 2)
                                           (collect (output-of element-iomap))
                                           (collect (tree/leaf () (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))))
                                     :indentation 2
                                     :opening-delimiter (text/text () (text/string "{" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :closing-delimiter (text/text () (text/newline) (text/string "}" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output child-iomaps)))

(def printer javascript/statement/top-level->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence (elements-of input))
                                   (for element-iomap = (recurse-printer recursion element `((elt (elements-of (the javascript/statement/top-level document)) ,index)
                                                                                             ,@(typed-reference (form-type input) input-reference))))
                                   (unless (first-iteration-p)
                                     (setf (indentation-of (output-of element-iomap)) 0))
                                   (collect element-iomap))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/javascript/statement/top-level->tree/node)))
         (output (as (make-tree/node (iter (for element-iomap :in-sequence (va element-iomaps))
                                           (collect (output-of element-iomap))
                                           (collect (tree/leaf ()
                                                      (text/text () (text/string ";" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))))
                                     :indentation 2
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer javascript/expression/variable-reference->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/javascript/expression/variable-reference->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (name-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/orange*))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/expression/property-access->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (object-iomap (as (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/property-access document))
                                                                          ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil child-iomaps)
                                                (selection-of input)
                                                'forward-mapper/javascript/expression/property-access->tree/node)))
         (output (as (make-tree/node (list (output-of (va object-iomap))
                                           (make-tree/node (list (make-tree/leaf (text/text () (text/string (property-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/violet*))))
                                                           :selection (as (nthcdr 2 (va output-selection)))))
                                     :separator (text/text () (text/string "." :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output child-iomaps)))

(def printer javascript/expression/constructor-invocation->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (object-iomap (as (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/constructor-invocation document))
                                                                          ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil child-iomaps)
                                                (selection-of input)
                                                'forward-mapper/javascript/expression/constructor-invocation->tree/node)))
         (output (as (make-tree/node (list (make-tree/leaf (text/text () (text/string "new " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*)))
                                           (output-of (va object-iomap))
                                           (make-tree/node (list (make-tree/node (iter (for index :from 0)
                                                                                       (for argument :in-sequence (arguments-of input))
                                                                                       (for child-iomap = (recurse-printer recursion argument `((elt (the sequence (arguments-of (the javascript/expression/constructor-invocation document))) ,index)
                                                                                                                                                ,@(typed-reference (form-type input) input-reference))))
                                                                                       (push child-iomap child-iomaps)
                                                                                       (collect (output-of child-iomap)))
                                                                                 :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :separator (text/text () (text/string ", " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :selection (as (nthcdr 2 (va output-selection)))))))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output child-iomaps)))

(def printer javascript/expression/method-invocation->tree/node (projection recursion input input-reference)
  (bind ((argument-iomaps (as (iter (for index :from 0)
                                    (for argument :in-sequence (arguments-of input))
                                    (collect (recurse-printer recursion argument `((elt (the sequence (arguments-of (the javascript/expression/method-invocation document))) ,index)
                                                                                   ,@(typed-reference (form-type input) input-reference)))))))
         (object-iomap (as (recurse-printer recursion (object-of input) `((object-of (the javascript/expression/method-invocation document))
                                                                          ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list* (va object-iomap) (va argument-iomaps))))
                                                (selection-of input)
                                                'forward-mapper/javascript/expression/method-invocation->tree/node)))
         (output (as (make-tree/node (list (output-of (va object-iomap))
                                           (make-tree/node (list (tree/leaf (:selection (as (nthcdr 4 (va output-selection))))
                                                                   (text/text (:selection (as (nthcdr 5 (va output-selection))))
                                                                     (text/string (method-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/violet*)))
                                                                 (make-tree/node (mapcar 'output-of (va argument-iomaps))
                                                                                 :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :separator (text/text () (text/string ", " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                                                 :selection (as (nthcdr 4 (va output-selection)))))
                                                           :selection (as (nthcdr 2 (va output-selection)))))
                                     :separator (text/text () (text/string "." :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (list* (va object-iomap) (va argument-iomaps))))))

(def printer javascript/literal/string->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/javascript/literal/string->tree/leaf)))
         (output (as (make-tree/leaf (text/text (:selection (as (nthcdr 1 (va output-selection))))
                                       (text/string (value-of input) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))
                                     :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer javascript/definition/variable->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (body-iomap (as (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/variable document))
                                                                      ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (list* body-iomap child-iomaps))
                                                (selection-of input)
                                                'forward-mapper/javascript/definition/variable->tree/node)))
         (output (as (make-tree/node (list (make-tree/leaf (text/text () (text/string "var" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*)))
                                           (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/red*)))
                                           (make-tree/leaf (text/text () (text/string "=" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                           (output-of (va body-iomap)))
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (list* body-iomap child-iomaps))))

(def printer javascript/definition/function->tree/node (projection recursion input input-reference)
  (bind ((child-iomaps nil)
         (body-iomap (as (recurse-printer recursion (body-of input) `((body-of (the javascript/definition/function document))
                                                                      ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (list* body-iomap child-iomaps))
                                                (selection-of input)
                                                'forward-mapper/javascript/definition/function->tree/node)))
         (output (as (make-tree/node (list (make-tree/leaf (text/text () (text/string "function" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*)))
                                           (make-tree/leaf (text/text () (text/string (name-of input) :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/violet*)))
                                           (make-tree/node (iter (for index :from 0)
                                                                 (for argument :in-sequence (arguments-of input))
                                                                 (for element-iomap = (recurse-printer recursion argument `((elt (arguments-of (the javascript/definition/function document)) ,index)
                                                                                                                            ,@(typed-reference (form-type input) input-reference))))
                                                                 (push element-iomap child-iomaps)
                                                                 (collect (output-of element-iomap)))
                                                           :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                                           :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                           (output-of (va body-iomap)))
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (setf (indentation-of (output-of (va body-iomap))) 0)
    (make-iomap/compound projection recursion input input-reference output (list* body-iomap child-iomaps))))

;;;;;;
;;; Reader

(def reader javascript/statement/block->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/statement/block->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/statement/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/javascript/statement/top-level->tree/node 'backward-mapper/javascript/statement/top-level->tree/node)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/statement/top-level->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/expression/variable-reference->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/expression/variable-reference->tree/leaf nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/expression/property-access->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/expression/property-access->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/expression/constructor-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/expression/constructor-invocation->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/expression/method-invocation->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (method-of (the javascript/expression/method-invocation document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/expression/method-invocation->tree/node operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader javascript/literal/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/literal/string->tree/leaf nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/definition/variable->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/definition/variable->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader javascript/definition/function->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/javascript/definition/function->tree/node nil)
                  (make-command/nothing (gesture-of input))))
