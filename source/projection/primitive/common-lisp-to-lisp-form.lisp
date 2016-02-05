;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection common-lisp/insertion->lisp-form/insertion ()
  ())

;; TODO: rename to ->lisp-form/base?
(def projection common-lisp/constant->lisp-form/string ()
  ())

(def projection common-lisp/variable-reference->lisp-form/symbol ()
  ())

(def projection common-lisp/function-reference->lisp-form/symbol ()
  ())

(def projection common-lisp/if->lisp-form/list ()
  ())

(def projection common-lisp/the->lisp-form/list ()
  ())

(def projection common-lisp/progn->lisp-form/list ()
  ())

(def projection common-lisp/lexical-variable-binding->lisp-form/list ()
  ())

(def projection common-lisp/let->lisp-form/list ()
  ())

(def projection common-lisp/application->lisp-form/list ()
  ())

(def projection common-lisp/special-variable-definition->lisp-form/list ()
  ())

(def projection common-lisp/function-definition->lisp-form/list ()
  ())

(def projection common-lisp/lambda-function->lisp-form/list ()
  ())

(def projection common-lisp/function-argument->lisp-form/symbol ()
  ())

(def projection common-lisp/comment->lisp-form/comment ()
  ())

(def projection common-lisp/toplevel->lisp-form/toplevel ()
  ())

;;;;;;
;;; Construction

(def function make-projection/common-lisp/insertion->lisp-form/insertion ()
  (make-projection 'common-lisp/insertion->lisp-form/insertion))

(def function make-projection/common-lisp/constant->lisp-form/string ()
  (make-projection 'common-lisp/constant->lisp-form/string))

(def function make-projection/common-lisp/variable-reference->lisp-form/symbol ()
  (make-projection 'common-lisp/variable-reference->lisp-form/symbol))

(def function make-projection/common-lisp/function-reference->lisp-form/symbol ()
  (make-projection 'common-lisp/function-reference->lisp-form/symbol))

(def function make-projection/common-lisp/if->lisp-form/list ()
  (make-projection 'common-lisp/if->lisp-form/list))

(def function make-projection/common-lisp/the->lisp-form/list ()
  (make-projection 'common-lisp/the->lisp-form/list))

(def function make-projection/common-lisp/progn->lisp-form/list ()
  (make-projection 'common-lisp/progn->lisp-form/list))

(def function make-projection/common-lisp/lexical-variable-binding->lisp-form/list ()
  (make-projection 'common-lisp/lexical-variable-binding->lisp-form/list))

(def function make-projection/common-lisp/let->lisp-form/list ()
  (make-projection 'common-lisp/let->lisp-form/list))

(def function make-projection/common-lisp/application->lisp-form/list ()
  (make-projection 'common-lisp/application->lisp-form/list))

(def function make-projection/common-lisp/special-variable-definition->lisp-form/list ()
  (make-projection 'common-lisp/special-variable-definition->lisp-form/list))

(def function make-projection/common-lisp/function-definition->lisp-form/list ()
  (make-projection 'common-lisp/function-definition->lisp-form/list))

(def function make-projection/common-lisp/lambda-function->lisp-form/list ()
  (make-projection 'common-lisp/lambda-function->lisp-form/list))

(def function make-projection/common-lisp/function-argument->lisp-form/symbol ()
  (make-projection 'common-lisp/function-argument->lisp-form/symbol))

(def function make-projection/common-lisp/comment->lisp-form/comment ()
  (make-projection 'common-lisp/comment->lisp-form/comment))

(def function make-projection/common-lisp/toplevel->lisp-form/toplevel ()
  (make-projection 'common-lisp/toplevel->lisp-form/toplevel))

;;;;;;
;;; Construction

(def macro common-lisp/insertion->lisp-form/insertion ()
  '(make-projection/common-lisp/insertion->lisp-form/insertion))

(def macro common-lisp/constant->lisp-form/string ()
  '(make-projection/common-lisp/constant->lisp-form/string))

(def macro common-lisp/variable-reference->lisp-form/symbol ()
  '(make-projection/common-lisp/variable-reference->lisp-form/symbol))

(def macro common-lisp/function-reference->lisp-form/symbol ()
  '(make-projection/common-lisp/function-reference->lisp-form/symbol))

(def macro common-lisp/if->lisp-form/list ()
  '(make-projection/common-lisp/if->lisp-form/list))

(def macro common-lisp/the->lisp-form/list ()
  '(make-projection/common-lisp/the->lisp-form/list))

(def macro common-lisp/progn->lisp-form/list ()
  '(make-projection/common-lisp/progn->lisp-form/list))

(def macro common-lisp/lexical-variable-binding->lisp-form/list ()
  '(make-projection/common-lisp/lexical-variable-binding->lisp-form/list))

(def macro common-lisp/let->lisp-form/list ()
  '(make-projection/common-lisp/let->lisp-form/list))

(def macro common-lisp/application->lisp-form/list ()
  '(make-projection/common-lisp/application->lisp-form/list))

(def macro common-lisp/special-variable-definition->lisp-form/list ()
  '(make-projection/common-lisp/special-variable-definition->lisp-form/list))

(def macro common-lisp/function-definition->lisp-form/list ()
  '(make-projection/common-lisp/function-definition->lisp-form/list))

(def macro common-lisp/lambda-function->lisp-form/list ()
  '(make-projection/common-lisp/lambda-function->lisp-form/list))

(def macro common-lisp/function-argument->lisp-form/symbol ()
  '(make-projection/common-lisp/function-argument->lisp-form/symbol))

(def macro common-lisp/comment->lisp-form/comment ()
  '(make-projection/common-lisp/comment->lisp-form/comment))

(def macro common-lisp/toplevel->lisp-form/toplevel ()
  '(make-projection/common-lisp/toplevel->lisp-form/toplevel))

;;;;;;
;;; Forward mapper

(def function forward-mapper/common-lisp/insertion->lisp-form/insertion (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (value-of (the common-lisp/insertion document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (value-of (the lisp-form/insertion document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the lisp-form/insertion (printer-output (the common-lisp/insertion document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/constant->lisp-form/string (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the ?type (value-of (the common-lisp/constant document))) . ?rest)
       ?rest))
    #+nil
    (pattern-case reference
      (((the string (value-of (the common-lisp/constant document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (value-of (the lisp-form/string document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the ?type (value-of (the common-lisp/constant document)))
        (the string (write-to-string (the ?tpye document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the ,?type (value-of (the lisp-form/number document)))
         (the string (write-to-string (the ,?type document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the ?type (printer-output (the common-lisp/constant document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/variable-reference->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/symbol (printer-output (the common-lisp/variable-reference document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/function-reference->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/symbol (printer-output (the common-lisp/function-reference document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/if->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the common-lisp/if document))
       `((the lisp-form/list document)))
      (((the ?type (?reader (the common-lisp/if document)))
        . ?rest)
       (bind ((slot-iomap (elt (child-iomaps-of printer-iomap)
                               (ecase ?reader
                                 (condition-of 0)
                                 (then-of 1)
                                 (else-of 2))))
              (slot-output (output-of slot-iomap))
              (index (ecase ?reader
                       (condition-of 1)
                       (then-of 2)
                       (else-of 3))))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type slot-output) (elt (the sequence document) ,index)))
                 ?rest
                 slot-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/if document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/the->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list (printer-output (the common-lisp/the document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/progn->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the common-lisp/progn document))
       `((the lisp-form/list document)))
      (((the sequence (body-of (the common-lisp/progn document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?index))
              (element-output (output-of element-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type element-output) (elt (the sequence document) ,(1+ ?index))))
                 ?rest
                 element-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/progn document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/lexical-variable-binding->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/symbol (name-of (the common-lisp/lexical-variable-binding document)))
        . ?rest)
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/symbol (elt (the sequence document) 0))
         ,@?rest))
      (((the ?type (value-of (the common-lisp/lexical-variable-binding document)))
        . ?rest)
       (bind ((value-iomap (elt (child-iomaps-of printer-iomap) 0))
              (value (output-of value-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type value) (elt (the sequence document) 1)))
                 ?rest
                 value-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/lexical-variable-binding document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/let->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (bindings-of (the common-lisp/let document)))
        (the common-lisp/lexical-variable-binding (elt (the sequence document) ?index))
        . ?rest)
       (bind ((binding-iomap (elt (child-iomaps-of printer-iomap) ?index)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the lisp-form/list (elt (the sequence document) 1))
                   (the sequence (elements-of (the lisp-form/list document)))
                   (the lisp-form/list (elt (the sequence document) ,?index)))
                 ?rest
                 binding-iomap)))
      (((the sequence (body-of (the common-lisp/let document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((body-element-iomap (elt (child-iomaps-of printer-iomap) (+ ?index (length (bindings-of printer-input)))))
              (body-element (output-of body-element-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type body-element) (elt (the sequence document) ,(+ ?index 2))))
                 ?rest
                 body-element-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/let document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/application->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the common-lisp/application document))
       `((the lisp-form/list document)))
      (((the lisp-form/symbol (operator-of (the common-lisp/application document)))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/symbol (elt (the sequence document) 0))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
        (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
        (the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/symbol (elt (the sequence document) 0))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (arguments-of (the common-lisp/application document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((argument-iomap (elt (child-iomaps-of printer-iomap) ?index))
              (argument-output (output-of argument-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type argument-output) (elt (the sequence document) ,(1+ ?index))))
                 ?rest
                 argument-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/application document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/special-variable-definition->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/symbol (name-of (the common-lisp/special-variable-definition document)))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/symbol (elt (the sequence document) 1))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the ?value-type (value-of (the common-lisp/special-variable-definition document)))
        . ?rest)
       (bind ((value-iomap (elt (child-iomaps-of printer-iomap) 0))
              (value (output-of value-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type value) (elt (the sequence document) 2)))
                 ?rest
                 value-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/special-variable-definition document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/function-definition->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the common-lisp/function-definition document))
       `((the lisp-form/list document)))
      (((the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/symbol (elt (the sequence document) 1))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the string (documentation-of (the common-lisp/function-definition document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the sequence (elements-of (the lisp-form/list document)))
         (the lisp-form/string (elt (the sequence document) 3))
         (the string (value-of (the lisp-form/string document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (bindings-of (the common-lisp/function-definition document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((iomap-index ?index)
              (binding-iomap (elt (child-iomaps-of printer-iomap) iomap-index))
              (binding-output (output-of binding-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the lisp-form/list (elt (the sequence document) 2))
                   (the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type binding-output) (elt (the sequence document) ,?index)))
                 ?rest
                 binding-iomap)))
      (((the sequence (body-of (the common-lisp/function-definition document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((iomap-index (+ (length (bindings-of printer-input)) ?index))
              (body-iomap (elt (child-iomaps-of printer-iomap) iomap-index))
              (body-output (output-of body-iomap)))
         (values `((the sequence (elements-of (the lisp-form/list document)))
                   (the ,(document-type body-output) (elt (the sequence document) ,(+ ?index 4))))
                 ?rest
                 body-iomap)))
      (((the lisp-form/list (printer-output (the common-lisp/function-definition document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/lambda-function->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list (printer-output (the common-lisp/lambda-function document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/function-argument->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/symbol (name-of (the common-lisp/function-argument document)))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the lisp-form/symbol (printer-output (the common-lisp/function-argument document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/comment->lisp-form/comment (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the text/text (content-of (the common-lisp/comment document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the text/text (content-of (the lisp-form/comment document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the lisp-form/comment (printer-output (the common-lisp/comment document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/common-lisp/toplevel->lisp-form/toplevel (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (body-of (the common-lisp/toplevel document)))
        (the ?type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?child-index)))
         (values `((the sequence (elements-of (the lisp-form/toplevel document)))
                   (the ,(document-type (output-of element-iomap)) (elt (the sequence document) ,?child-index)))
                 ?rest
                 element-iomap)))
      (((the lisp-form/toplevel (printer-output (the common-lisp/toplevel document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/common-lisp/insertion->lisp-form/insertion (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (value-of (the lisp-form/insertion document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (value-of (the common-lisp/insertion document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the lisp-form/insertion (printer-output (the common-lisp/insertion document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/constant->lisp-form/string (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (append `((the ,(document-type (output-of printer-iomap)) (value-of (the common-lisp/constant document)))) reference)
    #+nil
    (pattern-case reference
      (((the string (value-of (the lisp-form/string document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (value-of (the common-lisp/constant document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the ?type (value-of (the lisp-form/number document)))
        (the string (write-to-string (the ?type document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the ,?type (value-of (the common-lisp/constant document)))
         (the string (write-to-string (the ,?type document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the ,(document-type (output-of printer-iomap)) (printer-output (the common-lisp/constant document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/variable-reference->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap))
         (variable (variable-of printer-input)))
    (pattern-case reference
      (((the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the ,(document-type variable) (variable-of (the common-lisp/variable-reference document)))
         (the lisp-form/symbol (name-of (the ,(document-type variable) document)))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the lisp-form/symbol (printer-output (the common-lisp/variable-reference document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/function-reference->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap))
         (function (function-of printer-input)))
    (pattern-case reference
      (((the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the ,(document-type function) (function-of (the common-lisp/function-reference document)))
         (the lisp-form/symbol (name-of (the ,(document-type function) document)))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the lisp-form/symbol (printer-output (the common-lisp/function-reference document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/if->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list document))
       `((the common-lisp/if document)))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) ?index (?if (> ?index 0))))
        . ?rest)
       (bind ((slot-reader (ecase ?index
                             (1 'condition-of)
                             (2 'then-of)
                             (3 'else-of)))
              (slot-value (funcall slot-reader printer-input))
              (element-iomap (elt (child-iomaps-of printer-iomap) (1- ?index))))
         (values `((the ,(document-type slot-value) (,slot-reader (the common-lisp/if document))))
                 ?rest
                 element-iomap)))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/if document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/the->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/the document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/progn->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list document))
       `((the common-lisp/progn document)))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) ?index (?if (> ?index 0))))
        . ?rest)
       (bind ((element-index (1- ?index))
              (element (elt (body-of printer-input) element-index))
              (element-iomap (elt (child-iomaps-of printer-iomap) element-index)))
         (values `((the sequence (body-of (the common-lisp/progn document)))
                   (the ,(document-type element) (elt (the sequence document) ,element-index)))
                 ?rest
                 element-iomap)))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/progn document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/lexical-variable-binding->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/symbol (elt (the sequence document) 0))
        . ?rest)
       `((the lisp-form/symbol (name-of (the common-lisp/lexical-variable-binding document)))
         ,@?rest))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) 1))
        . ?rest)
       (bind ((value-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the ,(document-type (value-of printer-input)) (value-of (the common-lisp/lexical-variable-binding document))))
                 ?rest
                 value-iomap)))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/lexical-variable-binding document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/let->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/list (elt (the sequence document) 1))
        (the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((binding-iomap (elt (child-iomaps-of printer-iomap) ?index)))
         (values `((the sequence (bindings-of (the common-lisp/let document)))
                   (the common-lisp/lexical-variable-binding (elt (the sequence document) ,?index)))
                 ?rest
                 binding-iomap)))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (if (zerop ?index)
           (append `((the lisp-form/list (printer-output (the common-lisp/let document) ,projection ,recursion))) reference)
           (bind ((index (- ?index 2))
                  (body-element-iomap (elt (child-iomaps-of printer-iomap) (+ index (length (bindings-of printer-input)))))
                  (body-element (elt (body-of printer-input) index)))
             (values `((the sequence (body-of (the common-lisp/let document)))
                       (the ,(document-type body-element) (elt (the sequence document) ,index)))
                     ?rest
                     body-element-iomap))))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/let document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/application->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list document))
       `((the common-lisp/application document)))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/symbol (elt (the sequence document) 0))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (etypecase (operator-of printer-input)
         (lisp-form/symbol `((the lisp-form/symbol (operator-of (the common-lisp/application document)))
                             (the string (name-of (the lisp-form/symbol document)))
                             (the string (subseq (the string document) ,?start-index ,?end-index))))
         (common-lisp/function-reference `((the common-lisp/function-reference (operator-of (the common-lisp/application document)))
                                           (the common-lisp/function-definition (function-of (the common-lisp/function-reference document)))
                                           (the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
                                           (the string (name-of (the lisp-form/symbol document)))
                                           (the string (subseq (the string document) ,?start-index ,?end-index))))))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?type (elt (the sequence document) ?index (?if (> ?index 0))))
        . ?rest)
       (bind ((argument-index (1- ?index))
              (argument (elt (arguments-of printer-input) argument-index))
              (child-iomap (elt (child-iomaps-of printer-iomap) argument-index)))
         (values `((the sequence (arguments-of (the common-lisp/application document)))
                   (the ,(document-type argument) (elt (the sequence document) ,argument-index)))
                 ?rest
                 child-iomap)))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/application document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/special-variable-definition->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/symbol (elt (the sequence document) 1))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the lisp-form/symbol (name-of (the common-lisp/special-variable-definition document)))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?element-type (elt (the sequence document) 2))
        . ?rest)
       (bind ((value (value-of printer-input))
              (value-iomap (elt (child-iomaps-of printer-iomap) 0)))
         (values `((the ,(document-type value) (value-of (the common-lisp/special-variable-definition document))))
                 ?rest
                 value-iomap)))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/special-variable-definition document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/function-definition->lisp-form/list (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the lisp-form/list document))
       `((the common-lisp/function-definition document)))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/symbol (elt (the sequence document) 1))
        (the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the lisp-form/string (elt (the sequence document) 3))
        (the string (value-of (the lisp-form/string document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the string (documentation-of (the common-lisp/function-definition document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?child-type (elt (the sequence document) ?element-index))
        . ?rest)
       (cond ((= ?element-index 2)
              (pattern-case ?rest
                (((the sequence (elements-of (the lisp-form/list document)))
                  (the ?type (elt (the sequence document) ?binding-index))
                  . ?rest)
                 (bind ((binding (elt (bindings-of printer-input) ?binding-index))
                        (iomap-index ?binding-index)
                        (child-iomap (elt (child-iomaps-of printer-iomap) iomap-index)))
                   (values `((the sequence (bindings-of (the common-lisp/function-definition document)))
                             (the ,(document-type binding) (elt (the sequence document) ,?binding-index)))
                           ?rest
                           child-iomap)))
                (?a
                 (append `((the lisp-form/list (printer-output (the common-lisp/function-definition document) ,projection ,recursion))) reference))))
             ((>= ?element-index 4)
              (bind ((element-index (- ?element-index 4))
                     (element (elt (body-of printer-input) element-index))
                     (iomap-index (+ (length (bindings-of printer-input)) element-index))
                     (child-iomap (elt (child-iomaps-of printer-iomap) iomap-index)))
                (values `((the sequence (body-of (the common-lisp/function-definition document)))
                          (the ,(document-type element) (elt (the sequence document) ,element-index)))
                        ?rest
                        child-iomap)))
             (t (append `((the lisp-form/list (printer-output (the common-lisp/function-definition document) ,projection ,recursion))) reference))))
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/function-definition document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/lambda-function->lisp-form/list (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (?a
       (append `((the lisp-form/list (printer-output (the common-lisp/lambda-function document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/function-argument->lisp-form/symbol (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the lisp-form/symbol (name-of (the common-lisp/function-argument document)))
         (the string (name-of (the lisp-form/symbol document)))
         (the string (subseq (the string document) ,?start-index ,?end-index))))
      (?a
       (append `((the lisp-form/symbol (printer-output (the common-lisp/function-argument document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/comment->lisp-form/comment (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the text/text (content-of (the lisp-form/comment document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the text/text (content-of (the common-lisp/comment document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (?a
       (append `((the lisp-form/comment (printer-output (the common-lisp/comment document) ,projection ,recursion))) reference)))))

(def function backward-mapper/common-lisp/toplevel->lisp-form/toplevel (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the sequence (elements-of (the lisp-form/toplevel document)))
        (the ?type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((element (elt (body-of printer-input) ?child-index))
              (element-iomap (elt (child-iomaps-of printer-iomap) ?child-index)))
         (values `((the sequence (body-of (the common-lisp/toplevel document)))
                   (the ,(document-type element) (elt (the sequence document) ,?child-index)))
                 ?rest
                 element-iomap)))
      (?a
       (append `((the lisp-form/toplevel (printer-output (the common-lisp/toplevel document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def function recurse/slot (recursion input slot input-reference)
  (bind ((value (slot-value input slot))
         (reader (find-slot-reader (class-of input) (find-slot (class-of input) slot))))
    (typecase value
      (computed-ll
       (map-ll* (slot-value input slot)
                (lambda (element index)
                  (recurse-printer recursion (value-of element)
                                   `((elt (the sequence document) ,index)
                                     (the sequence (,reader (the ,(document-type input) document)))
                                     ,@(typed-reference (document-type input) input-reference))))))
      (sequence
       (iter (for index :from 0)
             (for element :in-sequence (slot-value input slot))
             (collect (recurse-printer recursion element `((elt (the sequence document) ,index)
                                                           (the sequence (,reader (the ,(document-type input) document)))
                                                           ,@(typed-reference (document-type input) input-reference))))))
      (t
       (recurse-printer recursion value `((,reader (the ,(document-type input) document))
                                          ,@(typed-reference (document-type input) input-reference)))))))

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
        :for argument :in (coerce arguments 'list)
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
                               (list '&aux))))
                     (common-lisp/insertion)) :into result
        :collect (recurse-printer recursion argument `((elt (the sequence document) ,index)
                                                       ,(typed-reference (document-type input) input-reference))) :into result
        :finally (return (nconc result (ensure-&allow-other-keys)))))))

(def printer common-lisp/insertion->lisp-form/insertion (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/insertion->lisp-form/insertion)))
         (output (as (lisp-form/insertion (:default-value (default-value-of input) :compound (compound-p input) :selection output-selection)
                                          (value-of input) (factory-of input)))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/constant->lisp-form/string (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/constant->lisp-form/string)))
         (output (as (value-of input)
                     #+nil
                     (bind ((value (value-of input)))
                       (etypecase value
                         ((or number document/number) (make-lisp-form/number value :selection output-selection))
                         ((or string document/string) (make-lisp-form/string value :selection output-selection))
                         (symbol (if (keywordp value)
                                     (make-lisp-form/symbol (symbol-name value) (package-name (symbol-package value))
                                                            :font-color *color/solarized/magenta*
                                                            :selection output-selection)
                                     (make-lisp-form/quote (make-lisp-form/symbol (symbol-name value) (package-name (symbol-package value))
                                                                                  :font-color *color/solarized/magenta*
                                                                                  :selection (as (nthcdr 1 (va output-selection))))
                                                           :selection output-selection))))))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/variable-reference->lisp-form/symbol (projection recursion input input-reference)
  (bind ((output-selection (as (pattern-case (selection-of input)
                                 (((the ?variable-type (variable-of (the common-lisp/variable-reference document)))
                                   (the lisp-form/symbol (name-of (the ?variable-type document)))
                                   (the string (name-of (the lisp-form/symbol document)))
                                   (the string (subseq (the string document) ?start-index ?end-index)))
                                  `((the string (name-of (the lisp-form/symbol document)))
                                    (the string (subseq (the string document) ,?start-index ,?end-index))))
                                 (((the lisp-form/symbol (printer-output (the common-lisp/variable-reference document) ?projection ?recursion)) . ?rest)
                                  (when (eq projection ?projection)
                                    ?rest)))))
         (output (as (bind ((variable-name (name-of (variable-of input))))
                       (make-lisp-form/symbol (name-of variable-name) (package-of variable-name) :default-value "enter variable name" :font-color *color/solarized/orange* :selection output-selection)))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/function-reference->lisp-form/symbol (projection recursion input input-reference)
  (bind ((output-selection (as (pattern-case (selection-of input)
                                 (((the ?function-type (function-of (the common-lisp/function-reference document)))
                                   (the lisp-form/symbol (name-of (the ?function-type document)))
                                   (the string (name-of (the lisp-form/symbol document)))
                                   (the string (subseq (the string document) ?start-index ?end-index)))
                                  `((the string (name-of (the lisp-form/symbol document)))
                                    (the string (subseq (the string document) ,?start-index ,?end-index))))
                                 (((the lisp-form/symbol (printer-output (the common-lisp/function-reference document) ?projection ?recursion)) . ?rest)
                                  (when (eq projection ?projection)
                                    ?rest)))))
         (output (as (bind ((function-name (name-of (function-of input))))
                       (make-lisp-form/symbol (name-of function-name) (package-of function-name) :default-value "enter function name" :font-color *color/solarized/blue* :selection output-selection)))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/if->lisp-form/list (projection recursion input input-reference)
  (bind ((condition-iomap (as (recurse/slot recursion input 'condition input-reference)))
         (then-iomap (as (recurse/slot recursion input 'then input-reference)))
         (else-iomap (as (recurse/slot recursion input 'else input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va condition-iomap) (va then-iomap) (va else-iomap))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/if->lisp-form/list)))
         (output (as (make-lisp-form/list (append (list (make-lisp-form/symbol* 'if :font-color *color/solarized/blue* :selection (as (nthcdr 2 (va output-selection)))))
                                                  (awhen (va condition-iomap)
                                                    (list (output-of it)))
                                                  (awhen (va then-iomap)
                                                    (list (etypecase (output-of it)
                                                            (lisp-form/base (lisp-form/clone (output-of it) :indentation 4))
                                                            (tree/base (tree/clone (output-of it) :indentation 4)))))
                                                  (awhen (va else-iomap)
                                                    (list (etypecase (output-of it)
                                                            (lisp-form/base (lisp-form/clone (output-of it) :indentation 4))
                                                            (tree/base (tree/clone (output-of it) :indentation 4))))))
                                          :collapsed (as (collapsed-p input))
                                          :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va condition-iomap) (va then-iomap) (va else-iomap))))))

(def printer common-lisp/the->lisp-form/list (projection recursion input input-reference)
  (bind ((value-iomap (as (recurse/slot recursion input 'value input-reference)))
         (output (as (make-lisp-form/list (list (make-lisp-form/symbol* 'the :font-color *color/solarized/blue*)
                                                (make-lisp-form/symbol* (declared-type-of input) :font-color *color/solarized/violet*)
                                                (output-of (va value-iomap)))))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/progn->lisp-form/list (projection recursion input input-reference)
  (bind ((body-iomaps (as (recurse/slot recursion input 'body input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil body-iomaps)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/progn->lisp-form/list)))
         (output (as (make-lisp-form/list (list* (make-lisp-form/symbol* 'progn :font-color *color/solarized/blue* :selection (as (nthcdr 2 (va output-selection))))
                                                 (iter (for body-iomap :in-sequence (va body-iomaps))
                                                       (for body-output = (output-of body-iomap))
                                                       (collect (etypecase body-output
                                                                  (lisp-form/base (lisp-form/clone body-output :indentation 2))
                                                                  (tree/base (tree/clone body-output :indentation 2))))))
                                          :collapsed (as (collapsed-p input))
                                          :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output body-iomaps)))

(def printer common-lisp/lexical-variable-binding->lisp-form/list (projection recursion input input-reference)
  (bind ((value-iomap (as (recurse/slot recursion input 'value input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va value-iomap))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/lexical-variable-binding->lisp-form/list)))
         (output (as (bind ((variable-name (name-of input)))
                       (make-lisp-form/list (list (make-lisp-form/symbol (name-of variable-name) (package-of variable-name) :selection (as (nthcdr 2 (va output-selection))) :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/red*)
                                                  (output-of (va value-iomap)))
                                            :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va value-iomap))))))

(def printer common-lisp/let->lisp-form/list (projection recursion input input-reference)
  (bind ((binding-iomaps (as (recurse/slot recursion input 'bindings input-reference)))
         (body-iomaps (as (recurse/slot recursion input 'body input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (append (va binding-iomaps) (va body-iomaps))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/let->lisp-form/list)))
         (output (as (make-lisp-form/list (list* (make-lisp-form/symbol* 'let :selection (as (nthcdr 2 (va output-selection))) :font-color *color/solarized/blue*)
                                                 (make-lisp-form/list (iter (for binding-iomap :in-sequence (va binding-iomaps))
                                                                            (for binding-output = (output-of binding-iomap))
                                                                            (collect (if (first-iteration-p)
                                                                                         binding-output
                                                                                         (etypecase binding-output
                                                                                           (lisp-form/base (lisp-form/clone binding-output :indentation 1))
                                                                                           (tree/base (tree/clone binding-output :indentation 1))))))
                                                                      :selection (as (nthcdr 2 (va output-selection))))
                                                 (iter (for body-iomap :in-sequence (va body-iomaps))
                                                       (for body-output = (output-of body-iomap))
                                                       (collect (when body-output
                                                                  (etypecase body-output
                                                                    (lisp-form/base (lisp-form/clone body-output :indentation 2))
                                                                    (tree/base (tree/clone body-output :indentation 2)))))))
                                          :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (append (va binding-iomaps) (va body-iomaps))))))

(def printer common-lisp/application->lisp-form/list (projection recursion input input-reference)
  (bind ((argument-iomaps (as (when (arguments-of input)
                                (recurse/slot recursion input 'arguments input-reference))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil argument-iomaps)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/application->lisp-form/list)))
         (output (as (bind ((operator (operator-of input))
                            ((:values operator-name operator-package) (etypecase operator
                                                                        (lisp-form/symbol
                                                                         (values (name-of operator) (package-of operator)))
                                                                        (common-lisp/function-reference
                                                                         (bind ((function-name (name-of (function-of operator))))
                                                                           (values (name-of function-name) (package-of function-name)))))))
                       (make-lisp-form/list (append-ll (list-ll (list-ll (make-lisp-form/symbol operator-name operator-package :default-value "enter function name" :font-color *color/solarized/violet* :selection (as (nthcdr 2 (va output-selection)))))
                                                                (map-ll (va argument-iomaps) 'output-of)))
                                            :selection output-selection
                                            :collapsed (as (collapsed-p input)))))))
    (make-iomap/compound projection recursion input input-reference output argument-iomaps)))

(def printer common-lisp/special-variable-definition->lisp-form/list (projection recursion input input-reference)
  (bind ((value-iomap (as (recurse/slot recursion input 'value input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va value-iomap))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/special-variable-definition->lisp-form/list)))
         (output (as (bind ((variable-name (name-of input)))
                       (make-lisp-form/list (list (make-lisp-form/symbol* 'defvar :font-color *color/solarized/blue* :selection (as (nthcdr 2 (va output-selection))))
                                                  (make-lisp-form/symbol (name-of variable-name) (package-of variable-name) :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/violet* :selection (as (nthcdr 2 (va output-selection))))
                                                  (bind ((value-output (output-of (va value-iomap))))
                                                    (etypecase value-output
                                                      (lisp-form/base (lisp-form/clone value-output :indentation 2))
                                                      (tree/base (tree/clone value-output :indentation 2)))))
                                            :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va value-iomap))))))

(def printer common-lisp/function-definition->lisp-form/list (projection recursion input input-reference)
  (bind ((binding-iomaps (as (recurse/ordinary-lambda-list recursion input input-reference)))
         (body-iomaps (as (recurse/slot recursion input 'body input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (append (va binding-iomaps) (coerce (va body-iomaps) 'list))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/function-definition->lisp-form/list)))
         (output (as (bind ((documentation (documentation-of input))
                            (function-name (name-of input)))
                       (make-lisp-form/list (append (list (make-lisp-form/symbol* 'defun :font-color *color/solarized/blue* :selection (as (nthcdr 2 (va output-selection))))
                                                          (make-lisp-form/symbol (name-of function-name) (package-of function-name) :default-value "enter function name" :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/violet* :selection (as (nthcdr 2 (va output-selection))))
                                                          (make-lisp-form/list (mapcar 'output-of (va binding-iomaps)) :selection (as (pattern-case (va output-selection)
                                                                                                                                        (((the sequence (elements-of (the lisp-form/list document)))
                                                                                                                                          (the lisp-form/list (elt (the sequence document) 2))
                                                                                                                                          . ?rest)
                                                                                                                                         (nthcdr 2 (va output-selection)))))))
                                                    (when documentation (list (make-lisp-form/string documentation :default-value "enter function documentation" :indentation 2 :selection (as (nthcdr 2 (va output-selection))))))
                                                    (iter (for body-iomap :in-sequence (va body-iomaps))
                                                          (for body-output = (output-of body-iomap))
                                                          (collect (etypecase body-output
                                                                     (lisp-form/base (lisp-form/clone body-output :indentation 2))
                                                                     (tree/base (tree/clone body-output :indentation 2))))))
                                            :collapsed (as (collapsed-p input))
                                            :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output (as (append (va binding-iomaps) (coerce (va body-iomaps) 'list))))))

(def printer common-lisp/lambda-function->lisp-form/list (projection recursion input input-reference)
  (bind ((binding-iomaps (as (recurse/ordinary-lambda-list recursion input input-reference)))
         (body-iomaps (as (recurse/slot recursion input 'body input-reference)))
         (output (as (make-lisp-form/list (list* (make-lisp-form/symbol* 'lambda :font-color *color/solarized/blue*)
                                                 (make-lisp-form/list (mapcar 'output-of (va binding-iomaps)))
                                                 (iter (for body-iomap :in-sequence (va body-iomaps))
                                                       (for body-output = (output-of body-iomap))
                                                       (collect (etypecase body-output
                                                                  (lisp-form/base (lisp-form/clone body-output :indentation 2))
                                                                  (tree/base (tree/clone body-output :indentation 2))))))))))
    (make-iomap/compound projection recursion input input-reference output (as (append (va binding-iomaps) (va body-iomaps))))))

(def printer common-lisp/function-argument->lisp-form/symbol (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/function-argument->lisp-form/symbol)))
         (output (as (bind ((name (name-of input)))
                       (make-lisp-form/symbol (name-of name) (package-of name) :default-value "enter argument name" :selection output-selection :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/red*)))))
    (make-iomap projection recursion input input-reference output)))

(def printer common-lisp/comment->lisp-form/comment (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va content-iomap))))
                                                (selection-of input)
                                                'forward-mapper/common-lisp/comment->lisp-form/comment)))
         (output (as (make-lisp-form/comment (output-of (va content-iomap))
                                             :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

(def printer common-lisp/toplevel->lisp-form/toplevel (projection recursion input input-reference)
  (bind ((body-iomaps (as (recurse/slot recursion input 'body input-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil body-iomaps)
                                                (selection-of input)
                                                'forward-mapper/common-lisp/toplevel->lisp-form/toplevel)))
         (output (as (make-lisp-form/toplevel (iter (for body-iomap :in-sequence (va body-iomaps))
                                                     (for body-output = (output-of body-iomap))
                                                     (collect (if (first-iteration-p)
                                                                  body-output
                                                                  (etypecase body-output
                                                                    (lisp-form/base (lisp-form/clone body-output :indentation 0))
                                                                    (tree/base (tree/clone body-output :indentation 0))))))
                                               :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output body-iomaps)))

;;;;;;
;;; Reader

(def function common-lisp/read-expression-command (printer-iomap gesture)
  (bind ((printer-input (input-of printer-iomap)))
    (gesture-case gesture
      ((make-key-press-gesture :scancode-e :control)
       :domain "Common Lisp" :description "Evaluates the common lisp form at the selection"
       :operation (make-operation/functional (lambda ()
                                               (eval (printer-output printer-input (sequential
                                                                                     (recursive (make-projection/common-lisp->lisp-form))
                                                                                     (recursive (make-projection/lisp-form->form)))))))))))

(def reader common-lisp/insertion->lisp-form/insertion (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((gesture (gesture-of input))
         (printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case gesture
                      ((make-key-press-gesture :scancode-tab)
                       :domain "Common Lisp" :description "Inserts the suggested completion at the selection"
                       :operation (bind (((:values nil completion) (funcall (factory-of printer-input) (factory-of printer-input) printer-input input (value-of printer-input)))
                                         (end (length (value-of printer-input))))
                                    (when completion
                                      (make-operation/string/replace-range printer-input `((the string (value-of (the common-lisp/insertion document)))
                                                                                           (the string (subseq (the string document) ,end ,end))) completion))))
                      ((make-key-press-gesture :scancode-return)
                       :domain "Common Lisp" :description "Inserts a new object of the provided type"
                       :operation (bind (((:values immediate-new-instance completion) (funcall (factory-of printer-input) (factory-of printer-input) printer-input input (value-of printer-input)))
                                         (new-instance (or immediate-new-instance (funcall (factory-of printer-input) (factory-of printer-input) printer-input input (string+ (value-of printer-input) completion)))))
                                    (when new-instance
                                      (make-operation/replace-target printer-input nil new-instance))))
                      ((make-type-in-gesture #\Space)
                       :domain "Common Lisp" :description "Inserts a new object of the provided type"
                       :operation (bind (((:values immediate-new-instance completion) (funcall (factory-of printer-input) (factory-of printer-input) printer-input input (value-of printer-input)))
                                         (new-instance (or immediate-new-instance (funcall (factory-of printer-input) (factory-of printer-input) printer-input input (string+ (value-of printer-input) completion)))))
                                    (when new-instance
                                      (make-operation/replace-target printer-input nil new-instance))))
                      ((make-type-in-gesture #\()
                       :domain "Common Lisp" :description "TODO"
                       :operation (make-operation/functional (lambda () (notf (compound-p printer-input)))))
                      ((make-type-in-gesture #\")
                       :domain "Common Lisp" :description "TODO"
                       :operation (make-operation/replace-target printer-input nil (make-common-lisp/constant
                                                                                    (lisp-form/string (:selection `((the string (value-of (the lisp-form/string document)))
                                                                                                                    (the string (subseq (the string document) 0 0)))) "")
                                                                                    :selection `((the lisp-form/number (value-of (the common-lisp/constant document)))
                                                                                                 (the string (value-of (the lisp-form/string document)))
                                                                                                 (the string (subseq (the string document) 0 0))))))
                      ((make-type-in-gesture #\')
                       :domain "Common Lisp" :description "TODO"
                       :operation (make-operation/replace-target printer-input nil (make-common-lisp/constant
                                                                                    (lisp-form/quote (:selection `((the lisp-form/symbol (value-of (the lisp-form/quote document)))
                                                                                                                   (the string (name-of (the lisp-form/symbol document)))
                                                                                                                   (the string (subseq (the string document) 0 0))))
                                                                                      (lisp-form/symbol (:selection `((the string (name-of (the lisp-form/symbol document)))
                                                                                                                      (the string (subseq (the string document) 0 0))))
                                                                                                        "" "COMMON-LISP-USER"))
                                                                                    :selection `((the lisp-form/quote (value-of (the common-lisp/constant document)))
                                                                                                 (the lisp-form/symbol (value-of (the lisp-form/quote document)))
                                                                                                 (the string (name-of (the lisp-form/symbol document)))
                                                                                                 (the string (subseq (the string document) 0 0))))))
                      #+nil
                      ((make-key-press-gesture :scancode-escape)
                       :domain "Common Lisp" :description "Aborts the object insertion"
                       :operation (make-operation/replace-target printer-input nil (document/nothing))))
                    (when (and (typep gesture 'gesture/keyboard/type-in)
                               (digit-char-p (character-of gesture)))
                      (make-command gesture
                                    (make-operation/replace-target printer-input nil
                                                                   (make-common-lisp/constant
                                                                    (lisp-form/number (:selection `((the number (value-of (the lisp-form/number document)))
                                                                                                    (the string (write-to-string (the number document)))
                                                                                                    (the string (subseq (the string document) 1 1))))
                                                                      (parse-number:parse-number (string (character-of gesture))))
                                                                    :selection `((the lisp-form/number (value-of (the common-lisp/constant document)))
                                                                                 (the number (value-of (the lisp-form/number document)))
                                                                                 (the string (write-to-string (the number document)))
                                                                                 (the string (subseq (the string document) 1 1)))))
                                    :domain "Common Lisp"
                                    :description "Inserts a new constant number at the selection"))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/insertion->lisp-form/insertion nil)
                    (make-nothing-command gesture))))

(def reader common-lisp/constant->lisp-form/string (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/constant->lisp-form/string nil)
                    (make-nothing-command (gesture-of input)))))

(def reader common-lisp/variable-reference->lisp-form/symbol (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/variable-reference->lisp-form/symbol nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/function-reference->lisp-form/symbol (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/function-reference->lisp-form/symbol nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/if->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/if->lisp-form/list 'backward-mapper/common-lisp/if->lisp-form/list)
                    ;; TODO:
                    #+nil
                    (gesture-case (gesture-of input)
                      ((make-type-in-gesture #\Space)
                       :domain "Common Lisp" :description "Moves the selection to the next branch"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the ?type (condition-of (the common-lisp/if document))) . ?rest)
                                     (make-operation/replace-selection printer-input `((the ,(document-type (then-of printer-input)) (then-of (the common-lisp/if document)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/if->lisp-form/list nil)
                    (make-nothing-command (gesture-of input)))))

(def reader common-lisp/the->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/the->lisp-form/list 'backward-mapper/common-lisp/the->lisp-form/list)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/the->lisp-form/list nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/progn->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/progn->lisp-form/list 'backward-mapper/common-lisp/progn->lisp-form/list)
                    (gesture-case (gesture-of input)
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Common lisp" :description "Starts an insertion into the elements of the form"
                       :operation (bind ((body-length (length (body-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (body-of (the common-lisp/progn document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,body-length ,body-length)))
                                                                                                          (list (document/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (body-of (the common-lisp/progn document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,body-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/progn->lisp-form/list nil)
                    (make-nothing-command (gesture-of input)))))

(def reader common-lisp/lexical-variable-binding->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/lexical-variable-binding->lisp-form/list 'backward-mapper/common-lisp/lexical-variable-binding->lisp-form/list)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/lexical-variable-binding->lisp-form/list nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/let->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/let->lisp-form/list 'backward-mapper/common-lisp/let->lisp-form/list)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/let->lisp-form/list nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/application->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/application->lisp-form/list 'backward-mapper/common-lisp/application->lisp-form/list)
                    (gesture-case (gesture-of input)
                      ((make-type-in-gesture #\()
                       :domain "Common Lisp" :description "Starts a Common Lisp insertion into the arguments of the function application"
                       :operation (bind ((arguments-length (length (arguments-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (arguments-of (the common-lisp/application document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,arguments-length ,arguments-length)))
                                                                                                          (list (make-common-lisp/insertion "" 'common-lisp/complete-document :compound #t)))
                                                                   (make-operation/replace-selection printer-input `((the sequence (arguments-of (the common-lisp/application document)))
                                                                                                                     (the common-lisp/insertion (elt (the sequence document) ,arguments-length))
                                                                                                                     (the string (value-of (the common-lisp/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((make-type-in-gesture #\Space)
                       :domain "Common Lisp" :description "Starts a Common Lisp insertion into the arguments of the function application"
                       :operation (bind ((arguments-length (length (arguments-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (arguments-of (the common-lisp/application document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,arguments-length ,arguments-length)))
                                                                                                          (list (make-common-lisp/insertion "" (factory-of printer-input))))
                                                                   (make-operation/replace-selection printer-input `((the sequence (arguments-of (the common-lisp/application document)))
                                                                                                                     (the common-lisp/insertion (elt (the sequence document) ,arguments-length))
                                                                                                                     (the string (value-of (the common-lisp/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Common lisp" :description "Starts an insertion into the arguments of the application"
                       :operation (bind ((argument-length (length (arguments-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (arguments-of (the common-lisp/function-definition document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,argument-length ,argument-length)))
                                                                                                          (list (document/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (arguments-of (the common-lisp/function-definition document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,argument-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/application->lisp-form/list nil)
                    (make-nothing-command (gesture-of input)))))

(def reader common-lisp/special-variable-definition->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/special-variable-definition->lisp-form/list 'backward-mapper/common-lisp/special-variable-definition->lisp-form/list)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/special-variable-definition->lisp-form/list nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/function-definition->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/function-definition->lisp-form/list 'backward-mapper/common-lisp/function-definition->lisp-form/list)
                    (gesture-case (gesture-of input)
                      ((make-type-in-gesture #\()
                       :domain "Common Lisp" :description "Starts a Common Lisp insertion into the body of the function definition"
                       :operation (bind ((body-length (length (body-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (body-of (the common-lisp/function-definition document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,body-length ,body-length)))
                                                                                                          (list (make-common-lisp/insertion "" (make-instance 'common-lisp/function-definition/completion :function-definition printer-input) :compound #t)))
                                                                   (make-operation/replace-selection printer-input `((the sequence (body-of (the common-lisp/function-definition document)))
                                                                                                                     (the common-lisp/insertion (elt (the sequence document) ,body-length))
                                                                                                                     (the string (value-of (the common-lisp/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((make-type-in-gesture #\Space)
                       :domain "Common Lisp" :description "Moves the selection to the bindings of the function definition"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the lisp-form/symbol (name-of (the common-lisp/function-definition document))) . ?rest)
                                     (make-operation/replace-selection printer-input `((the sequence (bindings-of (the common-lisp/function-definition document)))
                                                                                       (the common-lisp/insertion (elt (the sequence document) 0))
                                                                                       (the string (value-of (the common-lisp/insertion document)))
                                                                                       (the string (subseq (the string document) 0 0)))))))
                      ((make-type-in-gesture #\")
                       :domain "Common Lisp" :description "Moves the selection to the documentation of the function definition"
                       :operation (make-operation/replace-selection printer-input `((the string (documentation-of (the common-lisp/function-definition document)))
                                                                                    (the string (subseq (the string document) 0 0)))))
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Common lisp" :description "Starts an insertion into the body of the function definition"
                       :operation (bind ((body-length (length (body-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (body-of (the common-lisp/function-definition document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,body-length ,body-length)))
                                                                                                          (list (document/insertion ())))
                                                                   (make-operation/replace-selection printer-input `((the sequence (body-of (the common-lisp/function-definition document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,body-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0)))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/function-definition->lisp-form/list nil)
                    (make-nothing-command (gesture-of input)))))

(def reader common-lisp/lambda-function->lisp-form/list (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/lambda-function->lisp-form/list 'backward-mapper/common-lisp/lambda-function->lisp-form/list)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/lambda-function->lisp-form/list nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/function-argument->lisp-form/symbol (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/function-argument->lisp-form/symbol nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/comment->lisp-form/comment (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/comment->lisp-form/comment 'backward-mapper/common-lisp/comment->lisp-form/comment)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/comment->lisp-form/comment nil)
                  (make-nothing-command (gesture-of input))))

(def reader common-lisp/toplevel->lisp-form/toplevel (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (common-lisp/read-expression-command printer-iomap (gesture-of input))
                  (command/read-selection recursion input printer-iomap 'forward-mapper/common-lisp/toplevel->lisp-form/toplevel 'backward-mapper/common-lisp/toplevel->lisp-form/toplevel)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/common-lisp/toplevel->lisp-form/toplevel nil)
                  (make-nothing-command (gesture-of input))))
