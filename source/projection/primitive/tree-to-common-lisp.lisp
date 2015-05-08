;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection tree/leaf->common-lisp/progn ()
  ())

(def projection tree/node->common-lisp/progn ()
  ())

;;;;;;
;;; Construction

(def function make-projection/tree/leaf->common-lisp/progn ()
  (make-projection 'tree/leaf->common-lisp/progn))

(def function make-projection/tree/node->common-lisp/progn ()
  (make-projection 'tree/node->common-lisp/progn))

;;;;;;
;;; Construction

(def macro tree/leaf->common-lisp/progn ()
  `(make-projection/tree/leaf->common-lisp/progn))

(def macro tree/node->common-lisp/progn ()
  `(make-projection/tree/node->common-lisp/progn))

;;;;;;
;;; Forward mapper

(def function forward-mapper/tree/leaf->common-lisp/progn (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (pattern-case reference
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (or (opening-delimiter-of printer-input)
               (closing-delimiter-of printer-input))
           `((the sequence (body-of (the common-lisp/progn document)))
             (the common-lisp/application (elt (the sequence document) 1))
             (the sequence (arguments-of (the common-lisp/application document)))
             (the common-lisp/constant (elt (the sequence document) 0))
             (the string (value-of (the common-lisp/constant document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))
           `((the sequence (arguments-of (the common-lisp/application document)))
             (the common-lisp/constant (elt (the sequence document) 0))
             (the string (value-of (the common-lisp/constant document)))
             (the string (subseq (the string document) ,?start-index ,?end-index)))))
      (((the common-lisp/progn (printer-output (the tree/leaf document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

(def function forward-mapper/tree/node->common-lisp/progn (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((index 0)
              #+nil
              (element (elt (body-of (output-of printer-iomap)) index)))
         (values `((the sequence (body-of (the common-lisp/progn document)))
                   (the common-lisp/progn (elt (the sequence document) ,index)))
                 ?rest
                 (elt (child-iomaps-of printer-iomap) index))))
      (((the common-lisp/progn (printer-output (the tree/node document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/tree/leaf->common-lisp/progn (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (arguments-of (the common-lisp/application document)))
        (the common-lisp/constant (elt (the sequence document) 0))
        (the string (value-of (the common-lisp/constant document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (((the sequence (body-of (the common-lisp/progn document)))
        (the common-lisp/application (elt (the sequence document) 1))
        (the sequence (arguments-of (the common-lisp/application document)))
        (the common-lisp/constant (elt (the sequence document) 0))
        (the string (value-of (the common-lisp/constant document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
      (?a
       (append `((the common-lisp/progn (printer-output (the tree/leaf document) ,projection ,recursion))) reference)))))

(def function backward-mapper/tree/node->common-lisp/progn (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the sequence (body-of (the common-lisp/progn document)))
        (the ?type (elt (the sequence document) ?index))
        . ?rest)
       (bind ((index 0)
              (child (elt (children-of (input-of printer-iomap)) index)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(form-type child) (elt (the sequence document) ,index)))
                 ?rest
                 (elt (child-iomaps-of printer-iomap) index))))
      (?a
       (append `((the common-lisp/progn (printer-output (the tree/node document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer tree/leaf->common-lisp/progn (projection recursion input input-reference)
  (bind ((content-reference `(((content-of (the ,(form-type input) document))) ,@(typed-reference (form-type input) input-reference)))
         (content-iomap (as (recurse-printer recursion (content-of input) content-reference)))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil (as (list (va content-iomap))))
                                                (selection-of input)
                                                'forward-mapper/tree/leaf->common-lisp/progn)))
         (output (as (if (or (opening-delimiter-of input)
                             (closing-delimiter-of input))
                         (make-common-lisp/progn (optional-list (awhen (opening-delimiter-of input)
                                                                  (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                (list (make-common-lisp/constant (text/as-string it) :selection (as (nthcdr 4 (va output-selection)))))
                                                                                                :selection (as (nthcdr 2 (va output-selection)))))
                                                                (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                              (list (make-common-lisp/constant (text/as-string (output-of (va content-iomap))) :selection (as (nthcdr 4 (va output-selection)))))
                                                                                              :selection (as (nthcdr 2 (va output-selection))))
                                                                (awhen (closing-delimiter-of input)
                                                                  (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                (list (make-common-lisp/constant (text/as-string it) :selection (as (nthcdr 4 (va output-selection)))))
                                                                                                :selection (as (nthcdr 2 (va output-selection))))))
                                                 :selection output-selection)
                         (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                       (list (make-common-lisp/constant (text/as-string (output-of (va content-iomap))) :selection (as (nthcdr 2 (va output-selection)))))
                                                       :selection output-selection)))))
    (make-iomap/compound projection recursion input input-reference output (as (list (va content-iomap))))))

(def printer tree/node->common-lisp/progn (projection recursion input input-reference)
  (bind ((child-iomaps (as (map-ll* (ll (children-of input))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the tree/node document)))
                                                                ,@(typed-reference (form-type input) input-reference))))
                                        (recurse-printer recursion (value-of child) child-reference))))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil child-iomaps)
                                                (selection-of input)
                                                'forward-mapper/tree/node->common-lisp/progn)))
         (output (as (make-common-lisp/progn (append (awhen (opening-delimiter-of input)
                                                       (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                           (list (make-common-lisp/constant (text/as-string it))))))
                                                     (iter (for child :in-sequence (coerce (va child-iomaps) 'list))
                                                           (unless (first-iteration-p)
                                                             (awhen (separator-of input)
                                                               (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                      (list (make-common-lisp/constant (text/as-string it)))))))
                                                           (awhen (and (typep (input-of child) 'tree/base)
                                                                       (indentation-of (input-of child)))
                                                             (collect (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                                    (list (make-common-lisp/constant " ")))))
                                                           (collect (output-of child)))
                                                     (awhen (closing-delimiter-of input)
                                                       (list (make-common-lisp/application (make-lisp-form/symbol* 'write-string)
                                                                                           (list (make-common-lisp/constant (substitute #\Space #\NewLine (text/as-string it))))))))
                                             :selection output-selection))))
    (make-iomap/compound projection recursion input input-reference output child-iomaps)))

;;;;;;
;;; Reader

(def reader tree/leaf->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/tree/leaf->common-lisp/progn 'backward-mapper/tree/leaf->common-lisp/progn)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/tree/leaf->common-lisp/progn nil)
                  (make-command/nothing (gesture-of input))))

(def reader tree/node->common-lisp/progn (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/tree/node->common-lisp/progn 'backward-mapper/tree/node->common-lisp/progn)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/tree/node->common-lisp/progn nil)
                  (make-command/nothing (gesture-of input))))
