;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection object-searching ()
  ((searcher :type function)))

;;;;;;
;;; Construction

(def function make-projection/object-searching (searcher)
  (make-projection 'object-searching :searcher searcher))

;;;;;;
;;; Construction

(def macro object-searching (searcher)
  `(make-projection/object-searching ,searcher))

;;;;;;
;;; IO map

#+nil
(def iomap iomap/object-searching ()
  ((result :type sequence)))

;;;;;;
;;; Forward mapper

(def forward-mapper object-searching ()
  (pattern-case -reference-
    #+nil
    (((the string (search-of (the searching/search document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the sequence (children-of (the tree/node document)))
       (the tree/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the tree/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the ?type (document-of (the searching/search document)))
      . ?rest)
     #+nil
     (iter (for index :from 0)
           (for reference :in result)
           (when (and (<= (length reference) (length ?rest))
                      (equal reference (subseq ?rest 0 (length reference))))
             (return (values (append `((the sequence (children-of (the tree/node document)))
                                       (the ?type (elt (the sequence document) ,index))
                                       (the sequence (children-of (the tree/node document)))
                                       (the tree/node (elt (the sequence document) 0)))
                                     (subseq ?rest (length reference)))
                             index)))))
    (((the searching/result (printer-output (the searching/search document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper object-searching ()
  (pattern-case -reference-
    (((the sequence (elements-of (the searching/result document)))
      (the searching/result-element (elt (the sequence document) ?index))
      (the ?type (document-of (the searching/result-element document)))
      . ?rest)
     `((the ,(document-type (document-of -printer-input-)) (document-of (the searching/search document)))
       ,@?rest))
    #+nil
    (((the sequence (children-of (the tree/node document)))
      (the tree/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the string (search-of (the searching/search document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    #+nil
    (((the sequence (children-of (the tree/node document)))
      (the tree/node (elt (the sequence document) ?index))
      (the sequence (children-of (the tree/node document)))
      (the ?type (elt (the sequence document) 0))
      . ?rest)
     (values `((the ,(document-type (content-of printer-input)) (content-of (the searching/search document))))
             ?rest
             ;; TODO:
             nil))
    (?a
     (append `((the searching/result (printer-output (the searching/search document) ,-projection- ,-recursion-))) -reference-))))

;;;;;;
;;; Printer

(def printer object-searching ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (selection-of -input-)
                                                'forward-mapper/object-searching)))
         (output (as (bind ((document (document-of -input-))
                            (search (search-of -input-))
                            (references (unless (string= search "")
                                          (funcall (searcher-of -projection-) search document))))
                       (make-searching/result (mapcar (lambda (reference)
                                                        (searching/result-element (reference)
                                                          document))
                                                      references)
                                              :selection output-selection)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader object-searching ()
  (merge-commands #+nil
                  (awhen (labels ((recurse (operation)
                                    (typecase operation
                                      (operation/quit operation)
                                      (operation/functional operation)
                                      (operation/replace-selection
                                       (awhen (pattern-case (selection-of operation)
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/leaf (elt (the sequence document) 0))
                                                  (the text/text (content-of (the tree/leaf document)))
                                                  (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                 `((the string (search-of (the searching/search document)))
                                                   (the string (subseq (the string document) ,?start-index ,?end-index))))
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/node (elt (the sequence document) ?index))
                                                  (the sequence (children-of (the tree/node document)))
                                                  (the ?type (elt (the sequence document) 0))
                                                  . ?rest)
                                                 (bind ((index ?index)
                                                        (content (document-of printer-input))
                                                        (reference (elt (result-of printer-iomap) index)))
                                                   (append `((the ,(document-type content) (document-of (the searching/search document)))) reference ?rest)))
                                                (?a
                                                 (append `((the tree/node (printer-output (the searching/search document) ,projection ,recursion))) (selection-of operation))))
                                         (make-operation/replace-selection printer-input it)))
                                      (operation/sequence/replace-range
                                       (awhen (pattern-case (selection-of operation)
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/node (elt (the sequence document) ?index))
                                                  (the sequence (children-of (the tree/node document)))
                                                  (the ?type (elt (the sequence document) 0))
                                                  . ?rest)
                                                 (bind ((index ?index)
                                                        (content (document-of printer-input))
                                                        (reference (elt (result-of printer-iomap) index)))
                                                   (append `((the ,(document-type content) (document-of (the searching/search document)))) reference ?rest))))
                                         (make-operation/sequence/replace-range printer-input it (replacement-of operation))))
                                      (operation/text/replace-range
                                       (awhen (pattern-case (selection-of operation)
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/leaf (elt (the sequence document) 0))
                                                  (the text/text (content-of (the tree/leaf document)))
                                                  (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                                 (if (zerop (length (search-of  printer-input)))
                                                     `((the string (search-of (the searching/search document)))
                                                       (the string (subseq (the string document) 0 0)))
                                                     `((the string (search-of (the searching/search document)))
                                                       (the string (subseq (the string document) ,?start-index ,?end-index)))))
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/node (elt (the sequence document) ?index))
                                                  (the sequence (children-of (the tree/node document)))
                                                  (the ?type (elt (the sequence document) 0))
                                                  . ?rest)
                                                 (bind ((index ?index)
                                                        (content (document-of printer-input))
                                                        (reference (elt (result-of printer-iomap) index)))
                                                   (append `((the ,(document-type content) (document-of (the searching/search document)))) reference ?rest))))
                                         (make-operation/sequence/replace-range printer-input it (replacement-of operation))))
                                      (operation/number/replace-range
                                       (awhen (pattern-case (selection-of operation)
                                                (((the sequence (children-of (the tree/node document)))
                                                  (the tree/node (elt (the sequence document) ?index))
                                                  (the sequence (children-of (the tree/node document)))
                                                  (the ?type (elt (the sequence document) 0))
                                                  . ?rest)
                                                 (bind ((index ?index)
                                                        (content (document-of printer-input))
                                                        (reference (elt (result-of printer-iomap) index)))
                                                   (append `((the ,(document-type content) (document-of (the searching/search document)))) reference ?rest))))
                                         (make-operation/number/replace-range printer-input it (replacement-of operation))))
                                      (operation/compound
                                       (bind ((operations (mapcar #'recurse (elements-of operation))))
                                         (unless (some 'null operations)
                                           (make-operation/compound operations)))))))
                           (recurse (operation-of input)))
                    (make-command (gesture-of input) it
                                  :domain (domain-of input)
                                  :description (description-of input)))
                  (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/object-searching 'backward-mapper/object-searching)
                  (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/object-searching nil)
                  (make-nothing-command -gesture-)))
