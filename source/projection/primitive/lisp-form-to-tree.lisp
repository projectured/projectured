;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection lisp-form/insertion->tree/leaf ()
  ())

(def projection lisp-form/comment->tree/node ()
  ())

(def projection lisp-form/number->tree/leaf ()
  ())

(def projection lisp-form/symbol->tree/leaf ()
  ())

(def projection lisp-form/string->tree/leaf ()
  ())

(def projection lisp-form/quote->tree/node ()
  ())

(def projection lisp-form/list->tree/node ()
  ())

(def projection lisp-form/object->tree/leaf ()
  ())

(def projection lisp-form/top-level->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/lisp-form/insertion->tree/leaf ()
  (make-projection 'lisp-form/insertion->tree/leaf))

(def function make-projection/lisp-form/comment->tree/node ()
  (make-projection 'lisp-form/comment->tree/node))

(def function make-projection/lisp-form/number->tree/leaf ()
  (make-projection 'lisp-form/number->tree/leaf))

(def function make-projection/lisp-form/symbol->tree/leaf ()
  (make-projection 'lisp-form/symbol->tree/leaf))

(def function make-projection/lisp-form/string->tree/leaf ()
  (make-projection 'lisp-form/string->tree/leaf))

(def function make-projection/lisp-form/quote->tree/node ()
  (make-projection 'lisp-form/quote->tree/node))

(def function make-projection/lisp-form/list->tree/node ()
  (make-projection 'lisp-form/list->tree/node))

(def function make-projection/lisp-form/object->tree/leaf ()
  (make-projection 'lisp-form/object->tree/leaf))

(def function make-projection/lisp-form/top-level->tree/node ()
  (make-projection 'lisp-form/top-level->tree/node))

;;;;;;
;;; Construction

(def macro lisp-form/insertion->tree/leaf ()
  '(make-projection/lisp-form/insertion->tree/leaf))

(def macro lisp-form/comment->tree/node ()
  '(make-projection/lisp-form/comment->tree/node))

(def macro lisp-form/number->tree/leaf ()
  '(make-projection/lisp-form/number->tree/leaf))

(def macro lisp-form/symbol->tree/leaf ()
  '(make-projection/lisp-form/symbol->tree/leaf))

(def macro lisp-form/string->tree/leaf ()
  '(make-projection/lisp-form/string->tree/leaf))

(def macro lisp-form/quote->tree/node ()
  '(make-projection/lisp-form/quote->tree/node))

(def macro lisp-form/list->tree/node ()
  '(make-projection/lisp-form/list->tree/node))

(def macro lisp-form/object->tree/leaf ()
  '(make-projection/lisp-form/object->tree/leaf))

(def macro lisp-form/top-level->tree/node ()
  '(make-projection/lisp-form/top-level->tree/node))

;;;;;;
;;; Forward mapper

(def function forward-mapper/lisp-form/insertion->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (value-of (the lisp-form/insertion document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
         (the text/text (content-of (the tree/leaf document)))))
      (((the tree/leaf (printer-output (the lisp-form/insertion document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/comment->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the tree/node (printer-output (the lisp-form/comment document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/number->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the number (value-of (the lisp-form/number document)))
        (the string (write-to-string (the number document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
         (the text/text (content-of (the tree/leaf document)))))
      (((the tree/leaf (printer-output (the lisp-form/number document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/symbol->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (name-of (the lisp-form/symbol document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
         (the text/text (content-of (the tree/leaf document)))))
      (((the tree/leaf (printer-output (the lisp-form/symbol document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/string->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (value-of (the lisp-form/string document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
         (the text/text (content-of (the tree/leaf document)))))
      (((the tree/leaf (printer-output (the lisp-form/string document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/quote->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the tree/node (printer-output (the lisp-form/quote document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/list->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the sequence (elements-of (the lisp-form/list document)))
        (the ?element-type (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((element-iomap (elt (child-iomaps-of printer-iomap) ?element-index))
              (element-output (output-of element-iomap)))
         (values `((the ,(form-type element-output) (elt (the sequence document) ,?element-index))
                   (the sequence (children-of (the tree/node document))))
                 (reverse ?rest)
                 element-iomap)))
      (((the tree/node (printer-output (the lisp-form/list document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/object->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the tree/leaf (printer-output (the lisp-form/object document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

(def function forward-mapper/lisp-form/top-level->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the tree/node (printer-output (the lisp-form/top-level document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/lisp-form/insertion->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       `((the string (subseq (the string document) ,?start-index ,?end-index))
         (the string (value-of (the lisp-form/insertion document)))))
      (?a
       (append reference `((the tree/leaf (printer-output (the lisp-form/insertion document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/comment->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (?a
       (append reference `((the tree/node (printer-output (the lisp-form/comment document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/number->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (not (value-of printer-input))
           (append reference `((the tree/leaf (printer-output (the lisp-form/number document) ,projection ,recursion))))
           `((the string (subseq (the string document) ,?start-index ,?end-index))
             (the string (write-to-string (the number document)))
             (the number (value-of (the lisp-form/number document))))))
      (?a
       (append reference `((the tree/leaf (printer-output (the lisp-form/number document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/symbol->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= (name-of printer-input) "")
           (append reference `((the tree/leaf (printer-output (the lisp-form/symbol document) ,projection ,recursion))))
           `((the string (subseq (the string document) ,?start-index ,?end-index))
             (the string (name-of (the lisp-form/symbol document))))))
      (?a
       (append reference `((the tree/leaf (printer-output (the lisp-form/symbol document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/string->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (if (string= (value-of printer-input) "")
           (append reference `((the tree/leaf (printer-output (the lisp-form/string document) ,projection ,recursion))))
           `((the string (subseq (the string document) ,?start-index ,?end-index))
             (the string (value-of (the lisp-form/string document))))))
      (?a
       (append reference `((the tree/leaf (printer-output (the lisp-form/string document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/quote->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (?a
       (append reference `((the tree/node (printer-output (the lisp-form/quote document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/list->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((element (elt (elements-of printer-input) ?child-index))
              (element-iomap (elt (child-iomaps-of printer-iomap) ?child-index)))
         (values `((the ,(form-type element) (elt (the sequence document) ,?child-index))
                   (the sequence (elements-of (the lisp-form/list document))))
                 (reverse ?rest)
                 element-iomap)))
      (?a
       (append reference `((the tree/node (printer-output (the lisp-form/list document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/object->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (?a
       (append reference `((the tree/leaf (printer-output (the lisp-form/object document) ,projection ,recursion))))))))

(def function backward-mapper/lisp-form/top-level->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the sequence (children-of (the tree/node document)))
        (the ?type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((element (elt (elements-of printer-input) ?child-index))
              (element-iomap (elt (child-iomaps-of printer-iomap) ?child-index)))
         (values `((the ,(form-type element) (elt (the sequence document) ,?child-index))
                   (the sequence (elements-of (the lisp-form/top-level document))))
                 (reverse ?rest)
                 element-iomap)))
      (?a
       (append reference `((the tree/node (printer-output (the lisp-form/top-level document) ,projection ,recursion))))))))

;;;;;;
;;; Printer

(def printer lisp-form/insertion->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/insertion->tree/leaf)))
         (output (as (bind (((:values nil completion) (funcall (factory-of input) (value-of input)))
                            (commitable? (not (null (funcall (factory-of input) (string+ (value-of input) completion)))))
                            (value-color (if commitable? *color/solarized/green* *color/solarized/red*)))
                       (tree/leaf (:selection output-selection
                                   :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                   :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                         (text/text (:selection (as (butlast (va output-selection)))) (text/string (value-of input) :font *font/ubuntu/monospace/regular/18* :font-color value-color)
                           (text/string (if completion (if commitable? (string+ completion "?") completion) "") :font *font/ubuntu/monospace/regular/18* :font-color (color/lighten value-color 0.75))))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/comment->tree/node (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion (content-of input) input-reference)))
         (output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/comment->tree/node)))
         ;; TODO:
         (output (as (make-tree/node (list (tree/leaf ()
                                             (output-of (va content-iomap))))
                                     #+nil(text/make-string content :font *font/ubuntu/regular/18* :font-color *color/solarized/gray*)
                                     :opening-delimiter (text/text () (text/string ";; " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/number->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/number->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/make-default-text (aif (value-of input) (write-to-string it) "") "enter lisp number" :selection (as (butlast (va output-selection))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/symbol->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/symbol->tree/leaf)))

         (output (as (bind ((font-color (or (font-color-of input) *color/solarized/violet*))
                            (name (name-of input))
                            (name-string (string-downcase (if (string= "KEYWORD" (package-of input)) (string+ ":" name) name))))
                       (tree/leaf (:selection output-selection)
                         (text/make-default-text name-string (or (default-value-of input) "enter lisp symbol") :selection (as (butlast (va output-selection))) :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color font-color)
                         ;; TODO: parameter
                         #+nil
                         (text/text (:selection (butlast output-selection))
                           (text/string (string+ (string-downcase (package-of input)) "::" ) :font (or (font-of input) *font/ubuntu/monospace/regular/18*) :font-color (color/lighten font-color 0.5))
                           (text/string name-string :font (or (font-of input) *font/ubuntu/monospace/bold/18*) :font-color font-color)))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/string->tree/leaf (projection recursion input input-reference)
  (bind ((value (write-to-string (value-of input)))
         (output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/string->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection
                                 :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                 :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                       (text/make-default-text (subseq value 1 (1- (length value))) (or (default-value-of input) "enter lisp string") :selection (as (butlast (va output-selection))) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/quote->tree/node (projection recursion input input-reference)
  (bind ((value-iomap (as (recurse-printer recursion (value-of input)
                                           `((value-of (the sequence document))
                                             ,@(typed-reference (form-type input) input-reference)))))
         (output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/quote->tree/node)))
         (output (as (tree/node (:opening-delimiter (text/text () (text/string "'" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                       (output-of (va value-iomap))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/list->tree/node (projection recursion input input-reference)
  (bind ((deep-list (find-if (of-type 'lisp-form/list) (elements-of input)))
         (element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence (elements-of input))
                                   (for element-iomap = (recurse-printer recursion element
                                                                         `((elt (the sequence document) ,index)
                                                                           (the sequence (elements-of (the document list-form/list)))
                                                                           ,@(typed-reference (form-type input) input-reference))))
                                   (for element-output = (output-of element-iomap))
                                   (setf (indentation-of element-output)
                                         (typecase element
                                           (tree/base (indentation-of element))
                                           (lisp-form/base (indentation-of element))
                                           (t (when (and deep-list (not (first-iteration-p)))
                                                2))))
                                   (collect element-iomap))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/list->tree/node)))
         (output (as (bind ((separator-font-color (as (color/darken/selection *color/solarized/gray* (va output-selection))))
                            (separator-fill-color (as (color/lighten/selection (color/lighten *color/solarized/cyan* 0.75) (va output-selection) nil))))
                       (make-tree/node (mapcar 'output-of (va element-iomaps))
                                       :selection output-selection
                                       :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color separator-font-color :fill-color separator-fill-color))
                                       :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color separator-font-color :fill-color separator-fill-color))
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

(def printer lisp-form/object->tree/leaf (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap/object projection recursion input input-reference nil)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/object->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (butlast (va output-selection))))
                         (text/string (write-to-string input) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*))))))
    (make-iomap/object projection recursion input input-reference output)))

(def printer lisp-form/top-level->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (iter (for index :from 0)
                                   (for element :in (elements-of input))
                                   (for element-iomap = (recurse-printer recursion element
                                                                         `((elt (the sequence document) ,index)
                                                                           (the sequence (elements-of (the lisp-form/top-level document)))
                                                                           ,@(typed-reference (form-type input) input-reference))))
                                   (for element-output = (output-of element-iomap))
                                   (setf (indentation-of element-output)
                                         (typecase element
                                           (lisp-form/base (indentation-of element))
                                           (t 0)))
                                   (collect element-iomap))))
         (output-selection (as (print-selection (make-iomap/compound projection recursion input input-reference nil element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/lisp-form/top-level->tree/node)))
         (output (as (make-tree/node (mapcar 'output-of (va element-iomaps))
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                     :indentation 2))))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

;;;;;;
;;; Reader

(def reader lisp-form/insertion->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case (reverse selection)
                                  (((the string (value-of (the lisp-form/insertion document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/insertion->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/comment->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/comment->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader lisp-form/number->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case (reverse selection)
                                  (((the number (value-of (the lisp-form/number document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range printer-input selection (replacement-of operation))))
                                  (((the tree/leaf (printer-output (the lisp-form/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range printer-input
                                                                          '((the string (subseq (the string document) 0 0))
                                                                            (the string (write-to-string (the number document)))
                                                                            (the number (value-of (the lisp-form/number document))))
                                                                          (replacement-of operation))))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/number->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/symbol->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case (reverse selection)
                                  (((the string (name-of (the lisp-form/symbol document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the lisp-form/symbol document) ?projection ?recursion)) . ?rest)
                                   (make-operation/sequence/replace-range printer-input
                                                                          '((the string (subseq (the string document) 0 0))
                                                                            (the string (name-of (the lisp-form/symbol document))))
                                                                          (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/symbol->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/string->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case (reverse selection)
                                  (((the string (value-of (the lisp-form/string document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the lisp-form/string document) ?projection ?recursion)) . ?rest)
                                   (make-operation/sequence/replace-range printer-input
                                                                          '((the string (subseq (the string document) 0 0))
                                                                            (the string (value-of (the lisp-form/string document))))
                                                                          (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/string->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/quote->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/quote->tree/node nil)
                  (make-command/nothing (gesture-of input))))

(def reader lisp-form/list->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/lisp-form/list->tree/node 'backward-mapper/lisp-form/list->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-insert)
                       :domain "Lisp form" :description "Starts an insertion into the elements of the list"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (subseq (the sequence document) ,elements-length ,elements-length))
                                                                                           (the sequence (elements-of (the lisp-form/list document))))
                                                                           (make-document/sequence (vector (as (document/insertion :selection '((the string (subseq (the string document) 0 0))
                                                                                                                                                (the string (value-of (the document/insertion document))))))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/list->tree/node nil)
                    (make-command/nothing (gesture-of input)))))

(def reader lisp-form/object->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/object->tree/leaf nil)
                  (make-command/nothing (gesture-of input))))

(def reader lisp-form/top-level->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/lisp-form/top-level->tree/node 'backward-mapper/lisp-form/top-level->tree/node)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/lisp-form/top-level->tree/node nil)
                  (make-command/nothing (gesture-of input))))
