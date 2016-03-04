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
  ((fully-qualified :type boolean)))

(def projection lisp-form/string->tree/leaf ()
  ())

(def projection lisp-form/quote->tree/node ()
  ())

(def projection lisp-form/list->tree/node ()
  ())

(def projection lisp-form/object->tree/leaf ()
  ())

(def projection lisp-form/toplevel->tree/node ()
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
  (make-projection 'lisp-form/symbol->tree/leaf :fully-qualified #f))

(def function make-projection/lisp-form/string->tree/leaf ()
  (make-projection 'lisp-form/string->tree/leaf))

(def function make-projection/lisp-form/quote->tree/node ()
  (make-projection 'lisp-form/quote->tree/node))

(def function make-projection/lisp-form/list->tree/node ()
  (make-projection 'lisp-form/list->tree/node))

(def function make-projection/lisp-form/object->tree/leaf ()
  (make-projection 'lisp-form/object->tree/leaf))

(def function make-projection/lisp-form/toplevel->tree/node ()
  (make-projection 'lisp-form/toplevel->tree/node))

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

(def macro lisp-form/toplevel->tree/node ()
  '(make-projection/lisp-form/toplevel->tree/node))

;;;;;;
;;; Forward mapper

(def forward-mapper lisp-form/insertion->tree/leaf ()
  (pattern-case -reference-
    (((the string (value-of (the lisp-form/insertion document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the tree/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the tree/leaf (printer-output (the lisp-form/insertion document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/comment->tree/node ()
  (pattern-case -reference-
    (((the text/text (content-of (the lisp-form/comment document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the sequence (children-of (the tree/node document)))
       (the tree/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the tree/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the tree/node (printer-output (the lisp-form/comment document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/number->tree/leaf ()
  (pattern-case -reference-
    (((the ?type (value-of (the lisp-form/number document)))
      (the string (write-to-string (the ?type document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the tree/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the tree/leaf (printer-output (the lisp-form/number document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/symbol->tree/leaf ()
  (pattern-case -reference-
    (((the string (name-of (the lisp-form/symbol document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     (bind ((offset (if (fully-qualified-p -projection-) (+ 2 (length (package-of -printer-input-))) 0)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,(+ ?start-index offset) ,(+ ?end-index offset))))))
    (((the tree/leaf (printer-output (the lisp-form/symbol document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/string->tree/leaf ()
  (pattern-case -reference-
    (((the string (value-of (the lisp-form/string document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the tree/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the tree/leaf (printer-output (the lisp-form/string document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/quote->tree/node ()
  (pattern-case -reference-
    (((the ?type (value-of (the lisp-form/quote document)))
      . ?rest)
     (values `((the sequence (children-of (the tree/node document)))
               (the ,(document-type (output-of (content-iomap-of -printer-iomap-))) (elt (the sequence document) 0)))
             ?rest
             (content-iomap-of -printer-iomap-)))
    (((the tree/node (printer-output (the lisp-form/quote document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/list->tree/node ()
  (pattern-case -reference-
    (((the lisp-form/list document))
     `((the tree/node document)))
    (((the sequence (elements-of (the lisp-form/list document)))
      (the ?element-type (elt (the sequence document) ?element-index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?element-index))
            (element-output (output-of element-iomap)))
       (values `((the sequence (children-of (the tree/node document)))
                 (the ,(document-type element-output) (elt (the sequence document) ,?element-index)))
               ?rest
               element-iomap)))
    (((the tree/node (printer-output (the lisp-form/list document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/object->tree/leaf ()
  (pattern-case -reference-
    (((the tree/leaf (printer-output (the lisp-form/object document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper lisp-form/toplevel->tree/node ()
  (pattern-case -reference-
    (((the lisp-form/toplevel document))
     `((the tree/node document)))
    (((the sequence (elements-of (the lisp-form/toplevel document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (children-of (the tree/node document)))
                 (the ,(document-type (output-of child-iomap)) (elt (the sequence document) ,?child-index)))
               ?rest
               child-iomap)))
    (((the tree/node (printer-output (the lisp-form/toplevel document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper lisp-form/insertion->tree/leaf ()
  (pattern-case -reference-
    (((the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the string (value-of (the lisp-form/insertion document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    (?a
     (append `((the tree/leaf (printer-output (the lisp-form/insertion document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/comment->tree/node ()
  (pattern-case -reference-
    (((the sequence (children-of (the tree/node document)))
      (the tree/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the text/text (content-of (the lisp-form/comment document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (?a
     (append `((the tree/node (printer-output (the lisp-form/comment document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/number->tree/leaf ()
  (pattern-case -reference-
    (((the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (if (string= "" (write-number (value-of -printer-input-) nil))
         (append `((the tree/leaf (printer-output (the lisp-form/number document) ,-projection- ,-recursion-))) -reference-)
         (bind ((type (document-type (value-of -printer-input-))))
           `((the ,type (value-of (the lisp-form/number document)))
             (the string (write-to-string (the ,type document)))
             (the string (subseq (the string document) ,?start-index ,?end-index))))))
    (?a
     (append `((the tree/leaf (printer-output (the lisp-form/number document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/symbol->tree/leaf ()
  (pattern-case -reference-
    (((the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (if (string= (name-of -printer-input-) "")
         (append `((the tree/leaf (printer-output (the lisp-form/symbol document) ,-projection- ,-recursion-))) -reference-)
         (bind ((offset (if (fully-qualified-p -projection-) (+ 2 (length (package-of -printer-input-))) 0)))
           `((the string (name-of (the lisp-form/symbol document)))
             (the string (subseq (the string document) ,(- ?start-index offset) ,(- ?end-index offset)))))))
    (?a
     (append `((the tree/leaf (printer-output (the lisp-form/symbol document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/string->tree/leaf ()
  (pattern-case -reference-
    (((the text/text (content-of (the tree/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (if (string= (value-of -printer-input-) "")
         (append `((the tree/leaf (printer-output (the lisp-form/string document) ,-projection- ,-recursion-))) -reference-)
         `((the string (value-of (the lisp-form/string document)))
           (the string (subseq (the string document) ,?start-index ,?end-index)))))
    (?a
     (append `((the tree/leaf (printer-output (the lisp-form/string document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/quote->tree/node ()
  (pattern-case -reference-
    (((the sequence (children-of (the tree/node document)))
      (the tree/leaf (elt (the sequence document) 0))
      . ?rest)
     (values `((the ,(document-type (input-of (content-iomap-of -printer-iomap-))) (value-of (the lisp-form/quote document))))
             ?rest
             (content-iomap-of -printer-iomap-)))
    (?a
     (append `((the tree/node (printer-output (the lisp-form/quote document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/list->tree/node ()
  (pattern-case -reference-
    (((the tree/node document))
     `((the lisp-form/list document)))
    (((the sequence (children-of (the tree/node document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((element (elt (elements-of -printer-input-) ?child-index))
            (element-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (elements-of (the lisp-form/list document)))
                 (the ,(document-type element) (elt (the sequence document) ,?child-index)))
               ?rest
               element-iomap)))
    (?a
     (append `((the tree/node (printer-output (the lisp-form/list document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/object->tree/leaf ()
  (pattern-case -reference-
    (?a
     (append `((the tree/leaf (printer-output (the lisp-form/object document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper lisp-form/toplevel->tree/node ()
  (pattern-case -reference-
    (((the tree/node document))
     `((the lisp-form/toplevel document)))
    (((the sequence (children-of (the tree/node document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((element (elt (elements-of -printer-input-) ?child-index))
            (element-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (elements-of (the lisp-form/toplevel document)))
                 (the ,(document-type element) (elt (the sequence document) ,?child-index)))
               ?rest
               element-iomap)))
    (?a
     (append `((the tree/node (printer-output (the lisp-form/toplevel document) ,-projection- ,-recursion-))) -reference-))))

;;;;;;
;;; Printer

(def printer lisp-form/insertion->tree/leaf ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/insertion->tree/leaf)))
         (output (as (bind (((:values nil completion) (funcall (factory-of -input-) (factory-of -input-) -input- nil (value-of -input-)))
                            (commitable? (not (null (funcall (factory-of -input-) (factory-of -input-) -input- nil (string+ (value-of -input-) completion)))))
                            (value-color (if commitable? *color/solarized/green* *color/solarized/red*)))
                       (tree/leaf (:selection output-selection :indentation (indentation-of -input-)
                                   :opening-delimiter (when (compound-p -input-)
                                                        (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                   :closing-delimiter (when (compound-p -input-)
                                                        (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))))
                         (if (and (zerop (length (value-of -input-)))
                                  (not (compound-p -input-)))
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string (default-value-of -input-) :font *font/ubuntu/monospace/regular/24* :font-color (color/lighten value-color 0.75)))
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string (value-of -input-) :font *font/ubuntu/monospace/regular/24* :font-color value-color)
                               (text/string (if completion (if commitable? (string+ completion "?") completion) "") :font *font/ubuntu/monospace/regular/24* :font-color (color/lighten value-color 0.75)))))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/comment->tree/node ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) -input-reference-)))
         (output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/comment->tree/node)))
         ;; TODO:
         (output (as (make-tree/node (list (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                             (output-of (va content-iomap))))
                                     :indentation (indentation-of -input-)
                                     :opening-delimiter (text/text () (text/string ";; " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/number->tree/leaf ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/number->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection :indentation (indentation-of -input-))
                       (text/make-default-text (write-number (value-of -input-) nil) "enter lisp number" :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/symbol->tree/leaf ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/symbol->tree/leaf)))

         (output (as (bind ((font-color (or (font-color-of -input-) *color/solarized/violet*))
                            (name (name-of -input-))
                            (name-string (string-downcase (if (string= "KEYWORD" (package-of -input-)) (string+ ":" name) name))))
                       (tree/leaf (:selection output-selection :indentation (indentation-of -input-))
                         (if (fully-qualified-p -projection-)
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string (string+ (string-downcase (package-of -input-)) "::" ) :font (or (font-of -input-) *font/ubuntu/monospace/regular/24*) :font-color (color/lighten font-color 0.5))
                               (text/string name-string :font (or (font-of -input-) *font/ubuntu/monospace/bold/24*) :font-color font-color))
                             (text/make-default-text name-string (or (default-value-of -input-) "enter lisp symbol") :selection (as (nthcdr 1 (va output-selection))) :font (or (font-of -input-) *font/ubuntu/monospace/regular/24*) :font-color font-color)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/string->tree/leaf ()
  (bind ((value (write-to-string (value-of -input-)))
         (output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/string->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection :indentation (indentation-of -input-)
                                 :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                 :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                       (text/make-default-text (subseq value 1 (1- (length value))) (or (default-value-of -input-) "enter lisp string") :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/quote->tree/node ()
  (bind ((value-iomap (as (recurse-printer -recursion- (value-of -input-)
                                           `((value-of (the sequence document))
                                             ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection (make-iomap/content -projection- -recursion- -input- -input-reference- nil value-iomap)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/quote->tree/node)))
         (output (as (tree/node (:selection output-selection :indentation (indentation-of -input-)
                                 :opening-delimiter (text/text () (text/string "'" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                       (output-of (va value-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output value-iomap)))

(def printer lisp-form/list->tree/node ()
  (bind ((deep-list (find-if (of-type 'lisp-form/list) (elements-of -input-)))
         (element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence (elements-of -input-))
                                   (collect (recurse-printer -recursion- element
                                                             `((elt (the sequence document) ,index)
                                                               (the sequence (elements-of (the document list-form/list)))
                                                               ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection (make-iomap/compound -projection- -recursion- -input- -input-reference- nil element-iomaps)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/list->tree/node)))
         (output (as (bind ((separator-font-color *color/solarized/gray*)
                            (separator-fill-color nil))
                       (make-tree/node (as (map-ll (ll (va element-iomaps))
                                                   (lambda (element-iomap)
                                                     (bind ((element-output (output-of element-iomap)))
                                                       (typecase element-output
                                                         (tree/base (tree/clone element-output :indentation (indentation-of element-output)))
                                                         (lisp-form/base (lisp-form/clone element-output :indentation (indentation-of element-output)))
                                                         (t (when (and deep-list (not (first-iteration-p)))
                                                              (break "~A" element-output)
                                                              2)))))))
                                       :indentation (indentation-of -input-)
                                       :collapsed (as (collapsed-p -input-))
                                       :selection output-selection
                                       :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                       :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))))
                     #+nil ;; TODO: to colorize
                     (bind ((separator-font-color (as (color/darken/selection *color/solarized/gray* (va output-selection))))
                            (separator-fill-color (as (color/lighten/selection (color/lighten *color/solarized/cyan* 0.75) (va output-selection) nil))))
                       (make-tree/node (mapcar 'output-of (va element-iomaps))
                                       :selection output-selection
                                       :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                       :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer lisp-form/object->tree/leaf ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/object->tree/leaf)))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (write-to-string -input-) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer lisp-form/toplevel->tree/node ()
  (bind ((element-iomaps (as (map-ll* (elements-of -input-) (lambda (element index)
                                                              (recurse-printer -recursion- (value-of element)
                                                                               `((elt (the sequence document) ,index)
                                                                                 (the sequence (elements-of (the lisp-form/toplevel document)))
                                                                                 ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection (make-iomap/compound -projection- -recursion- -input- -input-reference- nil element-iomaps)
                                                (get-selection -input-)
                                                'forward-mapper/lisp-form/toplevel->tree/node)))
         (output (as (make-tree/node (map-ll (va element-iomaps) 'output-of)
                                     :indentation (indentation-of -input-)
                                     :collapsed (as (collapsed-p -input-))
                                     :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                     :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

;;;;;;
;;; Reader

(def reader lisp-form/insertion->tree/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (value-of (the lisp-form/insertion document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/insertion->tree/leaf operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader lisp-form/comment->tree/node ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/comment->tree/node nil)
                  (make-nothing-command -gesture-)))

(def reader lisp-form/number->tree/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the ?type (value-of (the lisp-form/number document)))
                                    (the string (write-to-string (the ?type document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input- selection (replacement-of operation))))
                                  (((the tree/leaf (printer-output (the lisp-form/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (bind ((type (etypecase (value-of -printer-input-)
                                                    ((or null number) 'number)
                                                    (primitive/number 'primitive/number))))
                                       (make-operation/number/replace-range -printer-input-
                                                                            `((the ,type (value-of (the lisp-form/number document)))
                                                                              (the string (write-to-string (the ,type document)))
                                                                              (the string (subseq (the string document) 0 0)))
                                                                            (replacement-of operation)))))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/number->tree/leaf operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader lisp-form/symbol->tree/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (name-of (the lisp-form/symbol document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the lisp-form/symbol document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (name-of (the lisp-form/symbol document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-q :control)
                       :domain "Lisp form" :description "Toggles displaying fully qualified symbol names."
                       :operation (make-operation/functional (lambda () (notf (fully-qualified-p -projection-))))))
                    (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/symbol->tree/leaf operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader lisp-form/string->tree/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (value-of (the lisp-form/string document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the lisp-form/string document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (value-of (the lisp-form/string document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/string->tree/leaf operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader lisp-form/quote->tree/node ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/quote->tree/node nil)
                  (make-nothing-command -gesture-)))

(def reader lisp-form/list->tree/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/lisp-form/list->tree/node 'backward-mapper/lisp-form/list->tree/node)
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-insert)
                     :domain "Lisp form" :description "Starts an insertion into the elements of the list"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the lisp-form/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                         (make-document/sequence (vector (as (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                                               (the string (subseq (the string document) 0 0))))))))))))
                  (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/list->tree/node nil)
                  (make-nothing-command -gesture-)))

(def reader lisp-form/object->tree/leaf ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/object->tree/leaf nil)
                  (make-nothing-command -gesture-)))

(def reader lisp-form/toplevel->tree/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap- 'forward-mapper/lisp-form/toplevel->tree/node 'backward-mapper/lisp-form/toplevel->tree/node)
                  (command/read-backward -recursion- -input- -printer-iomap- 'backward-mapper/lisp-form/toplevel->tree/node nil)
                  (make-nothing-command -gesture-)))
