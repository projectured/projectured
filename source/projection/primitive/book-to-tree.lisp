;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection book/book->tree/node ()
  ())

(def projection book/chapter->tree/node ()
  ())

(def projection book/paragraph->tree/leaf ()
  ())

(def projection book/list->tree/node ()
  ())

(def projection book/picture->tree/leaf ()
  ())

;;;;;;
;;; IO map

(def iomap iomap/book/book->tree/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/chapter->tree/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/paragraph->tree/leaf ()
  ((content-iomap :type iomap)))

(def iomap iomap/book/list->tree/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/picture->tree/leaf ()
  ((content-iomap :type iomap)))

;;;;;;
;;; Construction

(def function make-projection/book/book->tree/node ()
  (make-projection 'book/book->tree/node))

(def function make-projection/book/chapter->tree/node ()
  (make-projection 'book/chapter->tree/node))

(def function make-projection/book/paragraph->tree/leaf ()
  (make-projection 'book/paragraph->tree/leaf))

(def function make-projection/book/list->tree/node ()
  (make-projection 'book/list->tree/node))

(def function make-projection/book/picture->tree/leaf ()
  (make-projection 'book/picture->tree/leaf))

;;;;;;
;;; Construction

(def macro book/book->tree/node ()
  '(make-projection/book/book->tree/node))

(def macro book/chapter->tree/node ()
  '(make-projection/book/chapter->tree/node))

(def macro book/paragraph->tree/leaf ()
  '(make-projection/book/paragraph->tree/leaf))

(def macro book/list->tree/node ()
  '(make-projection/book/list->tree/node))

(def macro book/picture->tree/leaf ()
  '(make-projection/book/picture->tree/leaf))

;;;;;;
;;; Forward mapper

(def function forward-mapper/book/book->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the book/book document))
       '((the tree/node document)))
      (((the string (title-of (the book/book document)))
        (the string document))
       '((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the tree/leaf document)))
      (((the string (author-of (the book/book document)))
        (the string document))
       '((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 1))
         (the tree/leaf document)))
      (((the string (title-of (the book/book document)))
        (the string (subseq (the string document) ?start-character-index ?end-character-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
      (((the string (author-of (the book/book document)))
        (the string (subseq (the string document) ?start-character-index ?end-character-index)))
       `((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 1))
         (the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
      (((the sequence (elements-of (the book/book document)))
        (the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
       (bind ((element-iomap (elt (element-iomaps-of printer-iomap) ?element-index))
              (element-output (output-of element-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(form-type element-output) (elt (the sequence document) ,(+ ?element-index (if (author-of printer-input) 2 1)))))
                 ?rest
                 element-iomap)))
      (((the tree/node (printer-output (the book/book document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/book/chapter->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the book/chapter document))
       '((the tree/node document)))
      (((the string (title-of (the book/chapter document)))
        (the string document))
       '((the sequence (children-of (the tree/node document)))
         (the tree/leaf (elt (the sequence document) 0))
         (the tree/leaf document)))
      (((the string (title-of (the book/chapter document)))
        (the string (subseq (the string document) ?start-character-index ?end-character-index)))
       (bind ((start-character-index (+ ?start-character-index (length (numbering-of (input-of printer-iomap))) 1))
              (end-character-index (+ ?end-character-index (length (numbering-of (input-of printer-iomap))) 1)))
         `((the sequence (children-of (the tree/node document)))
           (the tree/leaf (elt (the sequence document) 0))
           (the text/text (content-of (the tree/leaf document)))
           (the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index)))))
      (((the sequence (elements-of (the book/chapter document)))
        (the ?element-type (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((element-iomap (elt (element-iomaps-of printer-iomap) ?element-index))
              (element-output (output-of element-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the ,(form-type element-output) (elt (the sequence document) ,(+ ?element-index 1))))
                 ?rest
                 element-iomap)))
      (((the tree/node (printer-output (the book/chapter document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/book/paragraph->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the book/paragraph document))
       '((the tree/leaf document)))
      (((the ?type (content-of (the book/paragraph document))))
       `((the text/text (content-of (the tree/leaf document)))))
      (((the ?type (content-of (the book/paragraph document)))
        . ?rest)
       (bind ((content-iomap (content-iomap-of printer-iomap)))
         (values `((the text/text (content-of (the tree/leaf document))))
                 ?rest
                 content-iomap)))
      (((the tree/leaf (printer-output (the book/paragraph document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/book/list->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the book/list document))
       '((the tree/node document)))
      (((the sequence (elements-of (the book/list document)))
        (the ?element-type (elt (the sequence document) ?element-index))
        . ?rest)
       (bind ((element-iomap (elt (element-iomaps-of printer-iomap) ?element-index))
              (element-output (output-of element-iomap)))
         (values `((the sequence (children-of (the tree/node document)))
                   (the tree/node (elt (the sequence document) ,?element-index))
                   (the sequence (children-of (the tree/node document)))
                   (the ,(form-type element-output) (elt (the sequence document) 0)))
                 ?rest
                 element-iomap)))
      (((the tree/node (printer-output (the book/list document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

(def function forward-mapper/book/picture->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the book/picture document))
       '((the tree/leaf document)))
      (((the image/file (content-of (the book/picture document)))
        (the string (filename-of (the image/file document)))
        (the string (subseq (the string document) ?start-character-index ?end-character-index)))
       `((the text/text (content-of (the tree/leaf document)))
         (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
      (((the tree/leaf (printer-output (the book/picture document) ?projection ?recursion)) . ?rest)
       (when (eq projection ?projection)
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/book/book->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the book/book document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the tree/leaf document))
       '((the string (title-of (the book/book document)))
         (the string document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (if (string= (title-of printer-input) "")
           (append `((the tree/node (printer-output (the book/book document) ,projection ,recursion))) reference)
           `((the string (title-of (the book/book document)))
             (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))
      (((the sequence (children-of (the tree/node document)))
        (the ?child-type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((author (author-of printer-input)))
         (if (and author (= ?child-index 1))
             (pattern-case reference
               (((the sequence (children-of (the tree/node document)))
                 (the tree/leaf (elt (the sequence document) 1))
                 (the tree/leaf document))
                '((the string (author-of (the book/book document)))
                  (the string document)))
               (((the sequence (children-of (the tree/node document)))
                 (the tree/leaf (elt (the sequence document) 1))
                 (the text/text (content-of (the tree/leaf document)))
                 (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                (if (string= (author-of printer-input) "")
                    (append `((the tree/node (printer-output (the book/book document) ,projection ,recursion))) reference)
                    `((the string (author-of (the book/book document)))
                      (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))
               (?
                (append `((the tree/node (printer-output (the book/book document) ,projection ,recursion))) reference)))
             (bind ((element-index (- ?child-index (if (author-of printer-input) 2 1)))
                    (element (elt (elements-of printer-input) element-index))
                    (element-iomap (elt (element-iomaps-of printer-iomap) element-index)))
               (values `((the sequence (elements-of (the book/book document)))
                         (the ,(form-type element) (elt (the sequence document) ,element-index)))
                       ?rest
                       element-iomap)))))
      (?
       (append `((the tree/node (printer-output (the book/book document) ,projection ,recursion))) reference)))))

(def function backward-mapper/book/chapter->tree/node (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the book/chapter document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the tree/leaf document))
       '((the string (title-of (the book/chapter document)))
         (the string document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/leaf (elt (the sequence document) 0))
        (the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((start-character-index (- ?start-character-index (length (numbering-of printer-input)) 1))
              (end-character-index (- ?end-character-index (length (numbering-of printer-input)) 1)))
         (if (or (< start-character-index 0)
                 (< end-character-index 0)
                 (string= (title-of printer-input) ""))
             (append `((the tree/node (printer-output (the book/chapter document) ,projection ,recursion))) reference)
             `((the string (title-of (the book/chapter document)))
               (the string (subseq (the string document) ,start-character-index ,end-character-index))))))
      (((the sequence (children-of (the tree/node document)))
        (the ?child-type (elt (the sequence document) ?child-index))
        . ?rest)
       (bind ((element-index (- ?child-index 1))
              (element (elt (elements-of printer-input) element-index))
              (element-iomap (elt (element-iomaps-of printer-iomap) element-index)))
         (values `((the sequence (elements-of (the book/chapter document)))
                   (the ,(form-type element) (elt (the sequence document) ,element-index)))
                 ?rest
                 element-iomap)))
      (?
       (append `((the tree/node (printer-output (the book/chapter document) ,projection ,recursion))) reference)))))

(def function backward-mapper/book/paragraph->tree/leaf (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the book/paragraph document)))
      (((the text/text (content-of (the tree/leaf document)))
        . ?rest)
       (bind ((content-iomap (content-iomap-of printer-iomap)))
         (if (zerop (text/length (output-of content-iomap)))
             (append `((the tree/leaf (printer-output (the book/paragraph document) ,projection ,recursion))) reference)
             (values nil ?rest content-iomap))))
      (?
       (append `((the tree/leaf (printer-output (the book/paragraph document) ,projection ,recursion))) reference)))))

(def function backward-mapper/book/list->tree/node (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/node document))
       '((the book/list document)))
      (((the sequence (children-of (the tree/node document)))
        (the tree/node (elt (the sequence document) ?child-index))
        (the sequence (children-of (the tree/node document)))
        (the ?child-type (elt (the sequence document) 0))
        . ?rest)
       (bind ((element-iomap (elt (element-iomaps-of printer-iomap) ?child-index))
              (element-type (form-type (elt (elements-of (input-of printer-iomap)) ?child-index))))
         (values `((the sequence (elements-of (the book/list document)))
                   (the ,element-type (elt (the sequence document) ,?child-index)))
                 ?rest
                 element-iomap)))
      (?
       (append `((the tree/node (printer-output (the book/list document) ,projection ,recursion))) reference)))))

(def function backward-mapper/book/picture->tree/leaf (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case reference
      (((the tree/leaf document))
       '((the book/picture document)))
      (((the text/text (content-of (the tree/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (if (string= (filename-of (content-of printer-input)) "")
           (append `((the tree/leaf (printer-output (the book/picture document) ,projection ,recursion))) reference)
           `((the image/file (content-of (the book/picture document)))
             (the string (filename-of (the image/file document)))
             (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))
      (?
       (append `((the tree/leaf (printer-output (the book/picture document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer book/book->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (map-ll* (ll (elements-of input)) (lambda (element index)
                                                                 (recurse-printer recursion (value-of element)
                                                                                  `((elt (the sequence document) ,index)
                                                                                    (the sequence (elements-of (the book/book document)))
                                                                                    ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap 'iomap/book/book->tree/node
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :element-iomaps element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/book/book->tree/node)))
         (output (make-tree/node (as (append-ll (list-ll (ll (append (list (tree/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                                             (as (text/make-default-text (title-of input) "enter book title" :selection (as (nthcdr 3 (va output-selection))) :font *font/liberation/serif/bold/42* :font-color *color/solarized/red*))))
                                                                     (when-bind author (author-of input)
                                                                       (list (make-tree/leaf (as (text/make-default-text author "enter author" :selection (as (nthcdr 3 (va output-selection))) :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*))
                                                                                             :opening-delimiter (text/make-simple-text "Written by " :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*)
                                                                                             :indentation 0
                                                                                             :selection (as (nthcdr 2 (va output-selection))))))))
                                                         (map-ll (va element-iomaps) (lambda (element-iomap)
                                                                                       (bind ((element-output (output-of element-iomap)))
                                                                                         (etypecase element-output
                                                                                           (tree/leaf (tree/clone-leaf element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (tree/node (tree/clone-node element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (t element-output))))))))
                                 :selection output-selection)))
    (make-iomap 'iomap/book/book->tree/node
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :element-iomaps element-iomaps)))

(def printer book/chapter->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (map-ll* (ll (elements-of input)) (lambda (element index)
                                                                 (recurse-printer recursion (value-of element)
                                                                                  `((elt (the sequence document) ,index)
                                                                                    (the sequence (elements-of (the book/chapter document)))
                                                                                    ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap 'iomap/book/chapter->tree/node
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :element-iomaps element-iomaps)
                                                (selection-of input)
                                                'forward-mapper/book/chapter->tree/node)))
         (title-output (as (bind ((title (title-of input))
                                  (title? (string= title ""))
                                  (title-font (pattern-case input-reference
                                                (((elt (the sequence document) ?subindex)
                                                  (the sequence (elements-of (the book/chapter document)))
                                                  (the book/chapter (elt (the sequence document) ?index))
                                                  (the sequence (elements-of (the book/chapter document)))
                                                  . ?rest)
                                                 *font/liberation/serif/regular/24*)
                                                (((elt (the sequence document) ?index)
                                                  (the sequence (elements-of (the book/chapter document)))
                                                  . ?rest)
                                                 *font/liberation/serif/regular/30*)
                                                (?
                                                 *font/liberation/serif/bold/36*)))
                                  (font-color (if title?
                                                  (color/lighten *color/solarized/blue* 0.75)
                                                  *color/solarized/blue*))
                                  (numbering (numbering-of input)))
                             (if numbering
                                 (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                   (text/string numbering :font title-font :font-color font-color :padding (make-inset :top 10))
                                   (text/spacing 30)
                                   (text/string (if (string= title "") "enter chapter title" title) :font title-font :font-color font-color :padding (make-inset :top 10)))
                                 (text/make-default-text title "enter chapter title" :font title-font :font-color font-color :padding (make-inset :top 10) :selection (as (nthcdr 3 (va output-selection))))))))
         (output (make-tree/node (as (append-ll (list-ll (list-ll (tree/leaf (:selection (as (nthcdr 2 (va output-selection)))) title-output))
                                                         (map-ll (va element-iomaps) (lambda (element-iomap)
                                                                                       (bind ((element-output (output-of element-iomap)))
                                                                                         (etypecase element-output
                                                                                           (tree/leaf (tree/clone-leaf element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (tree/node (tree/clone-node element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (t element-output))))))))
                                 :collapsed (as (collapsed-p input))
                                 :selection output-selection)))
    (make-iomap 'iomap/book/chapter->tree/node
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :element-iomaps element-iomaps)))

(def printer book/paragraph->tree/leaf (projection recursion input input-reference)
  (bind ((content-iomap (as (recurse-printer recursion input input-reference)))
         (output-selection (as (print-selection (make-iomap 'iomap/book/paragraph->tree/leaf
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :content-iomap content-iomap)
                                                (selection-of input)
                                                'forward-mapper/book/paragraph->tree/leaf)))
         (output (as (bind ((content-output (output-of (va content-iomap))))
                       (tree/leaf (:selection output-selection)
                         (if (zerop (text/length content-output))
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string "enter paragraph text" :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/content/darker* 0.75) :padding (make-inset :top 10)))
                             (text/make-text (map-ll* (ll (elements-of content-output)) (lambda (element index)
                                                                                          (bind ((value (value-of element)))
                                                                                            (if (previous-element-of element)
                                                                                                value
                                                                                                (etypecase value
                                                                                                  (text/string (text/clone-string value :padding (make-inset :top 10)))
                                                                                                  (text/graphics value))))))
                                             :selection (as (nthcdr 1 (va output-selection))))))))))
    (make-iomap 'iomap/book/paragraph->tree/leaf
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :content-iomap content-iomap)))

(def printer book/list->tree/node (projection recursion input input-reference)
  (bind ((element-iomaps (as (map-ll* (ll (elements-of input)) (lambda (element index)
                                                                 (recurse-printer recursion (value-of element)
                                                                                  `((elt (the sequence document) ,index)
                                                                                    (the sequence (elements-of (the book/list document)))
                                                                                    ,@(typed-reference (form-type input) input-reference)))))))
         (output-selection (as (print-selection (make-iomap 'iomap/book/list->tree/node
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference
                                                            :element-iomaps (va element-iomaps))
                                                (selection-of input)
                                                'forward-mapper/book/list->tree/node)))
         (output (as (make-tree/node (map-ll* (va element-iomaps) (lambda (element-iomap index)
                                                                    (if (typep (input-of (value-of element-iomap)) 'book/list)
                                                                        (tree/node (:indentation 2 :selection (as (nthcdr 2 (va output-selection))))
                                                                          (output-of (value-of element-iomap)))
                                                                        (tree/node (:opening-delimiter (text/make-simple-text "• " :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker* :padding (make-inset :left 15))
                                                                                                       :indentation (unless (zerop index) 0)
                                                                                                       :selection (as (nthcdr 2 (va output-selection))))
                                                                          (output-of (value-of element-iomap))))))
                                     :indentation 2
                                     :selection output-selection))))
    (make-iomap 'iomap/book/list->tree/node
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :element-iomaps element-iomaps)))

(def printer book/picture->tree/leaf (projection recursion input input-reference)
  (bind ((content (content-of input))
         (filename (filename-of content))
         (absolute-filename (if (starts-with #\/ (namestring filename))
                                filename
                                (merge-pathnames filename (hu.dwim.asdf:system-pathname :projectured))))
         (filename-empty? (zerop (length (namestring filename))))
         (file-exists? (and (not filename-empty?)
                            (not (null (pathname-name absolute-filename)))
                            (not (null (pathname-type absolute-filename)))
                            (probe-file absolute-filename)))
         (filename-string (if filename-empty?
                              "enter picture path"
                              (namestring filename)))
         (filename-color (if filename-empty?
                             (color/lighten *color/solarized/gray* 0.75)
                             *color/solarized/gray*))
         (output-selection (as (unless file-exists?
                                 (print-selection (make-iomap 'iomap/book/picture->tree/leaf
                                                              :projection projection :recursion recursion
                                                              :input input :input-reference input-reference)
                                                  (selection-of input)
                                                  'forward-mapper/book/picture->tree/leaf))))
         (output (as (tree/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (if file-exists?
                             (content-of input)
                             (text/string filename-string :font *font/liberation/serif/regular/24* :font-color filename-color)))))))
    (make-iomap 'iomap/book/picture->tree/leaf
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output)))

;;;;;;
;;; Reader

(def reader book/book->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (title-of (the book/book document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the string (author-of (the book/book document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/node (printer-output (the book/book document) ?projection ?recursion))
                                    (the sequence (children-of (the tree/node document)))
                                    (the tree/leaf (elt (the sequence document) 0))
                                    . ?rest)
                                   (make-operation/string/replace-range printer-input '((the string (title-of (the book/book document)))
                                                                                        (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))
                                  (((the tree/node (printer-output (the book/book document) ?projection ?recursion))
                                    (the sequence (children-of (the tree/node document)))
                                    (the tree/leaf (elt (the sequence document) 1))
                                    . ?rest)
                                   (make-operation/string/replace-range printer-input '((the string (author-of (the book/book document)))
                                                                                        (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-delete)
                       :domain "Book" :description "Deletes the selected element from the book"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the sequence (elements-of (the book/book document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/book document)))
                                                                                            (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil)))))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/book/book->tree/node 'backward-mapper/book/book->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-insert)
                       :domain "Book" :description "Starts an insertion into the elements of the book"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/book document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (document/insertion :selection '((the string (value-of (the document/insertion document)))
                                                                                                                                                (the string (subseq (the string document) 0 0))))))))))
                      ((gesture/keyboard/key-press :key :sdl-key-a :modifiers :control)
                       :domain "Book" :description "Inserts a new chapter into the elements of the book"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/book document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                                                                                                                           (the string (subseq (the string document) 0 0))))))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/book/book->tree/node operation-mapper)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-delete)
                       :domain "Book" :description "Deletes the author of the book"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the string (author-of (the book/book document)))
                                      (the string document))
                                     (make-operation/replace-target printer-iomap (selection-of printer-input) nil)))))
                    (make-command/nothing (gesture-of input)))))

(def reader book/chapter->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the string (title-of (the book/chapter document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/node (printer-output (the book/chapter document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range printer-input '((the string (title-of (the book/chapter document)))
                                                                                        (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-delete)
                       :domain "Book" :description "Deletes the selected element from the chapter"
                       :operation (pattern-case (selection-of printer-input)
                                    (((the sequence (elements-of (the book/chapter document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/chapter document)))
                                                                                            (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil)))))
                    (command/read-selection recursion input printer-iomap 'forward-mapper/book/chapter->tree/node 'backward-mapper/book/chapter->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-insert)
                       :domain "Book" :description "Starts an insertion into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/chapter document)))
                                                                                                                          (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                          (make-document/sequence (vector (as (document/insertion :selection '((the string (value-of (the document/insertion document)))
                                                                                                                                                                               (the string (subseq (the string document) 0 0))))))))
                                                                   (make-operation/replace-selection printer-input `((the sequence (elements-of (the book/chapter document)))
                                                                                                                     (the document/insertion (elt (the sequence document) ,elements-length))
                                                                                                                     (the string (value-of (the document/insertion document)))
                                                                                                                     (the string (subseq (the string document) 0 0))))))))
                      ((gesture/keyboard/key-press :key :sdl-key-a :modifiers :control)
                       :domain "Book" :description "Inserts a new chapter into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/chapter document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                                                                                                                           (the string (subseq (the string document) 0 0)))))))))))
                      ((gesture/keyboard/key-press :key :sdl-key-r :modifiers :control)
                       :domain "Book" :description "Inserts a new paragraph into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/chapter document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                                                                                                                             (the text/text (text/subseq (the text/text document) 0 0))))
                                                                                                                 (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0))))
                                                                                                                   (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/book/chapter->tree/node operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader book/paragraph->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the tree/leaf (printer-output (the book/paragraph document) ?projection ?recursion)) . ?rest)
                                   (make-operation/text/replace-range printer-input '((the text/text (content-of (the book/paragraph document)))
                                                                                      (the text/text (text/subseq (the text/text document) 0 0)))
                                                                      (replacement-of operation)))))))))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/book/paragraph->tree/leaf 'backward-mapper/book/paragraph->tree/leaf)
                    (command/read-backward recursion input printer-iomap 'backward-mapper/book/paragraph->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))

(def reader book/list->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/book/list->tree/node 'backward-mapper/book/list->tree/node)
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :key :sdl-key-insert)
                       :domain "Book" :description "Starts an insertion into the elements of the list"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (document/insertion :selection '((the string (value-of (the document/insertion document)))
                                                                                                                                                (the string (subseq (the string document) 0 0))))))))))
                      ((gesture/keyboard/key-press :key :sdl-key-r :modifiers :control)
                       :domain "Book" :description "Inserts a new paragraph into the elements of the list"
                       :operation (bind ((elements-length (length (elements-of printer-input))))
                                    (make-operation/sequence/replace-range printer-input `((the sequence (elements-of (the book/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (make-document/sequence (vector (as (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                                                                                                                             (the text/text (text/subseq (the text/text document) 0 0))))
                                                                                                                 (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0))))
                                                                                                                   (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))))))
                    (command/read-backward recursion input printer-iomap 'backward-mapper/book/list->tree/node nil)
                    (make-command/nothing (gesture-of input)))))

(def reader book/picture->tree/leaf (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case selection
                                  (((the image/file (content-of (the book/picture document)))
                                    (the string (filename-of (the image/file document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range printer-input selection (replacement-of operation)))
                                  (((the tree/leaf (printer-output (the book/picture document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range printer-input '((the image/file (content-of (the book/picture document)))
                                                                                        (the string (filename-of (the image/file document)))
                                                                                        (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/book/picture->tree/leaf operation-mapper)
                    (make-command/nothing (gesture-of input)))))
