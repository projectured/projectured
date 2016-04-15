;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection book/book->syntax/node ()
  ())

(def projection book/chapter->syntax/node ()
  ())

(def projection book/paragraph->syntax/leaf ()
  ())

(def projection book/list->syntax/node ()
  ())

(def projection book/picture->syntax/leaf ()
  ())

;;;;;;
;;; IO map

(def iomap iomap/book/book->syntax/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/chapter->syntax/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/paragraph->syntax/leaf ()
  ((content-iomap :type iomap)))

(def iomap iomap/book/list->syntax/node ()
  ((element-iomaps :type sequence)))

(def iomap iomap/book/picture->syntax/leaf ()
  ((content-iomap :type iomap)))

;;;;;;
;;; Construction

(def function make-projection/book/book->syntax/node ()
  (make-projection 'book/book->syntax/node))

(def function make-projection/book/chapter->syntax/node ()
  (make-projection 'book/chapter->syntax/node))

(def function make-projection/book/paragraph->syntax/leaf ()
  (make-projection 'book/paragraph->syntax/leaf))

(def function make-projection/book/list->syntax/node ()
  (make-projection 'book/list->syntax/node))

(def function make-projection/book/picture->syntax/leaf ()
  (make-projection 'book/picture->syntax/leaf))

;;;;;;
;;; Construction

(def macro book/book->syntax/node ()
  '(make-projection/book/book->syntax/node))

(def macro book/chapter->syntax/node ()
  '(make-projection/book/chapter->syntax/node))

(def macro book/paragraph->syntax/leaf ()
  '(make-projection/book/paragraph->syntax/leaf))

(def macro book/list->syntax/node ()
  '(make-projection/book/list->syntax/node))

(def macro book/picture->syntax/leaf ()
  '(make-projection/book/picture->syntax/leaf))

;;;;;;
;;; Forward mapper

(def forward-mapper book/book->syntax/node ()
  (reference-case -reference-
    (((the book/book document))
     '((the syntax/node document)))
    (((the string (title-of (the book/book document)))
      (the string document))
     '((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the syntax/leaf document)))
    (((the string (author-of (the book/book document)))
      (the string document))
     '((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 1))
       (the syntax/leaf document)))
    (((the string (title-of (the book/book document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
    (((the string (author-of (the book/book document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 1))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))
    (((the sequence (elements-of (the book/book document)))
      (the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
     (bind ((element-iomap (elt (element-iomaps-of -printer-iomap-) ?element-index))
            (element-output (output-of element-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type element-output) (elt (the sequence document) ,(+ ?element-index (if (author-of -printer-input-) 2 1)))))
               ?rest
               element-iomap)))))

(def forward-mapper book/chapter->syntax/node ()
  (reference-case -reference-
    (((the book/chapter document))
     '((the syntax/node document)))
    (((the string (title-of (the book/chapter document)))
      (the string document))
     '((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the syntax/leaf document)))
    (((the string (title-of (the book/chapter document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     (bind ((start-character-index (+ ?start-character-index (length (numbering-of (input-of -printer-iomap-))) 1))
            (end-character-index (+ ?end-character-index (length (numbering-of (input-of -printer-iomap-))) 1)))
       `((the sequence (children-of (the syntax/node document)))
         (the syntax/leaf (elt (the sequence document) 0))
         (the text/text (content-of (the syntax/leaf document)))
         (the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index)))))
    (((the sequence (elements-of (the book/chapter document)))
      (the ?element-type (elt (the sequence document) ?element-index))
      . ?rest)
     (bind ((element-iomap (elt (element-iomaps-of -printer-iomap-) ?element-index))
            (element-output (output-of element-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type element-output) (elt (the sequence document) ,(+ ?element-index 1))))
               ?rest
               element-iomap)))))

(def forward-mapper book/paragraph->syntax/leaf ()
  (reference-case -reference-
    (((the book/paragraph document))
     '((the syntax/leaf document)))
    (((the ?type (content-of (the book/paragraph document))))
     `((the text/text (content-of (the syntax/leaf document)))))
    (((the ?type (content-of (the book/paragraph document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (values `((the text/text (content-of (the syntax/leaf document))))
               ?rest
               content-iomap)))))

(def forward-mapper book/list->syntax/node ()
  (reference-case -reference-
    (((the book/list document))
     '((the syntax/node document)))
    (((the sequence (elements-of (the book/list document)))
      (the ?element-type (elt (the sequence document) ?element-index))
      . ?rest)
     (bind ((element-iomap (elt (element-iomaps-of -printer-iomap-) ?element-index))
            (element-output (output-of element-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the syntax/node (elt (the sequence document) ,?element-index))
                 (the sequence (children-of (the syntax/node document)))
                 (the ,(document-type element-output) (elt (the sequence document) 0)))
               ?rest
               element-iomap)))))

(def forward-mapper book/picture->syntax/leaf ()
  (reference-case -reference-
    (((the book/picture document))
     '((the syntax/leaf document)))
    (((the image/file (content-of (the book/picture document)))
      (the string (filename-of (the image/file document)))
      (the string (subseq (the string document) ?start-character-index ?end-character-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-character-index ,?end-character-index))))))

;;;;;;
;;; Backward mapper

(def backward-mapper book/book->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     '((the book/book document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the syntax/leaf document))
     '((the string (title-of (the book/book document)))
       (the string document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (unless (string= (title-of -printer-input-) "")
       `((the string (title-of (the book/book document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))
    (((the sequence (children-of (the syntax/node document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((author (author-of -printer-input-)))
       (if (and author (= ?child-index 1))
           (reference-case -reference-
             (((the sequence (children-of (the syntax/node document)))
               (the syntax/leaf (elt (the sequence document) 1))
               (the syntax/leaf document))
              '((the string (author-of (the book/book document)))
                (the string document)))
             (((the sequence (children-of (the syntax/node document)))
               (the syntax/leaf (elt (the sequence document) 1))
               (the text/text (content-of (the syntax/leaf document)))
               (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
              (unless (string= (author-of -printer-input-) "")
                `((the string (author-of (the book/book document)))
                  (the string (subseq (the string document) ,?start-character-index ,?end-character-index))))))
           (bind ((element-index (- ?child-index (if (author-of -printer-input-) 2 1)))
                  (element (elt (elements-of -printer-input-) element-index))
                  (element-iomap (elt (element-iomaps-of -printer-iomap-) element-index)))
             (values `((the sequence (elements-of (the book/book document)))
                       (the ,(document-type element) (elt (the sequence document) ,element-index)))
                     ?rest
                     element-iomap)))))))

(def backward-mapper book/chapter->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     '((the book/chapter document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the syntax/leaf document))
     '((the string (title-of (the book/chapter document)))
       (the string document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (bind ((start-character-index (- ?start-character-index (length (numbering-of -printer-input-)) 1))
            (end-character-index (- ?end-character-index (length (numbering-of -printer-input-)) 1)))
       (unless (or (< start-character-index 0)
                   (< end-character-index 0)
                   (string= (title-of -printer-input-) ""))
         `((the string (title-of (the book/chapter document)))
           (the string (subseq (the string document) ,start-character-index ,end-character-index))))))
    (((the sequence (children-of (the syntax/node document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((element-index (- ?child-index 1))
            (element (elt (elements-of -printer-input-) element-index))
            (element-iomap (elt (element-iomaps-of -printer-iomap-) element-index)))
       (values `((the sequence (elements-of (the book/chapter document)))
                 (the ,(document-type element) (elt (the sequence document) ,element-index)))
               ?rest
               element-iomap)))))

(def backward-mapper book/paragraph->syntax/leaf ()
  (reference-case -reference-
    (((the syntax/leaf document))
     '((the book/paragraph document)))
    (((the text/text (content-of (the syntax/leaf document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (unless (zerop (text/length (output-of content-iomap)))
         (values nil ?rest content-iomap))))))

(def backward-mapper book/list->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     '((the book/list document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) ?child-index))
      (the sequence (children-of (the syntax/node document)))
      (the ?child-type (elt (the sequence document) 0))
      . ?rest)
     (bind ((element-iomap (elt (element-iomaps-of -printer-iomap-) ?child-index))
            (element-type (document-type (elt (elements-of (input-of -printer-iomap-)) ?child-index))))
       (values `((the sequence (elements-of (the book/list document)))
                 (the ,element-type (elt (the sequence document) ,?child-index)))
               ?rest
               element-iomap)))))

(def backward-mapper book/picture->syntax/leaf ()
  (reference-case -reference-
    (((the syntax/leaf document))
     '((the book/picture document)))
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (unless (string= (filename-of (content-of -printer-input-)) "")
       `((the image/file (content-of (the book/picture document)))
         (the string (filename-of (the image/file document)))
         (the string (subseq (the string document) ,?start-character-index ,?end-character-index)))))))

;;;;;;
;;; Printer

(def printer book/book->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (ll (elements-of -input-)) (lambda (element index)
                                                                   (recurse-printer -recursion- (value-of element)
                                                                                    `((elt (the sequence document) ,index)
                                                                                      (the sequence (elements-of (the book/book document)))
                                                                                      ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (make-syntax/node (as (append-ll (list-ll (ll (append (list (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                                             (as (text/make-default-text (title-of -input-) "enter book title" :selection (as (nthcdr 3 (va output-selection))) :font *font/liberation/serif/bold/42* :font-color *color/solarized/red*))))
                                                                     (when-bind author (author-of -input-)
                                                                       (list (make-syntax/leaf (as (text/make-default-text author "enter author" :selection (as (nthcdr 3 (va output-selection))) :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*))
                                                                                             :opening-delimiter (text/make-simple-text "Written by " :font *font/liberation/serif/italic/14* :font-color *color/solarized/content/darker*)
                                                                                             :indentation 0
                                                                                             :selection (as (nthcdr 2 (va output-selection))))))))
                                                         (map-ll (va element-iomaps) (lambda (element-iomap)
                                                                                       (bind ((element-output (output-of element-iomap)))
                                                                                         (etypecase element-output
                                                                                           (syntax/leaf (syntax/clone-leaf element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (syntax/node (syntax/clone-node element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (t element-output))))))))
                                 :selection output-selection)))
    (make-instance 'iomap/book/book->syntax/node
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :element-iomaps element-iomaps)))

(def printer book/chapter->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (ll (elements-of -input-)) (lambda (element index)
                                                                   (recurse-printer -recursion- (value-of element)
                                                                                    `((elt (the sequence document) ,index)
                                                                                      (the sequence (elements-of (the book/chapter document)))
                                                                                      ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (title-output (as (bind ((title (title-of -input-))
                                  (title? (string= title ""))
                                  (title-font (reference-case -input-reference-
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
                                  (numbering (numbering-of -input-)))
                             (if numbering
                                 (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                   (text/string numbering :font title-font :font-color font-color :padding (make-inset :top 10))
                                   (text/spacing 30)
                                   (text/string (if (string= title "") "enter chapter title" title) :font title-font :font-color font-color :padding (make-inset :top 10)))
                                 (text/make-default-text title "enter chapter title" :font title-font :font-color font-color :padding (make-inset :top 10) :selection (as (nthcdr 3 (va output-selection))))))))
         (output (make-syntax/node (as (append-ll (list-ll (list-ll (syntax/leaf (:selection (as (nthcdr 2 (va output-selection)))) title-output))
                                                         (map-ll (va element-iomaps) (lambda (element-iomap)
                                                                                       (bind ((element-output (output-of element-iomap)))
                                                                                         (etypecase element-output
                                                                                           (syntax/leaf (syntax/clone-leaf element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (syntax/node (syntax/clone-node element-output :indentation 0 :selection (as (nthcdr 2 (va output-selection)))))
                                                                                           (t element-output))))))))
                                 :collapsed (as (collapsed-p -input-))
                                 :selection output-selection)))
    (make-instance 'iomap/book/chapter->syntax/node
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :element-iomaps element-iomaps)))

(def printer book/paragraph->syntax/leaf ()
  (bind ((content-iomap (as (recurse-printer -recursion- -input- -input-reference-)))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (bind ((content-output (output-of (va content-iomap))))
                       (syntax/leaf (:selection output-selection)
                         (if (zerop (text/length content-output))
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string "enter paragraph text" :font *font/liberation/serif/regular/24* :font-color (color/lighten *color/solarized/content/darker* 0.75) :padding (make-inset :top 10)))
                             (text/make-text (map-ll* (ll (elements-of content-output)) (lambda (element index)
                                                                                          (declare (ignore index))
                                                                                          (bind ((value (value-of element)))
                                                                                            (if (previous-element-of element)
                                                                                                value
                                                                                                (etypecase value
                                                                                                  (text/string (text/clone-string value :padding (make-inset :top 10)))
                                                                                                  (text/graphics value))))))
                                             :selection (as (nthcdr 1 (va output-selection))))))))))
    (make-instance 'iomap/book/paragraph->syntax/leaf
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :content-iomap content-iomap)))

(def printer book/list->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (ll (elements-of -input-)) (lambda (element index)
                                                                   (recurse-printer -recursion- (value-of element)
                                                                                    `((elt (the sequence document) ,index)
                                                                                      (the sequence (elements-of (the book/list document)))
                                                                                      ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (map-ll* (va element-iomaps) (lambda (element-iomap index)
                                                                    (if (typep (input-of (value-of element-iomap)) 'book/list)
                                                                        (syntax/node (:indentation 2 :selection (as (nthcdr 2 (va output-selection))))
                                                                          (output-of (value-of element-iomap)))
                                                                        (syntax/node (:opening-delimiter (text/make-simple-text "• " :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker* :padding (make-inset :left 15))
                                                                                                       :indentation (unless (zerop index) 0)
                                                                                                       :selection (as (nthcdr 2 (va output-selection))))
                                                                          (output-of (value-of element-iomap))))))
                                     :indentation 2
                                     :selection output-selection))))
    (make-instance 'iomap/book/list->syntax/node
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :element-iomaps element-iomaps)))

(def printer book/picture->syntax/leaf ()
  (bind ((content (content-of -input-))
         (filename (filename-of content))
         (absolute-filename (if (starts-with #\/ (namestring filename))
                                filename
                                (merge-pathnames filename (hu.dwim.asdf:system-pathname :projectured.editor))))
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
                                 (print-selection -printer-iomap-))))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (if file-exists?
                             (content-of -input-)
                             (text/string filename-string :font *font/liberation/serif/regular/24* :font-color filename-color)))))))
    (make-instance 'iomap/book/picture->syntax/leaf
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output)))

;;;;;;
;;; Reader

(def reader book/book->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (title-of (the book/book document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the string (author-of (the book/book document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/node (printer-output (the book/book document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 0))
                                    . ?rest)
                                   (make-operation/string/replace-range -printer-input- '((the string (title-of (the book/book document)))
                                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))
                                  (((the syntax/node (printer-output (the book/book document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 1))
                                    . ?rest)
                                   (make-operation/string/replace-range -printer-input- '((the string (author-of (the book/book document)))
                                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-delete)
                       :domain "Book" :description "Deletes the selected element from the book"
                       :operation (reference-case (selection-of -printer-input-)
                                    (((the sequence (elements-of (the book/book document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/book document)))
                                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil)))))
                    (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Book" :description "Starts a generic insertion into the elements of the book"
                       :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                    (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/book document)))
                                                                                             (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (list (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                   (the string (subseq (the string document) 0 0)))))))))
                      ((make-key-press-gesture :scancode-a :control)
                       :domain "Book" :description "Inserts a new chapter into the elements of the book"
                       :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                    (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/book document)))
                                                                                             (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (list (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                                                                                             (the string (subseq (the string document) 0 0))))))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-delete)
                       :domain "Book" :description "Deletes the author of the book"
                       :operation (reference-case (selection-of -printer-input-)
                                    (((the string (author-of (the book/book document)))
                                      (the string document))
                                     (make-operation/replace-target -printer-iomap- (selection-of -printer-input-) nil)))))
                    (make-nothing-command -gesture-))))

(def reader book/chapter->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (title-of (the book/chapter document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/node (printer-output (the book/chapter document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input- '((the string (title-of (the book/chapter document)))
                                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-delete)
                       :domain "Book" :description "Deletes the selected element from the chapter"
                       :operation (reference-case (selection-of -printer-input-)
                                    (((the sequence (elements-of (the book/chapter document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/chapter document)))
                                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil)))))
                    (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-insert)
                       :domain "Book" :description "Starts a generic insertion into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                    (make-operation/compound (list (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/chapter document)))
                                                                                                                            (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                          (list (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                                                  (the string (subseq (the string document) 0 0)))))))
                                                                   (make-operation/replace-selection -printer-input- `((the sequence (elements-of (the book/chapter document)))
                                                                                                                       (the document/insertion (elt (the sequence document) ,elements-length))
                                                                                                                       (the string (value-of (the document/insertion document)))
                                                                                                                       (the string (subseq (the string document) 0 0))))))))
                      ((make-key-press-gesture :scancode-a :control)
                       :domain "Book" :description "Inserts a new chapter into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                    (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/chapter document)))
                                                                                             (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (list (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                                                                                             (the string (subseq (the string document) 0 0)))))))))
                      ((make-key-press-gesture :scancode-r :control)
                       :domain "Book" :description "Inserts a new paragraph into the elements of the chapter"
                       :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                    (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/chapter document)))
                                                                                             (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                           (list (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                                                                                               (the text/text (text/subseq (the text/text document) 0 0))))
                                                                                   (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0))))
                                                                                     (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader book/paragraph->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the syntax/leaf (printer-output (the book/paragraph document) ?projection ?recursion)) . ?rest)
                                   (make-operation/text/replace-range -printer-input- '((the text/text (content-of (the book/paragraph document)))
                                                                                        (the text/text (text/subseq (the text/text document) 0 0)))
                                                                      (replacement-of operation)))))))))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader book/list->syntax/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-insert)
                     :domain "Book" :description "Starts a generic insertion into the elements of the list"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                         (list (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                 (the string (subseq (the string document) 0 0)))))))))
                    ((make-key-press-gesture :scancode-r :control)
                     :domain "Book" :description "Inserts a new paragraph into the elements of the list"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the book/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                         (list (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                                                                                             (the text/text (text/subseq (the text/text document) 0 0))))
                                                                                 (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0))))
                                                                                   (text/string "" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))))
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader book/picture->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the image/file (content-of (the book/picture document)))
                                    (the string (filename-of (the image/file document)))
                                    (the string (subseq (the string document) ?start-character-index ?end-character-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/leaf (printer-output (the book/picture document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input- '((the image/file (content-of (the book/picture document)))
                                                                                          (the string (filename-of (the image/file document)))
                                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))
