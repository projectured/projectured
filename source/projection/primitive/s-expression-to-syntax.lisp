;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection s-expression/insertion->syntax/leaf ()
  ())

(def projection s-expression/comment->syntax/node ()
  ())

(def projection s-expression/number->syntax/leaf ()
  ())

(def projection s-expression/symbol->syntax/leaf ()
  ((fully-qualified :type boolean)))

(def projection s-expression/string->syntax/leaf ()
  ())

(def projection s-expression/quote->syntax/node ()
  ())

(def projection s-expression/list->syntax/node ()
  ())

(def projection s-expression/object->syntax/leaf ()
  ())

(def projection s-expression/toplevel->syntax/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/s-expression/insertion->syntax/leaf ()
  (make-projection 's-expression/insertion->syntax/leaf))

(def function make-projection/s-expression/comment->syntax/node ()
  (make-projection 's-expression/comment->syntax/node))

(def function make-projection/s-expression/number->syntax/leaf ()
  (make-projection 's-expression/number->syntax/leaf))

(def function make-projection/s-expression/symbol->syntax/leaf ()
  (make-projection 's-expression/symbol->syntax/leaf :fully-qualified #f))

(def function make-projection/s-expression/string->syntax/leaf ()
  (make-projection 's-expression/string->syntax/leaf))

(def function make-projection/s-expression/quote->syntax/node ()
  (make-projection 's-expression/quote->syntax/node))

(def function make-projection/s-expression/list->syntax/node ()
  (make-projection 's-expression/list->syntax/node))

(def function make-projection/s-expression/object->syntax/leaf ()
  (make-projection 's-expression/object->syntax/leaf))

(def function make-projection/s-expression/toplevel->syntax/node ()
  (make-projection 's-expression/toplevel->syntax/node))

;;;;;;
;;; Construction

(def macro s-expression/insertion->syntax/leaf ()
  '(make-projection/s-expression/insertion->syntax/leaf))

(def macro s-expression/comment->syntax/node ()
  '(make-projection/s-expression/comment->syntax/node))

(def macro s-expression/number->syntax/leaf ()
  '(make-projection/s-expression/number->syntax/leaf))

(def macro s-expression/symbol->syntax/leaf ()
  '(make-projection/s-expression/symbol->syntax/leaf))

(def macro s-expression/string->syntax/leaf ()
  '(make-projection/s-expression/string->syntax/leaf))

(def macro s-expression/quote->syntax/node ()
  '(make-projection/s-expression/quote->syntax/node))

(def macro s-expression/list->syntax/node ()
  '(make-projection/s-expression/list->syntax/node))

(def macro s-expression/object->syntax/leaf ()
  '(make-projection/s-expression/object->syntax/leaf))

(def macro s-expression/toplevel->syntax/node ()
  '(make-projection/s-expression/toplevel->syntax/node))

;;;;;;
;;; Forward mapper

(def forward-mapper s-expression/insertion->syntax/leaf ()
  (reference-case -reference-
    (((the string (value-of (the s-expression/insertion document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper s-expression/comment->syntax/node ()
  (reference-case -reference-
    (((the text/text (content-of (the s-expression/comment document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 0))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper s-expression/number->syntax/leaf ()
  (reference-case -reference-
    (((the ?type (value-of (the s-expression/number document)))
      (the string (write-to-string (the ?type document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper s-expression/symbol->syntax/leaf ()
  (reference-case -reference-
    (((the string (name-of (the s-expression/symbol document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     (bind ((offset (if (fully-qualified-p -projection-) (+ 2 (length (package-of -printer-input-))) 0)))
       `((the text/text (content-of (the syntax/leaf document)))
         (the text/text (text/subseq (the text/text document) ,(+ ?start-index offset) ,(+ ?end-index offset))))))))

(def forward-mapper s-expression/string->syntax/leaf ()
  (reference-case -reference-
    (((the string (value-of (the s-expression/string document)))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def forward-mapper s-expression/quote->syntax/node ()
  (reference-case -reference-
    (((the ?type (value-of (the s-expression/quote document)))
      . ?rest)
     (values `((the sequence (children-of (the syntax/node document)))
               (the ,(document-type (output-of (content-iomap-of -printer-iomap-))) (elt (the sequence document) 0)))
             ?rest
             (content-iomap-of -printer-iomap-)))))

(def forward-mapper s-expression/list->syntax/node ()
  (reference-case -reference-
    (((the s-expression/list document))
     `((the syntax/node document)))
    (((the sequence (elements-of (the s-expression/list document)))
      (the ?element-type (elt (the sequence document) ?element-index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?element-index))
            (element-output (output-of element-iomap)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type element-output) (elt (the sequence document) ,?element-index)))
               ?rest
               element-iomap)))))

(def forward-mapper s-expression/object->syntax/leaf ()
  nil)

(def forward-mapper s-expression/toplevel->syntax/node ()
  (reference-case -reference-
    (((the s-expression/toplevel document))
     `((the syntax/node document)))
    (((the sequence (elements-of (the s-expression/toplevel document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of child-iomap)) (elt (the sequence document) ,?child-index)))
               ?rest
               child-iomap)))))

;;;;;;
;;; Backward mapper

(def backward-mapper s-expression/insertion->syntax/leaf ()
  (reference-case -reference-
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the string (value-of (the s-expression/insertion document)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))))

(def backward-mapper s-expression/comment->syntax/node ()
  (reference-case -reference-
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the text/text (content-of (the s-expression/comment document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))

(def backward-mapper s-expression/number->syntax/leaf ()
  (reference-case -reference-
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (unless (string= "" (write-number (value-of -printer-input-) nil))
       (bind ((type (document-type (value-of -printer-input-))))
         `((the ,type (value-of (the s-expression/number document)))
           (the string (write-to-string (the ,type document)))
           (the string (subseq (the string document) ,?start-index ,?end-index))))))))

(def backward-mapper s-expression/symbol->syntax/leaf ()
  (reference-case -reference-
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (unless (string= (name-of -printer-input-) "")
       (bind ((offset (if (fully-qualified-p -projection-) (+ 2 (length (package-of -printer-input-))) 0)))
         `((the string (name-of (the s-expression/symbol document)))
           (the string (subseq (the string document) ,(- ?start-index offset) ,(- ?end-index offset)))))))))

(def backward-mapper s-expression/string->syntax/leaf ()
  (reference-case -reference-
    (((the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (unless (string= (value-of -printer-input-) "")
       `((the string (value-of (the s-expression/string document)))
         (the string (subseq (the string document) ,?start-index ,?end-index)))))))

(def backward-mapper s-expression/quote->syntax/node ()
  (reference-case -reference-
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 0))
      . ?rest)
     (values `((the ,(document-type (input-of (content-iomap-of -printer-iomap-))) (value-of (the s-expression/quote document))))
             ?rest
             (content-iomap-of -printer-iomap-)))))

(def backward-mapper s-expression/list->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     `((the s-expression/list document)))
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((element (elt (elements-of -printer-input-) ?child-index))
            (element-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (elements-of (the s-expression/list document)))
                 (the ,(document-type element) (elt (the sequence document) ,?child-index)))
               ?rest
               element-iomap)))))

(def backward-mapper s-expression/object->syntax/leaf ()
  nil)

(def backward-mapper s-expression/toplevel->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     `((the s-expression/toplevel document)))
    (((the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((element (elt (elements-of -printer-input-) ?child-index))
            (element-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values `((the sequence (elements-of (the s-expression/toplevel document)))
                 (the ,(document-type element) (elt (the sequence document) ,?child-index)))
               ?rest
               element-iomap)))))

;;;;;;
;;; Printer

(def printer s-expression/insertion->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output (as (bind (((:values nil completion) (funcall (factory-of -input-) (factory-of -input-) -input- nil (value-of -input-)))
                            (commitable? (not (null (funcall (factory-of -input-) (factory-of -input-) -input- nil (string+ (value-of -input-) completion)))))
                            (value-color (if commitable? *color/solarized/green* *color/solarized/red*)))
                       (syntax/leaf (:selection output-selection :indentation (indentation-of -input-)
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

(def printer s-expression/comment->syntax/node ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-) -input-reference-)))
         (output-selection (as (print-selection -printer-iomap-)))
         ;; TODO:
         (output (as (make-syntax/node (list (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                               (output-of (va content-iomap))))
                                       :indentation (indentation-of -input-)
                                       :opening-delimiter (text/text () (text/string ";; " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :selection output-selection))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/number->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output (as (syntax/leaf (:selection output-selection :indentation (indentation-of -input-))
                       (text/make-default-text (write-number (value-of -input-) nil) "enter lisp number" :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/symbol->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output (as (bind ((font-color (or (font-color-of -input-) *color/solarized/violet*))
                            (name (name-of -input-))
                            (name-string (string-downcase (if (string= "KEYWORD" (package-of -input-)) (string+ ":" name) name))))
                       (syntax/leaf (:selection output-selection :indentation (indentation-of -input-))
                         (if (fully-qualified-p -projection-)
                             (text/text (:selection (as (nthcdr 1 (va output-selection))))
                               (text/string (string+ (string-downcase (package-of -input-)) "::" ) :font (or (font-of -input-) *font/ubuntu/monospace/regular/24*) :font-color (color/lighten font-color 0.5))
                               (text/string name-string :font (or (font-of -input-) *font/ubuntu/monospace/bold/24*) :font-color font-color))
                             (text/make-default-text name-string (or (default-value-of -input-) "enter lisp symbol") :selection (as (nthcdr 1 (va output-selection))) :font (or (font-of -input-) *font/ubuntu/monospace/regular/24*) :font-color font-color)))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/string->syntax/leaf ()
  (bind ((value (write-to-string (value-of -input-)))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (syntax/leaf (:selection output-selection :indentation (indentation-of -input-)
                                   :opening-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                   :closing-delimiter (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                       (text/make-default-text (subseq value 1 (1- (length value))) (or (default-value-of -input-) "enter lisp string") :selection (as (nthcdr 1 (va output-selection))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*)))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/quote->syntax/node ()
  (bind ((value-iomap (as (recurse-printer -recursion- (value-of -input-)
                                           `((value-of (the sequence document))
                                             ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (syntax/node (:selection output-selection :indentation (indentation-of -input-)
                                   :opening-delimiter (text/text () (text/string "'" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                       (output-of (va value-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output value-iomap)))

(def printer s-expression/list->syntax/node ()
  (bind ((deep-list (find-if (of-type 's-expression/list) (elements-of -input-)))
         (element-iomaps (as (iter (for index :from 0)
                                   (for element :in-sequence (elements-of -input-))
                                   (collect (recurse-printer -recursion- element
                                                             `((elt (the sequence document) ,index)
                                                               (the sequence (elements-of (the document list-form/list)))
                                                               ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (bind ((separator-font-color *color/solarized/gray*)
                            (separator-fill-color nil))
                       (make-syntax/node (as (map-ll (ll (va element-iomaps))
                                                     (lambda (element-iomap)
                                                       (bind ((element-output (output-of element-iomap)))
                                                         (typecase element-output
                                                           (syntax/base (syntax/clone element-output :indentation (indentation-of element-output)))
                                                           (s-expression/base (s-expression/clone element-output :indentation (indentation-of element-output)))
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
                       (make-syntax/node (mapcar 'output-of (va element-iomaps))
                                         :selection output-selection
                                         :opening-delimiter (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                         :closing-delimiter (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/24* :font-color separator-font-color :fill-color separator-fill-color))
                                         :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer s-expression/object->syntax/leaf ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output (as (syntax/leaf (:selection output-selection)
                       (text/text (:selection (as (nthcdr 1 (va output-selection))))
                         (text/string (write-to-string -input-) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer s-expression/toplevel->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (elements-of -input-) (lambda (element index)
                                                              (recurse-printer -recursion- (value-of element)
                                                                               `((elt (the sequence document) ,index)
                                                                                 (the sequence (elements-of (the s-expression/toplevel document)))
                                                                                 ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (map-ll (va element-iomaps) 'output-of)
                                       :indentation (indentation-of -input-)
                                       :collapsed (as (collapsed-p -input-))
                                       :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                                       :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

;;;;;;
;;; Reader

(def reader s-expression/insertion->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (value-of (the s-expression/insertion document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader s-expression/comment->syntax/node ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader s-expression/number->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the ?type (value-of (the s-expression/number document)))
                                    (the string (write-to-string (the ?type document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range -printer-input- selection (replacement-of operation))))
                                  (((the syntax/leaf (printer-output (the s-expression/number document) ?projection ?recursion)) . ?rest)
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (bind ((type (etypecase (value-of -printer-input-)
                                                    ((or null number) 'number)
                                                    (primitive/number 'primitive/number))))
                                       (make-operation/number/replace-range -printer-input-
                                                                            `((the ,type (value-of (the s-expression/number document)))
                                                                              (the string (write-to-string (the ,type document)))
                                                                              (the string (subseq (the string document) 0 0)))
                                                                            (replacement-of operation)))))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader s-expression/symbol->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (name-of (the s-expression/symbol document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/leaf (printer-output (the s-expression/symbol document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (name-of (the s-expression/symbol document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-q :control)
                       :domain "Lisp form" :description "Toggles displaying fully qualified symbol names."
                       :operation (make-operation/functional (lambda () (notf (fully-qualified-p -projection-))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader s-expression/string->syntax/leaf ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (value-of (the s-expression/string document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range -printer-input- selection (replacement-of operation)))
                                  (((the syntax/leaf (printer-output (the s-expression/string document) ?projection ?recursion)) . ?rest)
                                   (make-operation/string/replace-range -printer-input-
                                                                        '((the string (value-of (the s-expression/string document)))
                                                                          (the string (subseq (the string document) 0 0)))
                                                                        (replacement-of operation)))))))))
    (merge-commands (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader s-expression/quote->syntax/node ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader s-expression/list->syntax/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-insert)
                     :domain "Lisp form" :description "Starts an insertion into the elements of the list"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/sequence/replace-range -printer-input- `((the sequence (elements-of (the s-expression/list document)))
                                                                                           (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                         (make-collection/sequence (vector (as (document/insertion (:selection '((the string (value-of (the document/insertion document)))
                                                                                                                                               (the string (subseq (the string document) 0 0))))))))))))
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader s-expression/object->syntax/leaf ()
  (merge-commands (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader s-expression/toplevel->syntax/node ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))
