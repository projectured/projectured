;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection t/sequence->syntax/node ()
  ())

(def projection t/object->syntax/node ()
  ((slot-provider :type function)))

(def projection t/null->syntax/leaf ()
  ())

(def projection t/number->syntax/leaf ()
  ())

(def projection t/string->syntax/leaf ()
  ())

(def projection t/symbol->syntax/leaf ()
  ())

(def projection t/pathname->syntax/leaf ()
  ())

;;;;;;
;;; Construction

(def function make-projection/t/sequence->syntax/node ()
  (make-projection 't/sequence->syntax/node))

(def function make-projection/t/object->syntax/node (slot-provider)
  (make-projection 't/object->syntax/node :slot-provider (or slot-provider (compose 'class-slots 'class-of))))

(def function make-projection/t/null->syntax/leaf ()
  (make-projection 't/null->syntax/leaf))

(def function make-projection/t/number->syntax/leaf ()
  (make-projection 't/number->syntax/leaf))

(def function make-projection/t/string->syntax/leaf ()
  (make-projection 't/string->syntax/leaf))

(def function make-projection/t/symbol->syntax/leaf ()
  (make-projection 't/symbol->syntax/leaf))

(def function make-projection/t/pathname->syntax/leaf ()
  (make-projection 't/pathname->syntax/leaf))

;;;;;;
;;; Construction

(def macro t/sequence->syntax/node ()
  '(make-projection/t/sequence->syntax/node))

(def macro t/object->syntax/node (&key slot-provider)
  `(make-projection/t/object->syntax/node :slot-provider ,slot-provider))

(def macro t/null->syntax/leaf ()
  '(make-projection/t/null->syntax/leaf))

(def macro t/number->syntax/leaf ()
  '(make-projection/t/number->syntax/leaf))

(def macro t/string->syntax/leaf ()
  '(make-projection/t/string->syntax/leaf))

(def macro t/symbol->syntax/leaf ()
  '(make-projection/t/symbol->syntax/leaf))

(def macro t/pathname->syntax/leaf ()
  '(make-projection/t/pathname->syntax/leaf))

;;;;;;
;;; Forward mapper

(def forward-mapper t/sequence->syntax/node ()
  (reference-case -reference-
    (((the sequence document))
     `((the syntax/node document)))
    (((the string (elt (the sequence document) ?index))
      (the string (subseq (the string document) ?start-index ?end-index)))
     `((the sequence (children-of (the syntax/node document)))
       (the syntax/node (elt (the sequence document) ,(1+ ?index)))
       (the sequence (children-of (the syntax/node document)))
       (the syntax/leaf (elt (the sequence document) 1))
       (the text/text (content-of (the syntax/leaf document)))
       (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))
    (((the ?element-type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (children-of (the syntax/node document)))
                 (the syntax/node (elt (the sequence document) ,(1+ ?index)))
                 (the sequence (children-of (the syntax/node document)))
                 (the ,(document-type (output-of element-iomap)) (elt (the sequence document) 1)))
               ?rest
               element-iomap)))))

(def forward-mapper t/object->syntax/node ()
  (bind ((class (class-of -printer-input-))
         (slots (funcall (slot-provider-of -projection-) -printer-input-))
         (slot-readers (mapcar (curry 'find-slot-reader class) slots)))
    (reference-case -reference-
      (((the ?type document))
       `((the syntax/node document)))
      (((the string (?slot-reader (the ?input-type document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (bind ((index (position ?slot-reader slot-readers)))
         (when index
           `((the sequence (children-of (the syntax/node document)))
             (the syntax/node (elt (the sequence document) ,(1+ index)))
             (the sequence (children-of (the syntax/node document)))
             (the syntax/leaf (elt (the sequence document) 1))
             (the text/text (content-of (the syntax/leaf document)))
             (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))
      (((the number (?slot-reader (the ?input-type document)))
        (the string (write-to-string (the number document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (bind ((index (position ?slot-reader slot-readers)))
         (when index
           `((the sequence (children-of (the syntax/node document)))
             (the syntax/node (elt (the sequence document) ,(1+ index)))
             (the sequence (children-of (the syntax/node document)))
             (the syntax/leaf (elt (the sequence document) 1))
             (the text/text (content-of (the syntax/leaf document)))
             (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))
      (((the ?slot-value-type (?slot-reader (the ?input-type document)))
        . ?rest)
       (bind ((index (position ?slot-reader slot-readers))
              (slot-iomap (elt (child-iomaps-of -printer-iomap-) index)))
         (values `((the sequence (children-of (the syntax/node document)))
                   (the syntax/node (elt (the sequence document) ,(1+ index)))
                   (the sequence (children-of (the syntax/node document)))
                   (the ,(document-type (output-of slot-iomap)) (elt (the sequence document) 1)))
                 ?rest
                 slot-iomap))))))

;;;;;;
;;; Forward mapper

(def backward-mapper t/sequence->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     `((the sequence document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) ?index))
      (the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 1))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     `((the string (elt (the sequence document) ,(1- ?index)))
       (the string (subseq (the string document) ,?start-index ,?end-index))))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) ?index))
      (the sequence (children-of (the syntax/node document)))
      (the ?type (elt (the sequence document) 1))
      . ?rest)
     (bind ((index (1- ?index))
            (element-iomap (elt (child-iomaps-of -printer-iomap-) index)))
       (values `((the ,(document-type (input-of element-iomap)) (elt (the sequence document) ,index)))
               ?rest
               element-iomap)))))

(def backward-mapper t/object->syntax/node ()
  (reference-case -reference-
    (((the syntax/node document))
     `((the ,(document-type -printer-input-) document)))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) ?child-index))
      (the sequence (children-of (the syntax/node document)))
      (the syntax/leaf (elt (the sequence document) 1))
      (the text/text (content-of (the syntax/leaf document)))
      (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (when (> ?child-index 0)
       (bind ((slots (funcall (slot-provider-of -projection-) -printer-input-))
              (slot-index (- ?child-index 1))
              (slot-reader (find-slot-reader (class-of -printer-input-) (elt slots slot-index)))
              (slot-value (input-of (elt (child-iomaps-of -printer-iomap-) slot-index))))
         (typecase slot-value
           (string
            (unless (string= slot-value "")
              `((the string (,slot-reader (the ,(document-type -printer-input-) document)))
                (the string (subseq (the string document) ,?start-index ,?end-index)))))
           (number
            `((the number (,slot-reader (the ,(document-type -printer-input-) document)))
              (the string (write-to-string (the number document)))
              (the string (subseq (the string document) ,?start-index ,?end-index))))))))
    (((the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) ?index))
      (the sequence (children-of (the syntax/node document)))
      (the syntax/node (elt (the sequence document) 1))
      . ?rest)
     (bind ((index (1- ?index))
            (slots (funcall (slot-provider-of -projection-) -printer-input-))
            (slot (elt slots index))
            (slot-reader (find-slot-reader (class-of -printer-input-) slot))
            (slot-iomap (elt (child-iomaps-of -printer-iomap-) index))
            (slot-value (input-of slot-iomap)))
       (values `((the ,(document-type slot-value) (,slot-reader (the ,(document-type -printer-input-) document))))
               ?rest
               slot-iomap)))))

;;;;;;
;;; Printer

(def printer t/sequence->syntax/node ()
  (bind ((element-iomaps (as (map-ll* (ll -input-) (lambda (element index)
                                                     (recurse-printer -recursion- (value-of element)
                                                                      `((elt (the ,(document-type -input-) document) ,index)
                                                                        ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (when (typep -input- 'document)
                                 (print-selection -printer-iomap-))))
         (output (as (if (emptyp -input-)
                         (syntax/leaf (:selection output-selection)
                           (text/text (:selection (as (nthcdr 1 (va output-selection))))
                             (text/string "")))
                         (make-syntax/node (append-ll (list-ll (list-ll (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                                                        (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                                          (text/string "SEQUENCE" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*))))
                                                             (map-ll* (va element-iomaps) (lambda (element-iomap index)
                                                                                            (bind ((element-iomap-output (output-of (value-of element-iomap))))
                                                                                              (syntax/node (:selection (as (nthcdr 2 (va output-selection))) :indentation 1 :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24*)))
                                                                                                (syntax/leaf (:selection (as (nthcdr 4 (va output-selection))))
                                                                                                  (text/text (:selection (as (nthcdr 5 (va output-selection))))
                                                                                                    (text/string (write-to-string index) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)))
                                                                                                (syntax/indentation (:indentation 0 :selection (as (nthcdr 4 (va output-selection))))
                                                                                                  element-iomap-output)))))))
                                         :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/24*))
                                         :selection output-selection)))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output element-iomaps)))

(def printer t/object->syntax/node ()
  (bind ((class (class-of -input-))
         (slots (funcall (slot-provider-of -projection-) -input-))
         (slot-readers (mapcar (curry 'find-slot-reader class) slots))
         (slot-iomaps (as (iter (for slot :in slots)
                                (for slot-reader :in slot-readers)
                                (for slot-reference = `((,slot-reader (the ,(document-type -input-) document))
                                                        ,@(typed-reference (document-type -input-) -input-reference-)))
                                (collect (if (slot-boundp-using-class class -input- slot)
                                             (recurse-printer -recursion- (slot-value-using-class class -input- slot) slot-reference)
                                             (make-iomap -recursion- -recursion- nil slot-reference (syntax/leaf ()
                                                                                                      (text/make-simple-text "<unbound>" :font *font/ubuntu/monospace/italic/24* :font-color *color/solarized/gray*))))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (as (make-syntax/node (list* (syntax/leaf (:selection (as (nthcdr 2 (va output-selection))))
                                              (text/text (:selection (as (nthcdr 3 (va output-selection))))
                                                (text/string (symbol-name (class-name (class-of -input-))) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/red*)))
                                            (iter (for slot :in slots)
                                                  (for slot-iomap :in (va slot-iomaps))
                                                  (for slot-iomap-output = (output-of slot-iomap))
                                                  (collect (syntax/node (:selection (as (nthcdr 2 (va output-selection))) :separator (text/make-simple-text " " :font *font/ubuntu/monospace/regular/24*) :indentation 1)
                                                             (syntax/leaf (:selection (as (nthcdr 4 (va output-selection))))
                                                               (text/text (:selection (as (nthcdr 5 (va output-selection))))
                                                                 (text/string (symbol-name (slot-definition-name slot)) :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/cyan*)))
                                                             (syntax/indentation (:indentation 1 :selection (as (nthcdr 4 (va output-selection))))
                                                               slot-iomap-output)))))
                                     :collapsed (when (find-slot (class-of -input-) 'collapsed :otherwise nil)
                                                  (as (collapsed-p -input-)))
                                     :selection output-selection))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output slot-iomaps)))

(def printer t/null->syntax/leaf ()
  (bind ((output (syntax/leaf ()
                   (text/make-simple-text "NIL" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer t/number->syntax/leaf ()
  (bind ((output (syntax/leaf ()
                   (text/make-default-text (write-to-string -input-) "enter number" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/magenta*))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer t/string->syntax/leaf ()
  (bind ((output (syntax/leaf (:opening-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                             :closing-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                   (text/make-default-text -input- "enter string" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output nil)))

(def printer t/symbol->syntax/leaf ()
  (bind ((output (syntax/leaf ()
                   (text/make-default-text (write-to-string -input-) "enter symbol" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/blue*))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer t/pathname->syntax/leaf ()
  (bind ((output (syntax/leaf (:opening-delimiter (text/make-simple-text "#P\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*)
                             :closing-delimiter (text/make-simple-text "\"" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/gray*))
                   (text/make-default-text (princ-to-string -input-) "enter path" :font *font/ubuntu/monospace/regular/24* :font-color *color/solarized/green*))))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader t/sequence->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (elt (the sequence document) ?index))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range selection (replacement-of operation)))))))))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader t/object->syntax/node ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the string (?reader (the ?type document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/string/replace-range selection (replacement-of operation)))
                                  (((the number (?reader (the ?type document)))
                                    (the string (write-to-string (the number document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (when (every 'digit-char-p (replacement-of operation))
                                     (make-operation/number/replace-range selection (replacement-of operation))))
                                  (((the syntax/node (printer-output (the ?type document) ?projection ?recursion))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/node (elt (the sequence document) ?child-index))
                                    (the sequence (children-of (the syntax/node document)))
                                    (the syntax/leaf (elt (the sequence document) 1))
                                    (the text/text (content-of (the syntax/leaf document)))
                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                   (when (> ?child-index 0)
                                     (bind ((slots (funcall (slot-provider-of -projection-) -printer-input-))
                                            (slot-index (- ?child-index 1))
                                            (slot-reader (find-slot-reader (class-of -printer-input-) (elt slots slot-index))))
                                       (make-operation/string/replace-range `((the string (,slot-reader (the ,?type document)))
                                                                              (the string (subseq (the string document) 0 0)))
                                                                            (replacement-of operation)))))))))))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader t/null->syntax/leaf ()
  -input-)

(def reader t/number->syntax/leaf ()
  -input-)

(def reader t/string->syntax/leaf ()
  -input-)

(def reader t/symbol->syntax/leaf ()
  -input-)

(def reader t/pathname->syntax/leaf ()
  -input-)
