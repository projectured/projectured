;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection syntax/delimitation->text/text ()
  ((output-delimiters :type boolean)))

(def projection syntax/indentation->text/text ()
  ())

(def projection syntax/collapsible->text/text ()
  ())

(def projection syntax/leaf->text/text ()
  ((output-delimiters :type boolean)))

(def projection syntax/separation->text/text ()
  ())

(def projection syntax/node->text/text ()
  ((output-delimiters :type boolean)))

;;;;;;
;;; Construction

(def function make-projection/syntax/delimitation->text/text ()
  (make-projection 'syntax/delimitation->text/text :output-delimiters #t))

(def function make-projection/syntax/indentation->text/text ()
  (make-projection 'syntax/indentation->text/text))

(def function make-projection/syntax/collapsible->text/text ()
  (make-projection 'syntax/collapsible->text/text))

(def function make-projection/syntax/leaf->text/text ()
  (make-projection 'syntax/leaf->text/text :output-delimiters #t))

(def function make-projection/syntax/separation->text/text ()
  (make-projection 'syntax/separation->text/text))

(def function make-projection/syntax/node->text/text ()
  (make-projection 'syntax/node->text/text :output-delimiters #t))

;;;;;;
;;; Construction

(def macro syntax/delimitation->text/text ()
  `(make-projection/syntax/delimitation->text/text))

(def macro syntax/indentation->text/text ()
  `(make-projection/syntax/indentation->text/text))

(def macro syntax/collapsible->text/text ()
  `(make-projection/syntax/collapsible->text/text))

(def macro syntax/leaf->text/text ()
  `(make-projection/syntax/leaf->text/text))

(def macro syntax/separation->text/text ()
  `(make-projection/syntax/separation->text/text))

(def macro syntax/node->text/text ()
  `(make-projection/syntax/node->text/text))

;;;;;;
;;; Forward mapper

(def forward-mapper syntax/delimitation->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/delimitation document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper syntax/indentation->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/indentation document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper syntax/collapsible->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/collapsible document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper syntax/leaf->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/leaf document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper syntax/separation->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/separation document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

(def forward-mapper syntax/node->text/text ()
  (pattern-case -reference-
    (((the text/text (printer-output (the syntax/node document) ?projection ?recursion)) . ?rest)
     (when (eq -projection- ?projection)
       ?rest))))

;;;;;;
;;; Backward mapper

(def backward-mapper syntax/delimitation->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/delimitation document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper syntax/indentation->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/indentation document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper syntax/collapsible->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/collapsible document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper syntax/leaf->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/leaf document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper syntax/separation->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/separation document) ,-projection- ,-recursion-))) -reference-))))

(def backward-mapper syntax/node->text/text ()
  (pattern-case -reference-
    (?
     (append `((the text/text (printer-output (the syntax/node document) ,-projection- ,-recursion-))) -reference-))))

;;;;;;
;;; Printer

(def printer syntax/delimitation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/delimitation->text/text)))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p -projection-))
                                     (opening-delimiter (opening-delimiter-of -input-))
                                     (closing-delimiter (closing-delimiter-of -input-)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (elements-of (output-of (va content-iomap)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/indentation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/indentation->text/text)))
         (output-elements (as ;; TODO: indent all lines
                              (elements-of (output-of (va content-iomap)))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/collapsible->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/collapsible->text/text)))
         (output-elements (as (concatenate-ll 0
                                              ;; TODO: get first line
                                              (elements-of (output-of (va content-iomap)))
                                              (when (collapsed-p -input-)
                                                (list-ll (text/string " …"
                                                                      :font *font/default*
                                                                      :font-color *color/solarized/gray*))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/leaf->text/text ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/leaf->text/text)))
         (output-elements nil)
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/separation->text/text ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/separation->text/text)))
         (output-elements nil)
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/node->text/text ()
  (bind ((output-selection (as (print-selection (make-iomap -projection- -recursion- -input- -input-reference- nil)
                                                (get-selection -input-)
                                                'forward-mapper/syntax/node->text/text)))
         (output-elements nil)
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

;;;;;;
;;; Reader

(def reader syntax/delimitation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/indentation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/collapsible->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/leaf->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/separation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/node->text/text ()
  (merge-commands (make-nothing-command -gesture-)))
