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
;;; IO map

(def iomap iomap/syntax/leaf->text/text ()
  ((content-iomap :type iomap)))

(def iomap iomap/syntax/node->text/text ()
  ((child-iomaps :type sequence)))

(def iomap iomap/syntax/node->text/text/child ()
  ((content-iomap :type iomap)
   (indented-child :type text/text)
   (line-indentation :type integer)
   (origin-character-index :type integer)
   (first-character-index :type integer)
   (last-character-index :type integer)))

;;;;;;
;;; Mapping

(def function find-child-character-index (iomap parent-character-index)
  (iter (for index :from 0)
        (for child-iomap-ll :initially (child-iomaps-of iomap) :then (next-element-of child-iomap-ll))
        (while child-iomap-ll)
        (for child-iomap = (value-of child-iomap-ll))
        (for first-character-index = (first-character-index-of child-iomap))
        (for origin-character-index = (origin-character-index-of child-iomap))
        (for indented-child = (indented-child-of child-iomap))
        ;; TODO: kills laziness (for last-character-index = (last-character-index-of child-iomap))
        (when (and (<= first-character-index parent-character-index) ;; TODO: used to be this, it but kills laziness (<= first-character-index parent-character-index last-character-index)
                   (text/origin-relative-position indented-child (- parent-character-index origin-character-index)))
          (bind ((line-indentation (line-indentation-of child-iomap)) (indented-character-index (- parent-character-index origin-character-index))
                 (indented-origin-position (text/origin-position indented-child))
                 (indented-character-position (text/relative-position indented-child indented-origin-position indented-character-index))
                 (indented-line-start-position (text/line-start-position indented-child indented-character-position))
                 (indented-line-character-index (text/length indented-child indented-line-start-position indented-character-position))
                 (output (output-of (content-iomap-of child-iomap))))
            (when (or (text/first-position? indented-child indented-line-start-position)
                      (<= line-indentation indented-line-character-index))
              (bind ((indented-line-count (if (< indented-character-index 0)
                                              (text/count indented-child #\NewLine indented-character-position indented-origin-position)
                                              (text/count indented-child #\NewLine indented-origin-position indented-character-position)))
                     (origin-relative-character-index (- indented-character-index (* indented-line-count line-indentation))))
                (when (text/relative-position output (text/origin-position output) origin-relative-character-index)
                  (return (values index origin-relative-character-index)))))))))

(def function find-parent-character-index (child-iomap origin-relative-character-index)
  (bind ((line-indentation (line-indentation-of child-iomap))
         (output (output-of (content-iomap-of child-iomap)))
         (origin-character-index (origin-character-index-of child-iomap))
         (origin-position (text/origin-position output))
         (character-position (text/relative-position output origin-position origin-relative-character-index))
         (line-count (if (< origin-relative-character-index 0)
                         (text/count output #\NewLine character-position origin-position)
                         (text/count output #\NewLine origin-position character-position)))
         (indented-character-index (+ origin-relative-character-index (* line-count line-indentation))))
    (+ origin-character-index indented-character-index)))

;;;;;;
;;; Forward mapper

(def forward-mapper syntax/delimitation->text/text ()
  nil)

(def forward-mapper syntax/indentation->text/text ()
  nil)

(def forward-mapper syntax/collapsible->text/text ()
  nil)

(def forward-mapper syntax/leaf->text/text ()
  (bind ((content-iomap (content-iomap-of -printer-iomap-)))
    (reference-case -reference-
      (((the syntax/leaf document))
       (bind ((start-index (aif (opening-delimiter-of -printer-input-) (- (text/length it)) 0))
              (end-index (+ (text/length (output-of content-iomap)) (aif (closing-delimiter-of -printer-input-) (text/length it) 0))))
         `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
      (((the ?content-type (content-of (the syntax/leaf document)))
        . ?rest)
       (values nil nil content-iomap))
      (((the text/text (opening-delimiter-of (the syntax/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((content-output (output-of content-iomap))
              (opening-delimiter (opening-delimiter-of -printer-input-))
              (start-character-index (- (+ (text/length opening-delimiter
                                                        (text/relative-position opening-delimiter (text/origin-position opening-delimiter) ?start-character-index)
                                                        (text/last-position opening-delimiter))
                                           (text/length content-output (text/first-position content-output) (text/origin-position content-output)))))
              (end-character-index (- (+ (text/length opening-delimiter
                                                      (text/relative-position opening-delimiter (text/origin-position opening-delimiter) ?end-character-index)
                                                      (text/last-position opening-delimiter))
                                         (text/length content-output (text/first-position content-output) (text/origin-position content-output))))))
         `((the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index)))))
      (((the text/text (closing-delimiter-of (the syntax/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
       (bind ((content-output (output-of content-iomap))
              (closing-delimiter (closing-delimiter-of -printer-input-))
              (start-character-index (+ (text/length closing-delimiter
                                                     (text/first-position closing-delimiter)
                                                     (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?start-character-index))
                                        (text/length content-output (text/origin-position content-output) (text/last-position content-output))))
              (end-character-index (+ (text/length closing-delimiter
                                                   (text/first-position closing-delimiter)
                                                   (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?end-character-index))
                                      (text/length content-output (text/origin-position content-output) (text/last-position content-output)))))
         `((the text/text (text/subseq (the text/text document) ,start-character-index ,end-character-index))))))))

(def forward-mapper syntax/separation->text/text ()
  nil)

(def forward-mapper syntax/node->text/text ()
  nil)

;;;;;;
;;; Backward mapper

(def backward-mapper syntax/delimitation->text/text ()
  nil)

(def backward-mapper syntax/indentation->text/text ()
  nil)

(def backward-mapper syntax/collapsible->text/text ()
  nil)

(def backward-mapper syntax/leaf->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
     (bind ((content-iomap (content-iomap-of -printer-iomap-))
            (content-output (output-of content-iomap))
            (content-output-origin-position (text/origin-position content-output))
            (start-position (text/relative-position content-output content-output-origin-position ?start-character-index))
            (end-position (text/relative-position content-output content-output-origin-position ?end-character-index)))
       (if (and start-position end-position)
           (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/leaf document))))
                   -reference-
                   content-iomap)
           (if (and (< ?start-character-index 0) (< ?end-character-index 0))
               (bind ((offset (+ (text/length (opening-delimiter-of -printer-input-))
                                 (text/length content-output (text/first-position content-output) content-output-origin-position))))
                 `((the text/text (opening-delimiter-of (the syntax/leaf document)))
                   (the text/text (text/subseq (the text/text document) ,(+ offset ?start-character-index) ,(+ offset ?end-character-index)))))
               (bind ((offset (- (text/length content-output content-output-origin-position (text/last-position content-output)))))
                 `((the text/text (closing-delimiter-of (the syntax/leaf document)))
                   (the text/text (text/subseq (the text/text document) ,(+ offset ?start-character-index) ,(+ offset ?end-character-index)))))))))))

(def backward-mapper syntax/separation->text/text ()
  nil)

(def backward-mapper syntax/node->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
     (bind (((:values child-start-index child-start-character-index) (find-child-character-index -printer-iomap- ?parent-start-character-index))
            ((:values child-end-index child-end-character-index) (find-child-character-index -printer-iomap- ?parent-end-character-index)))
       (if (and child-start-index child-end-index (= child-start-index child-end-index))
           (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of -printer-iomap-) child-start-index)))
                  (child (input-of child-iomap)))
             (values `((the sequence (children-of (the syntax/node document)))
                       (the ,(document-type child) (elt (the sequence document) ,child-start-index)))
                     `((the text/text (text/subseq (the text/text document) ,child-start-character-index ,child-end-character-index)))
                     child-iomap))
           (when (and (< ?parent-start-character-index 0) (< ?parent-end-character-index 0))
             (bind ((text (output-of -printer-iomap-))
                    (offset (text/length text (text/first-position text) (text/origin-position text))))
               `((the text/text (opening-delimiter-of (the syntax/node document)))
                 (the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index)))))
             #+nil
             (bind ((text (output-of printer-iomap))
                    (offset (text/length text (text/origin-position text) (text/last-position text))))
               `((the text/text (closing-delimiter-of (the syntax/node document)))
                 (the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index)))))))))))

;;;;;;
;;; Printer

(def printer syntax/delimitation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
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
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as ;; TODO: indent all lines
                            (elements-of (output-of (va content-iomap)))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/collapsible->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
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
  (bind ((content-reference `(((content-of (the ,(document-type -input-) document))) ,@(typed-reference (document-type -input-) -input-reference-)))
         (content-iomap (as (recurse-printer -recursion- (content-of -input-) content-reference)))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p -projection-))
                                     (opening-delimiter (opening-delimiter-of -input-))
                                     (closing-delimiter (closing-delimiter-of -input-)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (elements-of (output-of (va content-iomap)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-instance 'iomap/syntax/leaf->text/text
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :content-iomap content-iomap)))

(def printer syntax/separation->text/text ()
  (bind ((output-selection (as (print-selection -printer-iomap-)))
         (output-elements nil)
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap -projection- -recursion- -input- -input-reference- output)))

(def printer syntax/node->text/text ()
  (bind ((child-iomaps (as (map-ll* (ll (children-of -input-))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the syntax/node document)))
                                                                ,@(typed-reference (document-type -input-) -input-reference-))))
                                        (recurse-printer -recursion- (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-origin-character-index child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child-input (input-of child-iomap))
                                                  (child-output (output-of child-iomap))
                                                  (child-length (as (text/length child-output)))
                                                  (separator-length (aif (separator-of -input-) (text/length it) 0))
                                                  (last-line-length (as (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                              (while current-element)
                                                                              (for text = (output-of (value-of current-element)))
                                                                              (for position = (text/find text (text/last-position text) (lambda (ch) (char= ch #\NewLine)) :direction :backward))
                                                                              (if position
                                                                                  (return (- (text/length text) (text/length text position (text/last-position text))))
                                                                                  (summing (text/length text) :into result))
                                                                              (summing separator-length :into result)
                                                                              (awhen (indentation-of (input-of (value-of current-element)))
                                                                                (return (+ result it)))
                                                                              (finally (return (+ result (aif (opening-delimiter-of -input-) (text/length it) 0)))))))
                                                  (indentation (when (typep child-input 'syntax/base) (indentation-of child-input)))
                                                  (line-indentation (or indentation (va last-line-length)))
                                                  (indentation-length (as (bind ((child-line-count (text/count-lines child-output)))
                                                                            (+ (* line-indentation (1- child-line-count))
                                                                               (if indentation (1+ indentation) 0))))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of child-output))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element))
                                                                                                                                           (font (or (when (typep child-element 'text/string)
                                                                                                                                                       (font-of child-element))
                                                                                                                                                     *font/default*)))
                                                                                                                                      (if (and indentation
                                                                                                                                               (not (previous-element-of child-element-element)))
                                                                                                                                          (if (zerop indentation)
                                                                                                                                              (ll (list (text/newline)
                                                                                                                                                        child-element)
                                                                                                                                                  1)
                                                                                                                                              (ll (list (text/newline)
                                                                                                                                                        (text/string (make-string-of-spaces indentation) :font font)
                                                                                                                                                        child-element)
                                                                                                                                                  2))
                                                                                                                                          (if (and (not (zerop line-indentation))
                                                                                                                                                   (text/newline? child-element)
                                                                                                                                                   ;; TODO: delme or not? this produces extra indentation
                                                                                                                                                   #+nil(previous-element-of child-element-element))
                                                                                                                                              (ll (list child-element (text/string (make-string-of-spaces line-indentation) :font font)))
                                                                                                                                              (ll (list child-element)))))))))))
                                                                          (indented-child-origin-preceding-length (as (text/length (va indented-child) (text/first-position (va indented-child)) (text/origin-position (va indented-child)))))
                                                                          (indented-child-origin-following-length (as (text/length (va indented-child) (text/origin-position (va indented-child)) (text/last-position (va indented-child))))))
                                                                     (make-instance 'iomap/syntax/node->text/text/child
                                                                                    :content-iomap child-iomap
                                                                                    :indented-child indented-child
                                                                                    :line-indentation line-indentation
                                                                                    :origin-character-index (as (or child-origin-character-index
                                                                                                                    (and (va child-first-character-index)
                                                                                                                         (+ (va child-first-character-index) (va indented-child-origin-preceding-length)))
                                                                                                                    (and (va child-last-character-index)
                                                                                                                         (- (va child-last-character-index) (va indented-child-origin-following-length)))))
                                                                                    :first-character-index (as (or (va child-first-character-index)
                                                                                                                   (and child-origin-character-index
                                                                                                                        (- child-origin-character-index (va indented-child-origin-preceding-length)))
                                                                                                                   (and (va child-last-character-index)
                                                                                                                        (- (va child-last-character-index) (va child-length) (va indentation-length)))))
                                                                                    :last-character-index (as (or (va child-last-character-index)
                                                                                                                  (and child-origin-character-index
                                                                                                                       (+ child-origin-character-index (va indented-child-origin-following-length)))
                                                                                                                  (and (va child-first-character-index)
                                                                                                                       (+ (va child-first-character-index) (va child-length) (va indentation-length))))))))
                                                               (as (or previous-element
                                                                       (awhen (previous-element-of element)
                                                                         (recurse it nil -self- nil (as nil) (bind ((outer-self -self-)) (as (- (first-character-index-of (value-of outer-self)) separator-length)))))))
                                                               (as (or next-element
                                                                       (awhen (next-element-of element)
                                                                         (recurse it -self- nil nil (bind ((outer-self -self-)) (as (+ (last-character-index-of (value-of outer-self)) separator-length))) (as nil)))))))))
                                  (awhen (va child-iomaps)
                                    (recurse it nil nil 0 (as nil) (as nil))))))
         (output-elements (as (bind ((output-delimiters? (output-delimiters-p -projection-))
                                     (opening-delimiter (opening-delimiter-of -input-))
                                     (closing-delimiter (closing-delimiter-of -input-)))
                                (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
                                                (when (and output-delimiters? opening-delimiter) (elements-of opening-delimiter))
                                                (if (va node-child-iomaps)
                                                    (append-ll (bind ((indented-children (map-ll (if (collapsed-p -input-)
                                                                                                     (list-ll (value-of (first-element (va node-child-iomaps))))
                                                                                                     (va node-child-iomaps))
                                                                                                 (lambda (node-child-iomap)
                                                                                                   ;; DEBUG: (format t "~%~S ~A ~A ~A" (text/as-string (indented-child-of node-child-iomap)) (first-character-index-of node-child-iomap) (origin-character-index-of node-child-iomap) (last-character-index-of node-child-iomap))
                                                                                                   (elements-of (indented-child-of node-child-iomap))))))
                                                                 (aif (separator-of -input-)
                                                                      (separate-elements-ll indented-children (ll (elements-of it)))
                                                                      indented-children)))
                                                    (list-ll #+nil (text/string "" :font *font/default*)))
                                                (when (collapsed-p -input-)
                                                  (list-ll (text/string " …"
                                                                        :font (font-of (last-elt (elements-of (output-of (content-iomap-of (value-of (first-element (va node-child-iomaps))))))))
                                                                        :font-color *color/solarized/gray*)))
                                                (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter))))))
         (output-selection (as (reference-case (get-selection -input-)
                                 (((the syntax/node document))
                                  (bind ((text (text/make-text output-elements))
                                         (origin-position (text/origin-position text))
                                         (start-index (- (text/length text (text/first-position text) origin-position)))
                                         (end-index (text/length text origin-position (text/last-position text))))
                                    `((the text/text (text/subbox (the text/text document) ,start-index ,end-index)))))
                                 (((the text/text (opening-delimiter-of (the syntax/node document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((text (text/make-text output-elements))
                                         (character-index (+ (- (text/length text (text/first-position text) (text/origin-position text))) ?character-index)))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the text/text (closing-delimiter-of (the syntax/node document)))
                                   (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
                                  (bind ((text (text/make-text output-elements))
                                         (closing-delimiter (closing-delimiter-of -input-))
                                         (character-index (+ (text/length closing-delimiter
                                                                          (text/first-position closing-delimiter)
                                                                          (text/relative-position closing-delimiter (text/origin-position closing-delimiter) ?character-index))
                                                             (text/length text (text/origin-position text) (text/last-position text)))))
                                    `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                 (((the sequence (children-of (the syntax/node document)))
                                   (the ?child-type (elt (the sequence document) ?child-index))
                                   . ?rest)
                                  (bind ((child-iomap (elt (va node-child-iomaps) ?child-index)))
                                    (reference-case (get-selection (output-of (content-iomap-of child-iomap)))
                                      (((the text/text (text/subseq (the text/text document) ?child-character-index ?child-character-index)))
                                       (bind ((character-index (find-parent-character-index child-iomap ?child-character-index)))
                                         `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
                                      (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                       `((the text/text (text/subbox (the text/text document)
                                                                     ,(find-parent-character-index child-iomap ?start-character-index)
                                                                     ,(find-parent-character-index child-iomap ?end-character-index))))))))
                                 (((the text/text (printer-output (the syntax/node document) ?projection ?recursion)) . ?rest)
                                  (when (eq -projection- ?projection)
                                    ?rest)))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-instance 'iomap/syntax/node->text/text
                   :projection -projection- :recursion -recursion-
                   :input -input- :input-reference -input-reference- :output output
                   :child-iomaps node-child-iomaps)))

;;;;;;
;;; Reader

(def function syntax/make-text-reference (projection recursion printer-iomap &optional child-index)
  (bind ((node-iomap (if child-index
                         (content-iomap-of (elt (child-iomaps-of printer-iomap) child-index))
                         printer-iomap))
         (node-input (input-of node-iomap))
         (node-output (output-of node-iomap))
         (reference (if (opening-delimiter-of node-input)
                        `((the text/text (opening-delimiter-of (the ,(document-type node-input) document)))
                          (the text/text (text/subseq (the text/text document) 0 0)))
                        (bind ((index (- (text/length node-output (text/first-position node-output) (text/origin-position node-output)))))
                          (typecase node-input
                            (syntax/leaf `((the text/text (content-of (the syntax/leaf document)))
                                           (the text/text (text/subseq (the text/text document) ,index ,index))))
                            (syntax/node `((the text/text (printer-output (the syntax/node document) ,projection ,recursion))
                                           (the text/text (text/subseq (the text/text document) ,index ,index)))))))))
    (if child-index
        (append `((the sequence (children-of (the syntax/node document)))
                  (the ,(document-type node-input) (elt (the sequence document) ,child-index)))
                reference)
        reference)))

(def reader syntax/delimitation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/indentation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/collapsible->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/leaf->text/text ()
  (bind ((selection (get-selection -printer-input-))
         (text-selection? (text/reference? selection))
         (syntax-selection? (syntax/reference? selection)))
    (merge-commands (reference-case selection
                      (((the ?content-type (content-of (the syntax/leaf document))) . ?rest)
                       (bind ((content-iomap (content-iomap-of -printer-iomap-))
                              (output-operation (operation-of (recurse-reader -recursion- (make-nothing-command -gesture-) content-iomap))))
                         (awhen (operation/extend -printer-input- `((the ,?content-type (content-of (the syntax/leaf document)))) output-operation)
                           (clone-command -input- it)))))
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-i :control)
                       :domain "Syntax" :description "Describes the node at the selection"
                       :operation (when syntax-selection? (make-operation/describe selection)))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a text selection"
                       :operation (when syntax-selection?
                                    (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a tree selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection -printer-input- '((the syntax/leaf document)))))
                      ((make-key-press-gesture :scancode-d :control)
                       :domain "Syntax" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p -projection-) (not (output-delimiters-p -projection-))))))
                      ((make-key-press-gesture :scancode-home '(:control :alt))
                       :domain "Syntax" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection `((the syntax/leaf document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection -printer-input- new-selection)))))
                    (bind ((command (command/read-backward -recursion- -input- -printer-iomap-))
                           (operation (when command (operation-of command))))
                      (if (and (typep -gesture- 'gesture/mouse/click)
                               (typep operation 'operation/replace-selection)
                               (equal (selection-of operation) selection))
                          (clone-command command (make-operation/replace-selection -printer-input- '((the syntax/leaf document))))
                          command))
                    (make-nothing-command -gesture-))))

(def reader syntax/separation->text/text ()
  (merge-commands (make-nothing-command -gesture-)))

(def reader syntax/node->text/text ()
  (bind ((selection (get-selection -printer-input-))
         (text-selection? (text/reference? selection))
         (syntax-selection? (syntax/reference? selection)))
    (merge-commands (reference-case selection
                      (((the sequence (children-of (the syntax/node document)))
                        (the ?element-type (elt (the sequence document) ?element-index)) . ?rest)
                       (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of -printer-iomap-) ?element-index)))
                              (child-input-command (make-nothing-command -gesture-))
                              (child-output-command (recurse-reader -recursion- child-input-command child-iomap))
                              (output-operation (operation-of child-output-command)))
                         (awhen (operation/extend -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                    (the ,?element-type (elt (the sequence document) ,?element-index)))
                                                  output-operation)
                           (clone-command child-output-command it)))))
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-i :control)
                       :domain "Syntax" :description "Describes the node at the selection"
                       :operation (when syntax-selection? (make-operation/describe selection)))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a text position selection"
                       :operation (when syntax-selection?
                                    (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a tree node selection"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                            ((the text/text (opening-delimiter-of (the syntax/node document)))
                                             (the text/text (text/subseq (the text/text document) ?start-index ?end-index))))
                                       (make-operation/replace-selection -printer-input- '((the syntax/node document)))))))
                      ((make-key-press-gesture :scancode-d :control)
                       :domain "Syntax" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p -projection-) (not (output-delimiters-p -projection-))))))
                      ((make-key-press-gesture :scancode-tab :control)
                       :domain "Syntax" :description "Expands or collapses the selected ndoe"
                       :operation (make-operation/syntax/toggle-collapsed -printer-input- '((the syntax/node document))))
                      ((make-key-press-gesture :scancode-home '(:control :alt))
                       :domain "Syntax" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection '((the syntax/node document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection -printer-input- new-selection))))
                      ((make-key-press-gesture :scancode-up)
                       :domain "Syntax" :description "Moves the selection to the parent node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (make-operation/replace-selection -printer-input- '((the syntax/node document)))))))
                      ((make-key-press-gesture :scancode-down)
                       :domain "Syntax" :description "Moves the selection to the first child node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the syntax/node document))
                                       (bind ((child-type (type-of (elt (children-of -printer-input-) 0))))
                                         (make-operation/replace-selection -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                             (the ,child-type (elt (the sequence document) 0))
                                                                                             (the ,child-type document))))))))
                      ((make-key-press-gesture :scancode-home)
                       :domain "Syntax" :description "Moves the selection to the first sibling node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (bind ((child-type (type-of (elt (children-of -printer-input-) 0))))
                                         (make-operation/replace-selection -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                             (the ,child-type (elt (the sequence document) 0))
                                                                                             (the ,child-type document))))))))
                      ((make-key-press-gesture :scancode-end)
                       :domain "Syntax" :description "Moves the selection to the last sibling node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (bind ((index (1- (length (children-of -printer-input-))))
                                              (child-type (type-of (elt (children-of -printer-input-) index))))
                                         (make-operation/replace-selection -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                             (the ,child-type (elt (the sequence document) ,index))
                                                                                             (the ,child-type document))))))))
                      ((make-key-press-gesture :scancode-left)
                       :domain "Syntax" :description "Moves the selection to the preceding sibling node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (when (> ?child-index 0)
                                         (bind ((child-type (type-of (elt (children-of -printer-input-) (1- ?child-index)))))
                                           (make-operation/replace-selection -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                               (the ,child-type (elt (the sequence document) ,(1- ?child-index)))
                                                                                               (the ,child-type document)))))))))
                      ((make-key-press-gesture :scancode-right)
                       :domain "Syntax" :description "Moves the selection to the following sibling node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (when (< ?child-index (1- (length (children-of -printer-input-))))
                                         (bind ((child-type (type-of (elt (children-of -printer-input-) (1+ ?child-index)))))
                                           (make-operation/replace-selection -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                               (the ,child-type (elt (the sequence document) ,(1+ ?child-index)))
                                                                                               (the ,child-type document)))))))))
                      ((make-key-press-gesture :scancode-up :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the parent node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/leaf (elt (the sequence document) ?child-index))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest))
                                       (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))))
                      ((make-key-press-gesture :scancode-down :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the first child node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest)
                                            ((the text/text (closing-delimiter-of (the syntax/node document)))
                                             . ?rest))
                                       (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap- 0))))))
                      ((make-key-press-gesture :scancode-home :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the first sibling node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap- 0))))))
                      ((make-key-press-gesture :scancode-end :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the last sibling node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1- (length (children-of -printer-input-)))))))))
                      ((make-key-press-gesture :scancode-left :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the preceding sibling node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (when (> ?child-index 0)
                                         (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1- ?child-index))))))))
                      ((make-key-press-gesture :scancode-right :alt)
                       :domain "Syntax" :description "Moves the selection to the first character of the following sibling node"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/node (elt (the sequence document) ?child-index))
                                             (the text/text (opening-delimiter-of (the syntax/node document)))
                                             . ?rest)
                                            ((the sequence (children-of (the syntax/node document)))
                                             (the syntax/leaf (elt (the sequence document) ?child-index))
                                             . ?rest))
                                       (when (< ?child-index (1- (length (child-iomaps-of -printer-iomap-))))
                                         (make-operation/replace-selection -printer-input- (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1+ ?child-index))))))))
                      ((make-key-press-gesture :scancode-delete)
                       :domain "Syntax" :description "Deletes the selected child from the node"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil))))
                      ((make-key-press-gesture :scancode-k :control)
                       :domain "Syntax" :description "Deletes the selected child from the node"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (make-operation/sequence/replace-range -printer-input- `((the sequence (children-of (the syntax/node document)))
                                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil))))
                      ((make-key-press-gesture :scancode-t :control)
                       :domain "Syntax" :description "Transposes the selected child and the following sibling"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (make-operation/sequence/swap-ranges -printer-input-
                                                                          `((the sequence (children-of (the syntax/node document)))
                                                                            (the sequence (subseq (the sequence document) ,(+ ?index 1) ,(+ ?index 2))))
                                                                          `((the sequence (children-of (the syntax/node document)))
                                                                            (the sequence (subseq (the sequence document) ,?index ,(+ ?index 1)))))))))
                    (bind ((command (command/read-backward -recursion- -input- -printer-iomap-))
                           (operation (when command (operation-of command))))
                      (if (and (typep -gesture- 'gesture/mouse/click)
                               (typep operation 'operation/replace-selection))
                          (cond ((equal (selection-of operation) selection)
                                 (clone-command command (make-operation/replace-selection -printer-input- '((the syntax/node document)))))
                                ((equal (modifiers-of -gesture-) '(:control))
                                 (make-command -gesture- (make-operation/syntax/toggle-collapsed -printer-input- '((the syntax/node document)))
                                               :domain "Syntax" :description "Expands or collapses the selected ndoe"))
                                (t command))
                          command))
                    (make-nothing-command -gesture-))))
