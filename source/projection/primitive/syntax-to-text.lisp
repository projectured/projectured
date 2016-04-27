;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; TODO: factor out common parts

;;;;;;
;;; Projection

(def projection syntax/delimitation->text/text ()
  ((output-delimiters :type boolean)))

(def projection syntax/indentation->text/text ()
  ())

(def projection syntax/collapsible->text/text ()
  ())

(def projection syntax/navigation->text/text ()
  ())

(def projection syntax/leaf->text/text ()
  ((output-delimiters :type boolean)))

(def projection syntax/concatenation->text/text ()
  ())

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

(def function make-projection/syntax/navigation->text/text ()
  (make-projection 'syntax/navigation->text/text))

(def function make-projection/syntax/leaf->text/text ()
  (make-projection 'syntax/leaf->text/text :output-delimiters #t))

(def function make-projection/syntax/concatenation->text/text ()
  (make-projection 'syntax/concatenation->text/text))

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

(def macro syntax/navigation->text/text ()
  `(make-projection/syntax/navigation->text/text))

(def macro syntax/leaf->text/text ()
  `(make-projection/syntax/leaf->text/text))

(def macro syntax/concatenation->text/text ()
  `(make-projection/syntax/concatenation->text/text))

(def macro syntax/separation->text/text ()
  `(make-projection/syntax/separation->text/text))

(def macro syntax/node->text/text ()
  `(make-projection/syntax/node->text/text))

;;;;;;
;;; IO map

(def iomap iomap/syntax-child ()
  ((content-iomap :type iomap)
   (indented-child :type text/text)
   (line-indentation :type integer)
   (origin-character-index :type integer)
   (first-character-index :type integer)
   (last-character-index :type integer)))

;;;;;;
;;; Forward mapper

(def forward-mapper syntax/delimitation->text/text ()
  (bind ((content-iomap (content-iomap-of -printer-iomap-)))
    (reference-case -reference-
      (((the syntax/delimitation document))
       `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
      (((the text/text (opening-delimiter-of (the syntax/delimitation document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (bind ((content-output (output-of content-iomap))
              (opening-delimiter (opening-delimiter-of -printer-input-))
              (start-index (- (+ (text/length opening-delimiter (text/origin-relative-position opening-delimiter ?start-index) (text/last-position opening-delimiter))
                                 (text/origin-preceding-length content-output))))
              (end-index (- (+ (text/length opening-delimiter (text/origin-relative-position opening-delimiter ?end-index) (text/last-position opening-delimiter))
                               (text/origin-preceding-length content-output)))))
         `((the text/text (text/subseq (the text/text document) ,start-index ,end-index)))))
      (((the text/text (closing-delimiter-of (the syntax/delimitation document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (bind ((content-output (output-of content-iomap))
              (closing-delimiter (closing-delimiter-of -printer-input-))
              (start-index (+ (text/length closing-delimiter (text/first-position closing-delimiter) (text/origin-relative-position closing-delimiter ?start-index))
                              (text/origin-following-length content-output)))
              (end-index (+ (text/length closing-delimiter (text/first-position closing-delimiter) (text/origin-relative-position closing-delimiter  ?end-index))
                            (text/origin-following-length content-output))))
         `((the text/text (text/subseq (the text/text document) ,start-index ,end-index)))))
      (((the ?type (content-of (the syntax/delimitation document)))
        . ?rest)
       (values nil
               ?rest
               content-iomap
               (lambda (child-reference)
                 (reference-case child-reference
                   (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                    `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))))))))))

(def forward-mapper syntax/indentation->text/text ()
  (reference-case -reference-
    (((the syntax/indentation document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the ?type (content-of (the syntax/indentation document)))
      . ?rest)
     (values nil
             ?rest
             (content-iomap-of -printer-iomap-)
             (lambda (child-reference)
               (reference-case child-reference
                 (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                  (bind ((indentation (indentation-of -printer-input-))
                         (text (output-of (content-iomap-of -printer-iomap-)))
                         (first-position (text/first-position text))
                         (start-position (text/origin-relative-position text ?start-index))
                         (end-position (text/origin-relative-position text ?end-index))
                         (start-line-count (1+ (text/count text #\Newline first-position start-position)))
                         (end-line-count (1+ (text/count text #\Newline first-position end-position))))
                    `((the text/text (text/subseq (the text/text document)
                                                  ,(+ ?start-index (* indentation start-line-count) 1)
                                                  ,(+ ?end-index (* indentation end-line-count) 1))))))))))))

(def forward-mapper syntax/collapsible->text/text ()
  (reference-case -reference-
    (((the syntax/collapsible document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the ?type (content-of (the syntax/collapsible document)))
      . ?rest)
     (values nil
             ?rest
             (content-iomap-of -printer-iomap-)
             (lambda (child-reference)
               (reference-case child-reference
                 (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                  `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))))))

(def forward-mapper syntax/navigation->text/text ()
  (reference-case -reference-
    (((the syntax/navigation document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the ?type (content-of (the syntax/navigation document)))
      . ?rest)
     (values nil
             ?rest
             (content-iomap-of -printer-iomap-)
             (lambda (child-reference)
               (reference-case child-reference
                 (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                  `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))))))))))

(def forward-mapper syntax/leaf->text/text ()
  (bind ((content-iomap (content-iomap-of -printer-iomap-)))
    (reference-case -reference-
      (((the syntax/leaf document))
       `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
      (((the text/text (opening-delimiter-of (the syntax/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (bind ((content-output (output-of content-iomap))
              (opening-delimiter (opening-delimiter-of -printer-input-))
              (start-index (- (+ (text/length opening-delimiter (text/origin-relative-position opening-delimiter ?start-index) (text/last-position opening-delimiter))
                                 (text/origin-preceding-length content-output))))
              (end-index (- (+ (text/length opening-delimiter (text/origin-relative-position opening-delimiter ?end-index) (text/last-position opening-delimiter))
                               (text/origin-preceding-length content-output)))))
         `((the text/text (text/subseq (the text/text document) ,start-index ,end-index)))))
      (((the text/text (closing-delimiter-of (the syntax/leaf document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (bind ((content-output (output-of content-iomap))
              (closing-delimiter (closing-delimiter-of -printer-input-))
              (start-index (+ (text/length closing-delimiter (text/first-position closing-delimiter) (text/origin-relative-position closing-delimiter ?start-index))
                              (text/origin-following-length content-output)))
              (end-index (+ (text/length closing-delimiter (text/first-position closing-delimiter) (text/origin-relative-position closing-delimiter ?end-index))
                            (text/origin-following-length content-output))))
         `((the text/text (text/subseq (the text/text document) ,start-index ,end-index)))))
      (((the ?content-type (content-of (the syntax/leaf document)))
        . ?rest)
       (values nil
             ?rest
             content-iomap
             (lambda (child-reference)
               (reference-case child-reference
                 (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                  (bind ((indentation (indentation-of -printer-input-))
                         (text (output-of (content-iomap-of -printer-iomap-)))
                         (first-position (text/first-position text))
                         (start-position (text/origin-relative-position text ?start-index))
                         (end-position (text/origin-relative-position text ?end-index))
                         (start-line-count (1+ (text/count text #\Newline first-position start-position)))
                         (end-line-count (1+ (text/count text #\Newline first-position end-position))))
                    `((the text/text (text/subseq (the text/text document)
                                                  ,(+ ?start-index (* indentation start-line-count) 1)
                                                  ,(+ ?end-index (* indentation end-line-count) 1)))))))))))))

(def forward-mapper syntax/concatenation->text/text ()
  (reference-case -reference-
    (((the syntax/concatenation document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the sequence (children-of (the syntax/concatenation document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values nil
               ?rest
               (content-iomap-of child-iomap)
               (lambda (child-reference)
                 (reference-case child-reference
                   (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                    `((the text/text (text/subseq (the text/text document)
                                                  ,(+ (origin-character-index-of child-iomap) ?start-character-index)
                                                  ,(+ (origin-character-index-of child-iomap) ?end-character-index)))))
                   (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                    `((the text/text (text/subbox (the text/text document)
                                                  ,(+ (origin-character-index-of child-iomap) ?start-character-index)
                                                  ,(+ (origin-character-index-of child-iomap) ?end-character-index))))))))))))

(def forward-mapper syntax/separation->text/text ()
  (reference-case -reference-
    (((the syntax/separation document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the sequence (children-of (the syntax/separation document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
       (values nil
               ?rest
               (content-iomap-of child-iomap)
               (lambda (child-reference)
                 (reference-case child-reference
                   (((the text/text (text/subseq (the text/text document) ?start-character-index ?end-character-index)))
                    `((the text/text (text/subseq (the text/text document)
                                                  ,(+ (origin-character-index-of child-iomap) ?start-character-index)
                                                  ,(+ (origin-character-index-of child-iomap) ?end-character-index)))))
                   (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                    `((the text/text (text/subbox (the text/text document)
                                                  ,(+ (origin-character-index-of child-iomap) ?start-character-index)
                                                  ,(+ (origin-character-index-of child-iomap) ?end-character-index))))))))))))

(def forward-mapper syntax/node->text/text ()
  (reference-case -reference-
    (((the syntax/node document))
     `((the text/text (text/subbox (the text/text document) ,(- (text/origin-preceding-length -printer-output-)) ,(text/origin-following-length -printer-output-)))))
    (((the text/text (opening-delimiter-of (the syntax/node document)))
      (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
     (bind ((text (output-of -printer-iomap-))
            (character-index (+ (- (text/origin-preceding-length text)) ?character-index)))
       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
    (((the text/text (closing-delimiter-of (the syntax/node document)))
      (the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
     (bind ((text (output-of -printer-iomap-))
            (closing-delimiter (closing-delimiter-of -printer-input-))
            (character-index (+ (text/length closing-delimiter (text/first-position closing-delimiter) (text/origin-relative-position closing-delimiter ?character-index))
                                (text/origin-following-length text))))
       `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
    (((the sequence (children-of (the syntax/node document)))
      (the ?child-type (elt (the sequence document) ?child-index))
      . ?rest)
     (labels ((find-parent-character-index (child-iomap origin-relative-character-index)
                (bind ((line-indentation (line-indentation-of child-iomap))
                       (output (output-of (content-iomap-of child-iomap)))
                       (origin-character-index (origin-character-index-of child-iomap))
                       (origin-position (text/origin-position output))
                       (character-position (text/relative-position output origin-position origin-relative-character-index))
                       (line-count (if (< origin-relative-character-index 0)
                                       (text/count output #\Newline character-position origin-position)
                                       (text/count output #\Newline origin-position character-position)))
                       (indented-character-index (+ origin-relative-character-index (* line-count line-indentation))))
                  (+ origin-character-index indented-character-index))))
       (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?child-index)))
         (reference-case (get-selection (output-of (content-iomap-of child-iomap)))
           (((the text/text (text/subseq (the text/text document) ?child-character-index ?child-character-index)))
            (bind ((character-index (find-parent-character-index child-iomap ?child-character-index)))
              `((the text/text (text/subseq (the text/text document) ,character-index ,character-index)))))
           (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
            `((the text/text (text/subbox (the text/text document)
                                          ,(find-parent-character-index child-iomap ?start-character-index)
                                          ,(find-parent-character-index child-iomap ?end-character-index)))))))))))

;;;;;;
;;; Backward mapper

(def backward-mapper syntax/delimitation->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (bind ((text (output-of -printer-iomap-))
            (first-position (text/first-position text))
            (start-position (text/origin-relative-position text ?start-index))
            (end-position (text/origin-relative-position text ?end-index))
            (opening-delimiter-length (text/length (opening-delimiter-of -printer-input-)))
            (closing-delimiter-length (text/length (closing-delimiter-of -printer-input-)))
            (content-start-position (text/relative-position text (text/first-position text) (+ opening-delimiter-length)))
            (content-end-position (text/relative-position text (text/last-position text) (- closing-delimiter-length))))
       (cond ((and (text/position<= text content-start-position start-position)
                   (text/position<= text end-position content-end-position))
              (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/delimitation document))))
                      `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))
                      (content-iomap-of -printer-iomap-)))
             ((and (text/position<= text start-position content-start-position)
                   (text/position<= text end-position content-start-position))
              `((the text/text (opening-delimiter-of (the syntax/delimitation document)))
                (the text/text (text/subseq (the text/text document) ,(text/length text first-position start-position) ,(text/length text first-position end-position)))))
             ((and (text/position<= text content-end-position start-position)
                   (text/position<= text content-end-position end-position))
              `((the text/text (closing-delimiter-of (the syntax/delimitation document)))
                (the text/text (text/subseq (the text/text document) ,(text/length text content-end-position start-position) ,(text/length text content-end-position end-position))))))))))

(def backward-mapper syntax/indentation->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (bind ((text (output-of -printer-iomap-))
            (first-position (text/first-position text))
            (start-position (text/origin-relative-position text ?start-index))
            (end-position (text/origin-relative-position text ?end-index))
            (start-line-character-index (text/length text (text/line-start-position text start-position) start-position))
            (end-line-character-index (text/length text (text/line-start-position text end-position) end-position))
            (indentation (indentation-of -printer-input-)))
       (when (and (<= indentation start-line-character-index)
                  (<= indentation end-line-character-index))
         (bind ((start-line-count (text/count text #\Newline first-position start-position))
                (end-line-count (text/count text #\Newline first-position end-position)))
           (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/indentation document))))
                   `((the text/text (text/subseq (the text/text document) ,(- ?start-index (* indentation start-line-count) 1) ,(- ?end-index (* indentation end-line-count) 1))))
                   (content-iomap-of -printer-iomap-))))))))

(def backward-mapper syntax/collapsible->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (if (collapsed-p -printer-input-)
         (bind ((text (content-of -printer-input-))
                (first-position (text/first-position text))
                (first-line-length (text/length text first-position (text/line-end-position text first-position))))
           (when (and (< ?start-index first-line-length)
                      (< ?end-index first-line-length))
             `((the text/text (content-of (the syntax/collapsible document)))
               (the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))))
         (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/collapsible document))))
                 `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))
                 (content-iomap-of -printer-iomap-))))))

(def backward-mapper syntax/navigation->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/navigation document))))
             `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index)))
             (content-iomap-of -printer-iomap-)))))

(def backward-mapper syntax/leaf->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
     (bind ((indentation (indentation-of -printer-input-)))
       (when (or (not indentation)
                 (and (> ?start-index indentation)
                      (> ?end-index indentation)))
         (bind ((content-iomap (content-iomap-of -printer-iomap-))
                (content-output (output-of content-iomap))
                (start-position (text/origin-relative-position content-output ?start-index))
                (end-position (text/origin-relative-position content-output ?end-index)))
           (if (and start-position end-position)
               (values `((the ,(document-type (content-of -printer-input-)) (content-of (the syntax/leaf document))))
                       -reference-
                       content-iomap)
               (if (and (< ?start-index 0) (< ?end-index 0))
                   (bind ((offset (+ (text/length (opening-delimiter-of -printer-input-)) (text/origin-preceding-length content-output))))
                     `((the text/text (opening-delimiter-of (the syntax/leaf document)))
                       (the text/text (text/subseq (the text/text document) ,(+ offset ?start-index) ,(+ offset ?end-index)))))
                   (bind ((offset (- (text/origin-following-length content-output))))
                     `((the text/text (closing-delimiter-of (the syntax/leaf document)))
                       (the text/text (text/subseq (the text/text document) ,(+ offset ?start-index) ,(+ offset ?end-index)))))))))))))

(def backward-mapper syntax/concatenation->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
     (labels ((find-child-character-index (iomap parent-character-index)
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
                                                            (text/count indented-child #\Newline indented-character-position indented-origin-position)
                                                            (text/count indented-child #\Newline indented-origin-position indented-character-position)))
                                   (origin-relative-character-index (- indented-character-index (* indented-line-count line-indentation))))
                              (when (text/relative-position output (text/origin-position output) origin-relative-character-index)
                                (return (values index origin-relative-character-index))))))))))
       (bind (((:values child-start-index child-start-character-index) (find-child-character-index -printer-iomap- ?parent-start-character-index))
              ((:values child-end-index child-end-character-index) (find-child-character-index -printer-iomap- ?parent-end-character-index)))
         (if (and child-start-index child-end-index (= child-start-index child-end-index))
             (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of -printer-iomap-) child-start-index)))
                    (child (input-of child-iomap)))
               (values `((the sequence (children-of (the syntax/concatenation document)))
                         (the ,(document-type child) (elt (the sequence document) ,child-start-index)))
                       `((the text/text (text/subseq (the text/text document) ,child-start-character-index ,child-end-character-index)))
                       child-iomap))
             (when (and (< ?parent-start-character-index 0) (< ?parent-end-character-index 0))
               (bind ((offset (text/origin-preceding-length (output-of -printer-iomap-))))
                 `((the text/text (opening-delimiter-of (the syntax/concatenation document)))
                   (the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index))))))))))))

(def backward-mapper syntax/separation->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
     (labels ((find-child-character-index (iomap parent-character-index)
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
                                                            (text/count indented-child #\Newline indented-character-position indented-origin-position)
                                                            (text/count indented-child #\Newline indented-origin-position indented-character-position)))
                                   (origin-relative-character-index (- indented-character-index (* indented-line-count line-indentation))))
                              (when (text/origin-relative-position output origin-relative-character-index)
                                (return (values index origin-relative-character-index))))))))))
       (bind (((:values child-start-index child-start-character-index) (find-child-character-index -printer-iomap- ?parent-start-character-index))
              ((:values child-end-index child-end-character-index) (find-child-character-index -printer-iomap- ?parent-end-character-index)))
         (if (and child-start-index child-end-index (= child-start-index child-end-index))
             (bind ((child-iomap (content-iomap-of (elt (child-iomaps-of -printer-iomap-) child-start-index)))
                    (child (input-of child-iomap)))
               (values `((the sequence (children-of (the syntax/separation document)))
                         (the ,(document-type child) (elt (the sequence document) ,child-start-index)))
                       `((the text/text (text/subseq (the text/text document) ,child-start-character-index ,child-end-character-index)))
                       child-iomap))
             (when (and (< ?parent-start-character-index 0) (< ?parent-end-character-index 0))
               (bind ((offset (text/origin-preceding-length (output-of -printer-iomap-))))
                 `((the text/text (opening-delimiter-of (the syntax/separation document)))
                   (the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index))))))))))))

(def backward-mapper syntax/node->text/text ()
  (reference-case -reference-
    (((the text/text (text/subseq (the text/text document) ?parent-start-character-index ?parent-end-character-index)) . ?rest)
     (bind ((indentation (indentation-of -printer-input-)))
       (when (or (not indentation)
                 (and (> ?parent-start-character-index indentation)
                      (> ?parent-end-character-index indentation)))
         (labels ((find-child-character-index (iomap parent-character-index)
                    (iter (with parent-character-index = (if indentation
                                                             (- parent-character-index indentation)
                                                             parent-character-index))
                          (for index :from 0)
                          (for child-iomap-ll :initially (child-iomaps-of iomap) :then (next-element-of child-iomap-ll))
                          (while child-iomap-ll)
                          (for child-iomap = (value-of child-iomap-ll))
                          (for first-character-index = (first-character-index-of child-iomap))
                          (for origin-character-index = (origin-character-index-of child-iomap))
                          (for indented-child = (indented-child-of child-iomap))
                          ;; TODO: kills laziness (for last-character-index = (last-character-index-of child-iomap))
                          (when (and (<= first-character-index parent-character-index) ;; TODO: used to be this, it but kills laziness (<= first-character-index parent-character-index last-character-index)
                                     (text/origin-relative-position indented-child (- parent-character-index origin-character-index)))
                            ;;(format t "~%~A ~A ~A" first-character-index origin-character-index parent-character-index)
                            (bind ((line-indentation (line-indentation-of child-iomap))
                                   (indented-character-index (- parent-character-index origin-character-index))
                                   (indented-origin-position (text/origin-position indented-child))
                                   (indented-character-position (text/relative-position indented-child indented-origin-position indented-character-index))
                                   (indented-line-start-position (text/line-start-position indented-child indented-character-position))
                                   (indented-line-character-index (text/length indented-child indented-line-start-position indented-character-position))
                                   (output (output-of (content-iomap-of child-iomap))))
                              (when (or (text/first-position? indented-child indented-line-start-position)
                                        (<= line-indentation indented-line-character-index))
                                (bind ((indented-line-count (if (< indented-character-index 0)
                                                                (text/count indented-child #\Newline indented-character-position indented-origin-position)
                                                                (text/count indented-child #\Newline indented-origin-position indented-character-position)))
                                       (origin-relative-character-index (- indented-character-index (* indented-line-count line-indentation))))
                                  (when (text/origin-relative-position output origin-relative-character-index)
                                    (return (values index origin-relative-character-index))))))))))
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
                   (bind ((offset (text/origin-preceding-length (output-of -printer-iomap-))))
                     `((the text/text (opening-delimiter-of (the syntax/node document)))
                       (the text/text (text/subseq (the text/text document) ,(+ offset ?parent-start-character-index) ,(+ offset ?parent-end-character-index))))))))))))))

;;;;;;
;;; Printer

(def printer syntax/delimitation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (bind ((opening-delimiter (opening-delimiter-of -input-))
                                     (closing-delimiter (closing-delimiter-of -input-)))
                                (if (output-delimiters-p -projection-)
                                    (concatenate-ll (if opening-delimiter 1 0)
                                                    (when opening-delimiter (elements-of opening-delimiter))
                                                    (elements-of (output-of (va content-iomap)))
                                                    (when closing-delimiter (elements-of closing-delimiter)))
                                    (elements-of (output-of (va content-iomap)))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer syntax/indentation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (elements-of (text/indent (output-of (va content-iomap)) (indentation-of -input-) :insert-new-line #t))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer syntax/collapsible->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (bind ((elements (elements-of (output-of (va content-iomap)))))
                                (concatenate-ll 0
                                                (if (collapsed-p -input-)
                                                    (list-ll (first-elt elements))
                                                    elements)
                                                (when (collapsed-p -input-)
                                                  (list-ll (text/string " …"
                                                                        :font *font/default*
                                                                        :font-color *color/solarized/gray*)))))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer syntax/navigation->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (elements-of (output-of (va content-iomap)))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer syntax/leaf->text/text ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `(((content-of (the ,(document-type -input-) document)))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output-elements (as (bind ((opening-delimiter (opening-delimiter-of -input-))
                                     (closing-delimiter (closing-delimiter-of -input-))
                                     (elements (if (output-delimiters-p -projection-)
                                                   (concatenate-ll (if opening-delimiter 1 0)
                                                                   (when opening-delimiter (elements-of opening-delimiter))
                                                                   (elements-of (output-of (va content-iomap)))
                                                                   (when closing-delimiter (elements-of closing-delimiter)))
                                                   (elements-of (output-of (va content-iomap)))))
                                     (indentation (indentation-of -input-)))
                                (if indentation
                                    (elements-of (text/indent (text/make-text elements) indentation :insert-new-line #t))
                                    elements))))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer syntax/concatenation->text/text ()
  (bind ((child-iomaps (as (map-ll* (ll (children-of -input-))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the syntax/node document)))
                                                                ,@(typed-reference (document-type -input-) -input-reference-))))
                                        (recurse-printer -recursion- (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-origin-character-index child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child-output (output-of child-iomap))
                                                  (child-length (as (text/length child-output)))
                                                  (last-line-length (as (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                              (while current-element)
                                                                              (for text = (output-of (value-of current-element)))
                                                                              (for position = (text/find text (text/last-position text) (lambda (ch) (char= ch #\Newline)) :direction :backward))
                                                                              (if position
                                                                                  (return (- (text/length text) (text/length text position (text/last-position text))))
                                                                                  (summing (text/length text) :into result))
                                                                              (finally (return (+ result 0))))))
                                                  (line-indentation (if (text/newline? (first-elt (elements-of child-output)))
                                                                        0
                                                                        (va last-line-length)))
                                                  (indentation-length (as (bind ((child-line-count (text/count-lines child-output)))
                                                                            (* line-indentation (1- child-line-count))))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of child-output))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element))
                                                                                                                                           (font (or (when (typep child-element 'text/string)
                                                                                                                                                       (font-of child-element))
                                                                                                                                                     *font/default*)))
                                                                                                                                      (if (and (not (zerop line-indentation))
                                                                                                                                               (text/newline? child-element))
                                                                                                                                          (ll (list child-element (text/string (make-string-of-spaces line-indentation) :font font)))
                                                                                                                                          (ll (list child-element))))))))))
                                                                          (indented-child-origin-preceding-length (as (text/origin-preceding-length (va indented-child))))
                                                                          (indented-child-origin-following-length (as (text/origin-following-length (va indented-child)))))
                                                                     (make-instance 'iomap/syntax-child
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
                                                                         (recurse it nil -self- nil (as nil) (bind ((outer-self -self-)) (as (first-character-index-of (value-of outer-self))))))))
                                                               (as (or next-element
                                                                       (awhen (next-element-of element)
                                                                         (recurse it -self- nil nil (bind ((outer-self -self-)) (as (last-character-index-of (value-of outer-self)))) (as nil)))))))))
                                  (awhen (va child-iomaps)
                                    (recurse it nil nil 0 (as nil) (as nil))))))
         (output-elements (as (if (va node-child-iomaps)
                                  (append-ll (map-ll (va node-child-iomaps)
                                                     (lambda (node-child-iomap)
                                                       (elements-of (indented-child-of node-child-iomap)))))
                                  (list-ll (text/string "" :font *font/default*)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output node-child-iomaps)))

(def printer syntax/separation->text/text ()
  (bind ((child-iomaps (as (map-ll* (ll (children-of -input-))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the syntax/node document)))
                                                                ,@(typed-reference (document-type -input-) -input-reference-))))
                                        (recurse-printer -recursion- (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-origin-character-index child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child-output (output-of child-iomap))
                                                  (child-length (as (text/length child-output)))
                                                  (separator-length (aif (separator-of -input-) (text/length it) 0))
                                                  (last-line-length (as (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                              (while current-element)
                                                                              (for text = (output-of (value-of current-element)))
                                                                              (for position = (text/find text (text/last-position text) (lambda (ch) (char= ch #\Newline)) :direction :backward))
                                                                              (if position
                                                                                  (return (- (text/length text) (text/length text position (text/last-position text))))
                                                                                  (summing (text/length text) :into result))
                                                                              (summing separator-length :into result)
                                                                              (finally (return (+ result 0))))))
                                                  (line-indentation (if (text/newline? (first-elt (elements-of child-output)))
                                                                        0
                                                                        (va last-line-length)))
                                                  (indentation-length (as (bind ((child-line-count (text/count-lines child-output)))
                                                                            (* line-indentation (1- child-line-count))))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of child-output))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element))
                                                                                                                                           (font (or (when (typep child-element 'text/string)
                                                                                                                                                       (font-of child-element))
                                                                                                                                                     *font/default*)))
                                                                                                                                      (if (and (not (zerop line-indentation))
                                                                                                                                               (text/newline? child-element))
                                                                                                                                          (ll (list child-element (text/string (make-string-of-spaces line-indentation) :font font)))
                                                                                                                                          (ll (list child-element))))))))))
                                                                          (indented-child-origin-preceding-length (as (text/origin-preceding-length (va indented-child))))
                                                                          (indented-child-origin-following-length (as (text/origin-following-length (va indented-child)))))
                                                                     (make-instance 'iomap/syntax-child
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
         (output-elements (as (if (va node-child-iomaps)
                                  (append-ll (separate-elements-ll (map-ll (va node-child-iomaps)
                                                                           (lambda (node-child-iomap)
                                                                             (elements-of (indented-child-of node-child-iomap))))
                                                                   (ll (elements-of (separator-of -input-)))))
                                  (list-ll (text/string "" :font *font/default*)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output node-child-iomaps)))

(def printer syntax/node->text/text ()
  (bind ((child-iomaps (as (map-ll* (ll (children-of -input-))
                                    (lambda (child index)
                                      (bind ((child-reference `((elt (the sequence document) ,index)
                                                                (the sequence (children-of (the syntax/node document)))
                                                                ,@(typed-reference (document-type -input-) -input-reference-))))
                                        (recurse-printer -recursion- (value-of child) child-reference))))))
         (node-child-iomaps (as (labels ((recurse (element previous-element next-element child-origin-character-index child-first-character-index child-last-character-index)
                                           (bind ((child-iomap (value-of element))
                                                  (child-output (output-of child-iomap))
                                                  (child-length (as (text/length child-output)))
                                                  (separator-length (aif (separator-of -input-) (text/length it) 0))
                                                  (last-line-length (as (iter (for current-element :initially (previous-element-of element) :then (previous-element-of current-element))
                                                                              (while current-element)
                                                                              (for text = (output-of (value-of current-element)))
                                                                              (for position = (text/find text (text/last-position text) (lambda (ch) (char= ch #\Newline)) :direction :backward))
                                                                              (if position
                                                                                  (return (- (text/length text) (text/length text position (text/last-position text))))
                                                                                  (summing (text/length text) :into result))
                                                                              (summing separator-length :into result)
                                                                              (finally (return (+ result (aif (opening-delimiter-of -input-) (text/length it) 0)))))))
                                                  (line-indentation (if (text/newline? (first-elt (elements-of child-output)))
                                                                        0
                                                                        (va last-line-length)))
                                                  (indentation-length (as (bind ((child-line-count (text/count-lines child-output)))
                                                                            (* line-indentation (1- child-line-count))))))
                                             (make-computed-ll (as (bind ((indented-child (as (text/make-text (append-ll (map-ll* (ll (elements-of child-output))
                                                                                                                                  (lambda (child-element-element index)
                                                                                                                                    (declare (ignore index))
                                                                                                                                    (bind ((child-element (value-of child-element-element))
                                                                                                                                           (font (or (when (typep child-element 'text/string)
                                                                                                                                                       (font-of child-element))
                                                                                                                                                     *font/default*)))
                                                                                                                                      (if (and (not (zerop line-indentation))
                                                                                                                                               (text/newline? child-element))
                                                                                                                                          (ll (list child-element (text/string (make-string-of-spaces line-indentation) :font font)))
                                                                                                                                          (ll (list child-element))))))))))
                                                                          (indented-child-origin-preceding-length (as (text/origin-preceding-length (va indented-child))))
                                                                          (indented-child-origin-following-length (as (text/origin-following-length (va indented-child)))))
                                                                     (make-instance 'iomap/syntax-child
                                                                                    :content-iomap child-iomap
                                                                                    :indented-child indented-child
                                                                                    :line-indentation line-indentation
                                                                                    :origin-character-index (as (or child-origin-character-index
                                                                                                                    (and (va child-first-character-index)
                                                                                                                         (+ (va child-first-character-index) (va indented-child-origin-preceding-length)))
                                                                                                                    (and (va child-last-character-index)
                                                                                                                         (- (va child-last-character-index) (va indented-child-origin-following-length)))))
                                                                                    :first-character-index (va (as (or (va child-first-character-index)
                                                                                                                       (and child-origin-character-index
                                                                                                                            (- child-origin-character-index (va indented-child-origin-preceding-length)))
                                                                                                                       (and (va child-last-character-index)
                                                                                                                            (- (va child-last-character-index) (va child-length) (va indentation-length))))))
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
                                     (closing-delimiter (closing-delimiter-of -input-))
                                     (indentation (indentation-of -input-))
                                     (elements (concatenate-ll (if (and output-delimiters? opening-delimiter) 1 0)
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
                                                               (when (and output-delimiters? closing-delimiter) (elements-of closing-delimiter)))))
                                (if indentation
                                    (elements-of (text/indent (text/make-text elements) indentation :insert-new-line #t))
                                    elements))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (text/make-text output-elements :selection output-selection)))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output node-child-iomaps)))

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
                        (bind ((index (- (text/origin-preceding-length node-output))))
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
  (bind ((selection (get-selection -printer-input-))
         (text-selection? (text/reference? selection))
         (syntax-selection? (syntax/reference? selection)))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a text selection"
                       :operation (when syntax-selection?
                                    (make-operation/replace-selection `((the text/text (opening-delimiter-of (the syntax/delimitation document)))
                                                                        (the text/text (text/subseq (the text/text document) 0 0))))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a syntax selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection '((the syntax/delimitation document)))))
                      ((make-key-press-gesture :scancode-d :control)
                       :domain "Syntax" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p -projection-) (not (output-delimiters-p -projection-)))))))
                    (command/read-backward -recursion- -input- -printer-iomap-)
                    (make-nothing-command -gesture-))))

(def reader syntax/indentation->text/text ()
  (bind ((operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (reference-case selection
                                  (((the text/text (printer-output (the syntax/indentation document) ?projection ?recursion))
                                    (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                   (make-operation/functional (lambda ()))))))))
         (selection (get-selection -printer-input-))
         (text-selection? (text/reference? selection))
         (syntax-selection? (syntax/reference? selection)))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a text selection"
                       :operation (when syntax-selection?
                                    (make-operation/replace-selection `((the text/text (printer-output (the syntax/indentation document) ,-projection- ,-recursion-))
                                                                        (the text/text (text/subseq (the text/text document) 0 0))))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a syntax selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection '((the syntax/indentation document))))))
                    (command/read-backward -recursion- -input- -printer-iomap- operation-mapper)
                    (make-nothing-command -gesture-))))

(def reader syntax/collapsible->text/text ()
  (bind ((selection (get-selection -printer-input-))
         (text-selection? (text/reference? selection))
         (syntax-selection? (syntax/reference? selection)))
    (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                    (gesture-case -gesture-
                      ((make-key-press-gesture :scancode-tab :control)
                       :domain "Syntax" :description "Expands or collapses the selected node"
                       :operation (make-operation/syntax/toggle-collapsed '((the syntax/collapsible document))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a text selection"
                       :operation (when syntax-selection?
                                    (make-operation/replace-selection `((the text/text (printer-output (the syntax/collapsible document) ,-projection- ,-recursion-))
                                                                        (the text/text (text/subseq (the text/text document) 0 0))))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a syntax selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection '((the syntax/collapsible document))))))
                    (command/read-backward -recursion- -input- -printer-iomap-)
                    (make-nothing-command -gesture-))))

(def reader syntax/navigation->text/text ()
  ;; TODO: implement tree navigation
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

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
                                    (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a syntax selection"
                       :operation (when text-selection?
                                    (make-operation/replace-selection '((the syntax/leaf document)))))
                      ((make-key-press-gesture :scancode-d :control)
                       :domain "Syntax" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p -projection-) (not (output-delimiters-p -projection-))))))
                      ((make-key-press-gesture :scancode-home '(:control :alt))
                       :domain "Syntax" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection `((the syntax/leaf document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection new-selection)))))
                    (bind ((command (command/read-backward -recursion- -input- -printer-iomap-))
                           (operation (when command (operation-of command))))
                      (if (and (typep -gesture- 'gesture/mouse/click)
                               (typep operation 'operation/replace-selection)
                               (equal (selection-of operation) selection))
                          (clone-command command (make-operation/replace-selection '((the syntax/leaf document))))
                          command))
                    (make-nothing-command -gesture-))))

(def reader syntax/concatenation->text/text ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader syntax/separation->text/text ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

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
                                    (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))
                      ((make-key-press-gesture :scancode-space :control)
                       :domain "Syntax" :description "Turns the selection into a tree node selection"
                       :operation (when text-selection?
                                    (reference-case selection
                                      ((?or ((the text/text (printer-output (the syntax/node document) ?projection ?recursion))
                                             (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                                            ((the text/text (opening-delimiter-of (the syntax/node document)))
                                             (the text/text (text/subseq (the text/text document) ?start-index ?end-index))))
                                       (make-operation/replace-selection '((the syntax/node document)))))))
                      ((make-key-press-gesture :scancode-d :control)
                       :domain "Syntax" :description "Toggles visibility of delimiters"
                       :operation (make-operation/functional (lambda () (setf (output-delimiters-p -projection-) (not (output-delimiters-p -projection-))))))
                      ((make-key-press-gesture :scancode-tab :control)
                       :domain "Syntax" :description "Expands or collapses the selected node"
                       :operation (make-operation/syntax/toggle-collapsed '((the syntax/node document))))
                      ((make-key-press-gesture :scancode-home '(:control :alt))
                       :domain "Syntax" :description "Moves the selection to the root node"
                       :operation (bind ((new-selection '((the syntax/node document))))
                                    (unless (equal new-selection selection)
                                      (make-operation/replace-selection new-selection))))
                      ((make-key-press-gesture :scancode-up)
                       :domain "Syntax" :description "Moves the selection to the parent node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the sequence (children-of (the syntax/node document)))
                                        (the ?child-type (elt (the sequence document) ?child-index))
                                        (the ?child-type document))
                                       (make-operation/replace-selection '((the syntax/node document)))))))
                      ((make-key-press-gesture :scancode-down)
                       :domain "Syntax" :description "Moves the selection to the first child node"
                       :operation (when syntax-selection?
                                    (reference-case selection
                                      (((the syntax/node document))
                                       (bind ((child-type (type-of (elt (children-of -printer-input-) 0))))
                                         (make-operation/replace-selection `((the sequence (children-of (the syntax/node document)))
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
                                         (make-operation/replace-selection `((the sequence (children-of (the syntax/node document)))
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
                                         (make-operation/replace-selection `((the sequence (children-of (the syntax/node document)))
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
                                           (make-operation/replace-selection `((the sequence (children-of (the syntax/node document)))
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
                                           (make-operation/replace-selection `((the sequence (children-of (the syntax/node document)))
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
                                       (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap-))))))
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
                                       (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap- 0))))))
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
                                       (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap- 0))))))
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
                                       (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1- (length (children-of -printer-input-)))))))))
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
                                         (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1- ?child-index))))))))
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
                                         (make-operation/replace-selection (syntax/make-text-reference -projection- -recursion- -printer-iomap- (1+ ?child-index))))))))
                      ((make-key-press-gesture :scancode-delete)
                       :domain "Syntax" :description "Deletes the selected child from the node"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      (the ?type document))
                                     (make-operation/sequence/replace-range `((the sequence (children-of (the syntax/node document)))
                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil))))
                      ((make-key-press-gesture :scancode-k :control)
                       :domain "Syntax" :description "Deletes the selected child from the node"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (make-operation/sequence/replace-range `((the sequence (children-of (the syntax/node document)))
                                                                              (the sequence (subseq (the sequence document) ,?index ,(1+ ?index))))
                                                                            nil))))
                      ((make-key-press-gesture :scancode-t :control)
                       :domain "Syntax" :description "Transposes the selected child and the following sibling"
                       :operation (reference-case selection
                                    (((the sequence (children-of (the syntax/node document)))
                                      (the ?type (elt (the sequence document) ?index))
                                      . ?rest)
                                     (make-operation/sequence/swap-ranges `((the sequence (children-of (the syntax/node document)))
                                                                            (the sequence (subseq (the sequence document) ,(+ ?index 1) ,(+ ?index 2))))
                                                                          `((the sequence (children-of (the syntax/node document)))
                                                                            (the sequence (subseq (the sequence document) ,?index ,(+ ?index 1)))))))))
                    (bind ((command (command/read-backward -recursion- -input- -printer-iomap-))
                           (operation (when command (operation-of command))))
                      (if (and (typep -gesture- 'gesture/mouse/click)
                               (typep operation 'operation/replace-selection))
                          (cond ((equal (selection-of operation) selection)
                                 (clone-command command (make-operation/replace-selection '((the syntax/node document)))))
                                ((equal (modifiers-of -gesture-) '(:control))
                                 (make-command -gesture- (make-operation/syntax/toggle-collapsed '((the syntax/node document)))
                                               :domain "Syntax" :description "Expands or collapses the selected node"))
                                (t command))
                          command))
                    (make-nothing-command -gesture-))))
