;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/line-numbering (iomap/text)
  ((input-reference :type reference)
   (output-reference :type reference)
   (line-number-length :type integer)
   (line-start-indices :type sequence)))

;;;;;;
;;; Backward mapper

(def function map-backward/line-numbering (iomap output-reference)
  (labels ((map-character-index (character-index)
             (bind ((output-string (text/as-string (output-of iomap)))
                    (line-number-length (line-number-length-of iomap))
                    (line-start-character-index (or (position #\NewLine output-string :from-end #t :end character-index) 0))
                    (line-number? (<= (- character-index line-start-character-index) line-number-length))
                    (new-line-count (count #\NewLine output-string :end character-index)))
               (values line-number? new-line-count (if line-number?
                                                       (- character-index line-start-character-index)
                                                       (- character-index (* (1+ line-number-length) (1+ new-line-count))))))))
    (pattern-case output-reference
      (((the text/text (text/subseq (the text/text ?a) ?b ?b)))
       (bind (((:values line-number? line-index input-character-index) (map-character-index ?b)))
         (declare (ignore line-index))
         (unless line-number?
           `((the text/text (text/subseq (the text/text document) ,input-character-index ,input-character-index))))))
      (((the text/text (text/subseq (the text/text ?a) ?b ?c)))
       ;; TODO:
       (bind (((:values b-line-number? b-line-index b-input-character-index) (map-character-index ?b))
              ((:values c-line-number? c-line-index c-input-character-index) (map-character-index ?c)))
         (declare (ignore b-line-index c-line-index))
         (unless (or b-line-number? c-line-number?)
           `((the text/text (text/subseq (the text/text document) ,b-input-character-index ,c-input-character-index))))))
      (((the sequence-box (text/subbox (the text/text ?a) ?b ?c)))
       ;; TODO:
       `((the sequence-box (text/subbox (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c))))))))

;;;;;;
;;; Projection

(def projection line-numbering ()
  ((font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

;;;;;;
;;; Construction

(def function make-projection/line-numbering (&key font font-color fill-color line-color)
  (make-projection 'line-numbering
                   :font (or font *font/ubuntu/monospace/regular/18*)
                   :font-color (or font-color *color/solarized/content/light*)
                   :fill-color fill-color
                   :line-color (or line-color *color/solarized/background/light*)))

;;;;;;
;;; Construction

(def macro line-numbering (&key font font-color fill-color line-color)
  `(make-projection/line-numbering :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

;;;;;;
;;; Printer

(def printer line-numbering (projection recursion input input-reference)
  (bind ((line-number-length (as (1+ (floor (log (1+ (text/count input #\NewLine))) (log 10)))))
         (output (as (make-text/text (as (bind ((elements nil)
                                                (line-index 0)
                                                (line-number-format-string (format nil "\~~~A,' D " (va line-number-length))))
                                           (text/map-split input #\NewLine
                                                           (lambda (start-element-index start-character-index end-element-index end-character-index)
                                                             (bind ((line-number (format nil line-number-format-string (1+ line-index)))
                                                                    (line (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                                                               (push (make-text/string line-number
                                                                                       :font (font-of projection)
                                                                                       :font-color *color/solarized/content/light*
                                                                                       :fill-color (fill-color-of projection)
                                                                                       :line-color (line-color-of projection))
                                                                     elements)
                                                               (iter (for line-element-index :from 0)
                                                                     (for line-element :in-sequence (elements-of line))
                                                                     (push line-element elements))
                                                               (push (make-text/string (string #\NewLine) :font *font/default* :font-color *color/default*) elements)
                                                               (incf line-index))))
                                           (nreverse (rest elements))))
                                     :selection (as (labels ((map-character-index (character-index)
                                                               (+ character-index
                                                                  (* (1+ (va line-number-length))
                                                                     (1+ (count #\NewLine (text/as-string input) :end character-index))))))
                                                      (pattern-case (selection-of input)
                                                        (((the text/text (text/subseq (the text/text ?a) ?b ?c)))
                                                         `((the text/text (text/subseq (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c)))))
                                                        (((the sequence-box (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                                         `((the sequence-box (text/subbox (the text/text document) ,(map-character-index ?start-character-index) ,(map-character-index ?end-character-index))))))))))))
    (make-iomap 'iomap/line-numbering
                :projection projection :recursion recursion
                :input input :output output :input-reference (typed-reference (form-type input) input-reference)
                :line-number-length line-number-length)))

;;;;;;
;;; Reader

(def function line-number-reference? (reference)
  (pattern-case reference
    (((the text/text (text/subseq (the text/text (line-number ?a ?b)) ?c ?c)))
     #t)))

(def function line-numbering/read-backward (command printer-iomap)
  (awhen (labels ((recurse (operation)
                    (typecase operation
                      (operation/quit operation)
                      (operation/functional operation)
                      (operation/replace-selection
                       (awhen (map-backward/line-numbering printer-iomap (selection-of operation))
                         (make-operation/replace-selection (input-of printer-iomap) it)))
                      (operation/sequence/replace-element-range
                       (awhen (map-backward/line-numbering printer-iomap (target-of operation))
                         (make-operation/sequence/replace-element-range (input-of printer-iomap) it (replacement-of operation))))
                      (operation/describe
                       (make-instance 'operation/describe :target (map-backward/line-numbering printer-iomap (target-of operation))))
                      (operation/show-context-sensitive-help
                       (make-instance 'operation/show-context-sensitive-help
                                      :commands (iter (for command :in (commands-of operation))
                                                      (awhen (recurse (operation-of command))
                                                        (collect (make-instance 'command
                                                                                :gesture (gesture-of command)
                                                                                :domain (domain-of command)
                                                                                :description (description-of command)
                                                                                :operation it))))))
                      (operation/compound
                       (bind ((child-operations (mapcar #'recurse (elements-of operation))))
                         (unless (some 'null child-operations)
                           (make-operation/compound child-operations)))))))
           (recurse (operation-of command)))
    (make-command (gesture-of command) it
                  :domain (domain-of command)
                  :description (description-of command))))

(def reader line-numbering (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (bind ((printer-input (input-of printer-iomap))
         (text-operation (text/read-operation printer-input (gesture-of input)))
         (output-operation (line-numbering/read-backward input printer-iomap)))
    (merge-commands text-operation output-operation (make-command/nothing (gesture-of input)))))
