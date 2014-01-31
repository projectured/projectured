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
;;; Reference applier

#+nil
(def reference-applier iomap/line-numbering (iomap reference function)
  (declare (ignore iomap reference function))
  (not-yet-implemented))

;;;;;;
;;; Forward mapper

#+nil
(def forward-mapper iomap/line-numbering (iomap input-reference function)
  ;; TODO: properly map forward character indices that refer into the line-number
  (labels ((map-character-index (character-index)
             (+ character-index
                (* (1+ (line-number-length-of iomap))
                   (1+ (count #\NewLine (text/as-string (input-of iomap)) :end character-index)))))
           (map-line-number-index (line-index character-index)
             (not-yet-implemented)))
    (pattern-case input-reference
      (((the sequence-position (text/pos (the text/text (line-number ?a ?b)) ?c)) . ?rest)
       (funcall function iomap `((the sequence-position (text/pos ,(output-reference-of iomap) ,(map-line-number-index ?b ?c))))))
      (((the sequence-position (text/pos (the text/text document) ?b)))
       (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,(map-character-index ?b))))))
      (((the sequence (text/subseq (the text/text ?a) ?b ?c)))
       (funcall function iomap `((the text/text (text/subseq ,(output-reference-of iomap),(map-character-index ?b) ,(map-character-index ?c))))))
      (((the sequence-box (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
       (funcall function iomap `((the sequence-box (text/subbox (the text/text document) ,(map-character-index ?start-character-index) ,(map-character-index ?end-character-index)))))))))

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
      (((the sequence-position (text/pos (the text/text ?a) ?b)))
       (bind (((:values line-number? line-index input-character-index) (map-character-index ?b)))
         (declare (ignore line-index))
         (unless line-number?
           `((the sequence-position (text/pos (the text/text document) ,input-character-index))))))
      (((the character (text/elt (the text/text ?a) ?b)))
       (bind (((:values line-number? line-index input-character-index) (map-character-index ?b)))
         (declare (ignore line-index))
         (unless line-number?
           `((the character (text/elt (the text/text document) ,input-character-index))))))
      (((the sequence (text/subseq (the text/text ?a) ?b ?c)))
       ;; TODO:
       `((the text/text (text/subseq (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c)))))
      (((the sequence-box (text/subbox (the text/text ?a) ?b ?c)))
       ;; TODO:
       `((the sequence-box (text/subbox (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c))))))))

#+nil
(def backward-mapper iomap/line-numbering (iomap output-reference function)
  ;; TODO: properly map back character indices that refer into the line-number
  (labels ((map-character-index (character-index)
             (bind ((output-string (text/as-string (output-of iomap)))
                    (line-number-length (line-number-length-of iomap))
                    (line-start-character-index (or (position #\NewLine output-string :from-end #t :end character-index) 0))
                    (line-number-reference? (<= (- character-index line-start-character-index) line-number-length))
                    (new-line-count (count #\NewLine output-string :end character-index)))
               (if line-number-reference?
                   (values #t new-line-count (- character-index line-start-character-index))
                   (values #f new-line-count (- character-index (* (1+ line-number-length) (1+ new-line-count))))))))
    (pattern-case output-reference
      (((the sequence-position (text/pos (the text/text ?a) ?b)))
       (bind (((:values line-number? line-index input-character-index) (map-character-index ?b)))
         (funcall function iomap `((the sequence-position (text/pos (the text/text document) ,input-character-index))
                                   ,@(if line-number?
                                         `((the text/text (line-number (the text/text document) ,line-index))
                                           ,@(input-reference-of iomap))
                                         (input-reference-of iomap))))))
      (((the sequence (text/subseq (the text/text ?a) ?b ?c)))
       ;; TODO:
       (funcall function iomap `((the text/text (text/subseq ,(input-reference-of iomap) ,(map-character-index ?b) ,(map-character-index ?c))))))
      (((the sequence-box (text/subbox (the text/text ?a) ?b ?c)))
       ;; TODO:
       (funcall function iomap `((the sequence-box (text/subbox ,(input-reference-of iomap) ,(map-character-index ?b) ,(map-character-index ?c)))))))))


;;;;;;
;;; Projection

(def projection line-numbering ()
  ((font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

;;;;;;
;;; Construction

(def (function e) make-projection/line-numbering (&key font font-color fill-color line-color)
  (make-projection 'line-numbering
                   :font (or font *font/ubuntu/monospace/regular/18*)
                   :font-color (or font-color *color/solarized/content/light*)
                   :fill-color fill-color
                   :line-color (or line-color *color/solarized/background/light*)))

;;;;;;
;;; Construction

(def (macro e) line-numbering (&key font font-color fill-color line-color)
  `(make-projection/line-numbering :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

;;;;;;
;;; Printer

(def printer line-numbering (projection recursion input input-reference)
  (bind ((line-count (1+ (text/count input #\NewLine)))
         (line-number-length (1+ (floor (log line-count) (log 10))))
         (line-number-format-string (format nil "\~~~A,' D " line-number-length))
         (line-index 0)
         (elements nil)
         (output (labels ((write-element (element)
                            (push element elements)))
                   (text/map-split input #\NewLine
                                   (lambda (start-element-index start-character-index end-element-index end-character-index)
                                     (bind ((line-number (format nil line-number-format-string (1+ line-index)))
                                            (line (text/substring input start-element-index start-character-index end-element-index end-character-index)))
                                       (write-element (make-text/string line-number
                                                                        :font (font-of projection)
                                                                        :font-color *color/solarized/content/light*
                                                                        :fill-color (fill-color-of projection)
                                                                        :line-color (line-color-of projection)))
                                       (iter (for line-element-index :from 0)
                                             (for line-element :in-sequence (elements-of line))
                                             (write-element line-element))
                                       (write-element (make-text/string (string #\NewLine) :font *font/default* :font-color *color/default*))
                                       (incf line-index))))
                   (make-text/text (nreverse (rest elements))
                                   :selection (labels ((map-character-index (character-index)
                                                         (+ character-index
                                                            (* (1+ line-number-length)
                                                               (1+ (count #\NewLine (text/as-string input) :end character-index))))))
                                                (pattern-case (selection-of input)
                                                  (((the sequence-position (text/pos (the text/text document) ?b)))
                                                   `((the sequence-position (text/pos (the text/text document) ,(map-character-index ?b)))))
                                                  (((the sequence (text/subseq (the text/text ?a) ?b ?c)))
                                                   `((the text/text (text/subseq document ,(map-character-index ?b) ,(map-character-index ?c)))))
                                                  (((the sequence-box (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
                                                   `((the sequence-box (text/subbox (the text/text document) ,(map-character-index ?start-character-index) ,(map-character-index ?end-character-index)))))))))))
    (make-iomap 'iomap/line-numbering
                :projection projection :recursion recursion
                :input input :input-reference (typed-reference (form-type input) input-reference)
                :output output
                :line-number-length line-number-length)))

;;;;;;
;;; Reader

(def function line-number-reference? (reference)
  (pattern-case reference
    (((the sequence-position (text/pos (the text/text (line-number ?a ?b)) ?c)))
     #t)))

(def function line-numbering/read-backward (command printer-iomap)
  (awhen (labels ((recurse (operation)
                    (typecase operation
                      (operation/quit operation)
                      (operation/replace-selection
                       (awhen (map-backward/line-numbering printer-iomap (selection-of operation))
                         (make-operation/replace-selection (input-of printer-iomap) it)))
                      (operation/sequence/replace-element-range
                       (make-operation/sequence/replace-element-range (input-of printer-iomap) (map-backward/line-numbering printer-iomap (target-of operation)) (replacement-of operation)))
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
