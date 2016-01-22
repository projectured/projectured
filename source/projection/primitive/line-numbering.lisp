;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; IO map

(def iomap iomap/line-numbering ()
  ((line-number-length :type integer)))

;;;;;;
;;; Forward mapper

(def function forward-mapper/line-numbering (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (labels ((map-character-index (character-index)
               (bind ((character-position (text/relative-position printer-input (text/normalize-forward printer-input (text/origin-position printer-input)) character-index))
                      (line-count (text/count printer-input #\NewLine (text/first-position printer-input) character-position)))
                 (+ character-index
                    (text/length printer-input (text/first-position printer-input) (text/origin-position printer-input))
                    (* (1+ (line-number-length-of printer-iomap))
                       (1+ line-count))))))
      (pattern-case reference
        (((the text/text (text/subseq (the text/text ?a) ?b ?c)))
         `((the text/text (text/subseq (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c)))))
        (((the text/text (text/subbox (the text/text document) ?start-character-index ?end-character-index)))
         `((the text/text (text/subbox (the text/text document) ,(map-character-index ?start-character-index) ,(map-character-index ?end-character-index)))))
        (((the text/text (printer-output (the text/text document) ?projection ?recursion)) . ?rest)
         (when (eq projection ?projection)
           ?rest))))) )

;;;;;;
;;; Backward mapper

(def function backward-mapper/line-numbering (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (labels ((map-character-index (character-index)
               (bind ((output-string (text/as-string (output-of printer-iomap)))
                      (line-number-length (line-number-length-of printer-iomap))
                      (line-start-character-index (or (position #\NewLine output-string :from-end #t :end character-index) 0))
                      (line-number? (<= (- character-index line-start-character-index) line-number-length))
                      (new-line-count (count #\NewLine output-string :end character-index))
                      (origin-character-index (text/length printer-input (text/first-position printer-input) (text/origin-position printer-input))))
                 (values line-number? new-line-count (if line-number?
                                                         (- character-index origin-character-index line-start-character-index)
                                                         (- character-index origin-character-index (* (1+ line-number-length) (1+ new-line-count))))))))
      (pattern-case reference
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
        (((the text/text (text/subbox (the text/text ?a) ?b ?c)))
         ;; TODO:
         `((the text/text (text/subbox (the text/text document) ,(map-character-index ?b) ,(map-character-index ?c)))))
        (?a
         (append `((the text/text (printer-output (the text/text document) ,projection ,recursion))) reference))))))

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
                   :font (or font *font/ubuntu/monospace/regular/24*)
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
         (output-selection (as (print-selection (make-instance 'iomap/line-numbering
                                                               :projection projection :recursion recursion
                                                               :input input :input-reference input-reference
                                                               :line-number-length line-number-length)
                                                (selection-of input)
                                                'forward-mapper/line-numbering)))
         (output (as (text/make-text (as (bind ((elements nil)
                                                (line-index 0)
                                                (line-number-format-string (format nil "\~~~A,' D " (va line-number-length))))
                                           (text/map-split input #\NewLine
                                                           (lambda (start-position end-position)
                                                             (bind ((line-number (format nil line-number-format-string (1+ line-index)))
                                                                    (line (text/substring input start-position end-position)))
                                                               (push (text/make-string line-number
                                                                                       :font (font-of projection)
                                                                                       :font-color *color/solarized/content/light*
                                                                                       ;; KLUDGE: should be :line-color but changed to :fill-color for demo to avoid padding problems
                                                                                       :fill-color (line-color-of projection))
                                                                     elements)
                                                               (iter (for line-element :in-sequence (elements-of line))
                                                                     (push line-element elements))
                                                               (push (text/newline) elements)
                                                               (incf line-index))))
                                           (nreverse (rest elements))))
                                     :selection output-selection))))
    (make-instance 'iomap/line-numbering
                   :projection projection :recursion recursion
                   :input input :output output :input-reference input-reference
                   :line-number-length line-number-length)))

;;;;;;
;;; Reader

(def reader line-numbering (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (text/read-operation (input-of printer-iomap) (gesture-of input))
                  (command/read-backward recursion input printer-iomap 'backward-mapper/line-numbering nil)
                  (make-nothing-command (gesture-of input))))
