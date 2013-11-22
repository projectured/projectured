;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)


;;;;;;
;;; IO map

(def iomap iomap/text (iomap)
  ((input-reference :type reference)
   (output-reference :type reference)
   (input-offset :type integer)
   (output-offset :type integer)
   (length :type integer)))

;;;;;;
;;; Construction

(def (function e) make-iomap/text (projection recursion input input-reference input-offset output output-reference output-offset length)
  (make-iomap 'iomap/text
              :projection projection :recursion recursion
              :input input :input-reference (when input-reference `(the ,(form-type input) ,input-reference)) :input-offset input-offset
              :output output :output-reference (when output-reference `(the ,(form-type output) ,output-reference)) :output-offset output-offset
              :length length))

(def (function e) make-iomap/text* (projection recursion input input-reference input-offset output output-reference output-offset length)
  (make-iomap 'iomap/text
              :projection projection :recursion recursion
              :input input :input-reference input-reference :input-offset input-offset
              :output output :output-reference output-reference :output-offset output-offset
              :length length))

;;;;;;
;;; Reference applier

(def reference-applier iomap/text (iomap reference function)
  (declare (ignore function))
  (pattern-case reference
    ((the character (text/elt (the text/text ?a) ?b))
     (cond ((equal (input-reference-of iomap) `(the text/text ,?a))
            (text/elt (input-of iomap) ?b))
           ((equal (output-reference-of iomap) `(the text/text ,?a))
            (text/elt (output-of iomap) ?b))))))

;;;;;;
;;; Forward mapper

(def forward-mapper iomap/text (iomap input-reference function)
  (bind ((string-input? (stringp (input-of iomap)))
         (string-output? (stringp (output-of iomap))))
    (if string-input?
        (pattern-case input-reference
          ((the character (elt (the string ?a) ?b))
           (when (and (equal `(the string ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ -1 (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the character (,(if string-output? 'elt 'text/elt) ,(output-reference-of iomap)
                                                       ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
          ((the sequence-position (pos (the string ?a) ?b))
           (when (and (equal `(the string ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence-position (,(if string-output? 'pos 'text/pos) ,(output-reference-of iomap)
                                                               ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
          ((the sequence (subseq (the string ?a) ?b ?c))
           (when (and (equal `(the string ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap)))
                      (<= (input-offset-of iomap) ?c (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence (,(if string-output? 'subseq 'text/subseq) ,(output-reference-of iomap)
                                                      ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))
                                                      ,(+ (- ?c (input-offset-of iomap)) (output-offset-of iomap))))))))
        (pattern-case input-reference
          ((the character (text/elt (the text/text ?a) ?b))
           (when (and (equal `(the text/text ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ -1 (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the character (,(if string-output? 'elt 'text/elt) ,(output-reference-of iomap)
                                                       ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
          ((the sequence-position (text/pos (the text/text ?a) ?b))
           (when (and (equal `(the text/text ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence-position (,(if string-output? 'pos 'text/pos) ,(output-reference-of iomap)
                                                               ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap)))))))
          ((the sequence (text/subseq (the text/text ?a) ?b ?c))
           (when (and (equal `(the text/text ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap)))
                      (<= (input-offset-of iomap) ?c (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence (,(if string-output? 'subseq 'text/subseq) ,(output-reference-of iomap)
                                                      ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))
                                                      ,(+ (- ?c (input-offset-of iomap)) (output-offset-of iomap)))))))))))

;;;;;;
;;; Backward mapper

(def backward-mapper iomap/text (iomap output-reference function)
  (bind ((string-input? (stringp (input-of iomap)))
         (string-output? (stringp (output-of iomap))))
    (if string-output?
        (pattern-case output-reference
          ((the character (elt (the string ?a) ?b))
           (when (and (equal `(the string ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ -1 (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the character (text/elt ,(input-reference-of iomap)
                                                               ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
          ((the sequence-position (pos (the string ?a) ?b))
           (when (and (equal `(the text/string ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence-position (text/pos ,(input-reference-of iomap)
                                                                       ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
          ((the sequence (subseq (the string ?a) ?b ?c))
           (when (and (equal `(the string ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap)))
                      (<= (output-offset-of iomap) ?c (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence (text/subseq ,(input-reference-of iomap)
                                                                 ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))
                                                                 ,(+ (- ?c (output-offset-of iomap)) (input-offset-of iomap))))))))
        (pattern-case output-reference
          ((the character (text/elt (the text/text ?a) ?b))
           (when (and (equal `(the text/text ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ -1 (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the character (,(if string-input? 'elt 'text/elt) ,(input-reference-of iomap)
                                                       ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
          ((the sequence-position (text/pos (the text/text ?a) ?b))
           (when (and (equal `(the text/text ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence-position (,(if string-input? 'pos 'text/pos) ,(input-reference-of iomap)
                                                               ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap)))))))
          ((the sequence (text/subseq (the text/text ?a) ?b ?c))
           (when (and (equal `(the text/text ,?a) (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap)))
                      (<= (output-offset-of iomap) ?c (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `(the sequence (,(if string-input? 'subseq 'text/subseq) ,(input-reference-of iomap)
                                                      ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))
                                                      ,(+ (- ?c (output-offset-of iomap)) (input-offset-of iomap)))))))))))

;;;;;;
;;; Data structure

(def document text/base ()
  ((indentation :type integer)))

(def document text/element (text/base)
  ((font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

(def document text/character (text/element)
  ((content :type character)))

(def document text/string (text/element)
  ((content :type string)))

(def document text/text (text/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-text/character (content &key font font-color fill-color line-color)
  (make-instance 'text/character
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def (function e) make-text/string (content &key font font-color fill-color line-color)
  (make-instance 'text/string
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def (function e) make-text/text (elements)
  (make-instance 'text/text :elements (coerce elements 'vector)))

;;;;;;
;;; Construction

(def (macro e) text/character (content &key font font-color fill-color line-color)
  `(make-text/character ,content :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/string (content &key font font-color fill-color line-color)
  `(make-text/string ,content :font ,(or font '*font/default*) :font-color ,(or font-color '*color/default*) :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/newline ()
  `(make-text/string "
" :font *font/default* :font-color *color/default*))

(def (macro e) text/text ((&key) &body elements)
  `(make-text/text (list ,@elements)))

;;;;;;
;;; Operation data structure

(def operation operation/text/base (operation)
  ())

(def operation operation/text/replace-font (operation/text/base)
  ((selection :type selection)
   (font :type style/font)))

(def operation operation/text/replace-font-color (operation/text/base)
  ((selection :type selection)
   (font-color :type style/color)))

;;;;;;;
;;; Operation construction

(def (function e) make-operation/text/replace-font (selection font)
  (make-instance 'make-operation/text/replace-font :selection selection :font font))

(def (function e) make-operation/text/replace-font-color (selection color)
  (make-instance 'operation/text/replace-font-color :selection selection :color color))

;;;;;;;
;;; Operation API

(def method redo-operation ((operation operation/text/replace-font))
  (not-yet-implemented))

(def method redo-operation ((operation operation/text/replace-font-color))
  (not-yet-implemented))

;;;;;;
;;; API

(def (function e) text/elt (text character-index)
  (declare (ignore text character-index))
  (not-yet-implemented))

(def (function e) text/pos (text character-index)
  (declare (ignore text character-index))
  (not-yet-implemented))

(def (function e) text/subbox (text start-character-index end-character-index)
  (declare (ignore text start-character-index end-character-index))
  (not-yet-implemented))

(def (function e) text/length (text)
  (iter (for element :in-sequence (elements-of text))
        (summing
         (typecase element
           (text/string
            (length (content-of element)))
           (t 0)))))

(def (function e) text/substring (text start-element-index start-character-index end-element-index end-character-index)
  (make-text/text
   (iter (with elements = (elements-of text))
         (with elements-length = (length elements))
         (for element-index :from start-element-index :to end-element-index)
         (until (= element-index elements-length))
         (for element = (elt elements element-index))
         (typecase element
           (text/string
            (bind ((content (content-of element))
                   (content-length (length content))
                   (element-start-character-index (if (= element-index start-element-index)
                                                      start-character-index
                                                      0))
                   (element-end-character-index (if (= element-index end-element-index)
                                                    end-character-index
                                                    content-length)))
              (if (and (= element-start-character-index 0)
                       (= element-end-character-index content-length))
                  (collect element)
                  (bind ((word-part (subseq content element-start-character-index element-end-character-index)))
                    (unless (zerop (length word-part))
                      (collect (make-text/string word-part
                                                          :font (font-of element)
                                                          :font-color (font-color-of element)
                                                          :fill-color (fill-color-of element)
                                                          :line-color (line-color-of element))))))))
           (t
            (collect element))))))

(def (function e) text/find (text start-element-index start-character-index test)
  (iter (with elements = (elements-of text))
        (with element-index = start-element-index)
        (for element = (elt elements element-index))
        (for character-index :from start-character-index)
        (typecase element
          (text/string
           (for content = (content-of element))
           (when (= character-index (length content))
             (setf character-index -1)
             (incf element-index)
             (if (= element-index (length elements))
                 (return (values element-index 0))
                 (next-iteration)))
           (when (funcall test (elt content character-index))
             (return (values element-index character-index))))
          (t
           (setf character-index -1)
           (incf element-index)))))

(def (function e) text/count (text character)
  (iter (for element :in-sequence (elements-of text))
        (summing
         (typecase element
           (text/string
            (funcall 'count character (content-of element)))
           (t 0)))))

(def (function e) text/as-string (text)
  (with-output-to-string (stream)
    (iter (for element :in-sequence (elements-of text))
          (typecase element
            (text/string
             (write-string (content-of element) stream))))))

(def (function e) text/split (text split-character)
  (iter (with elements = (elements-of text))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
        (collect (text/substring text start-element-index start-character-index end-element-index end-character-index))
        (while (< end-element-index (length elements)))
        (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
        (while (< start-element-index (length elements)))))

(def (function e) text/map-split (text split-character function)
  (iter (with elements = (elements-of text))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
        (funcall function start-element-index start-character-index end-element-index end-character-index)
        (while (< end-element-index (length elements)))
        (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
        (while (< start-element-index (length elements)))))

(def (function e) text/next-index (text element-index character-index)
  (bind ((element (elt (elements-of text) element-index)))
    (typecase element
      (text/string
       (if (< (1+ character-index) (length (content-of element)))
           (values element-index (1+ character-index))
           (values (1+ element-index) 0)))
      (t (values (1+ element-index) 0)))))

(def (function e) text/concatenate (&rest texts)
  (make-text/text (apply #'concatenate 'vector (mapcar #'elements-of texts))))

(def (function e) text/element-index (text index)
  (iter (for element-index :from 0)
        (for element :in-sequence (elements-of text))
        (typecase element
          (text/string (decf index (length (content-of element)))))
        (when (<= index 0)
          (return element-index))))

(def (function e) text/character-index (text index)
  (iter (for element :in-sequence (elements-of text))
        (typecase element
          (text/string
           (for length = (length (content-of element)))
           (if (<= index length)
               (return index)
               (decf index length))))))

(def (function e) text/index (text element-index character-index)
  (+ (iter (for index :from 0 :below element-index)
           (for element :in-sequence (elements-of text))
           (summing
            (typecase element
              (text/string
               (length (content-of element)))
              (t 0))))
     character-index))

;; TODO: move and rename
(def function make-gesture-help-text (gesture->operation)
  (bind ((gesture (gesture-of gesture->operation))
         (modifier-text (when (modifiers-of gesture)
                          (with-output-to-string (string)
                            (iter (for modifier :in (modifiers-of gesture))
                                  (write-string (symbol-name modifier) string)
                                  (write-string  " + " string)))))
         (gesture-text (etypecase gesture
                         (gesture/keyboard/key-press
                          (string+ modifier-text
                                   (if (character-of gesture)
                                       (string (character-of gesture))
                                       (subseq (symbol-name (key-of gesture)) (length "SDL-KEY-")))))
                         (gesture/mouse/button/click
                          (string+ modifier-text (symbol-name (button-of gesture)))))))
    (list
     (text/string (domain-of gesture->operation) :font *font/default* :font-color *color/solarized/red*)
     (text/string " " :font *font/default* :font-color *color/black*)
     (text/string gesture-text :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)
     (text/string " " :font *font/default* :font-color *color/black*)
     (text/string (description-of gesture->operation) :font *font/default* :font-color *color/black*)
     (text/newline))))

(def (function e) text/read-operation/replace-selection (document text key &optional modifier)
  (pattern-case (selection-of document)
    ((the sequence-position (text/pos (the text/text ?a) ?character-index))
     (bind ((string (text/as-string text))
            (string-length (length string))
            (character-index ?character-index)
            (lines (split-sequence #\NewLine string))
            (height (1+ (count #\NewLine string)))
            ((:values x y)
             (iter (with line-begin-index = 0)
                   (for line-y :from 0)
                   (for line :in lines)
                   (for line-end-index = (+ line-begin-index (length line)))
                   (when (<= line-begin-index character-index line-end-index)
                     (return (values (- character-index line-begin-index) line-y)))
                   (setf line-begin-index (1+ line-end-index))))
            (line (elt lines y))
            (line-length (length line))
            (character-before (when (> character-index 0) (elt string (1- character-index))))
            (character-after (when (< character-index string-length) (elt string character-index)))
            ((:values new-x new-y new-character-index)
             (ecase key
               (:sdl-key-left
                (if (eq modifier :control)
                    (unless (= character-index 0)
                      (values nil nil
                              (1+ (or (if (alphanumericp character-before)
                                          (position-if-not 'alphanumericp string :end character-index :from-end #t)
                                          (position-if-not 'alphanumericp string :end (or (position-if 'alphanumericp string :end character-index :from-end #t) 0) :from-end #t))
                                      -1))))
                    (if (= x 0)
                        (unless (= y 0)
                          (values (length (elt lines y)) y))
                        (values (1- x) y))))
               (:sdl-key-right
                (if (eq modifier :control)
                    (unless (= character-index string-length)
                      (values nil nil
                              (or (if (alphanumericp character-after)
                                      (position-if-not 'alphanumericp string :start character-index)
                                      (position-if-not 'alphanumericp string :start (or (position-if 'alphanumericp string :start character-index)
                                                                                        string-length)))
                                  string-length)))
                    (if (= x (length line))
                        (unless (= y (1- height))
                          (values 0 (1+ y)))
                        (values (1+ x) y))))
               (:sdl-key-up
                (unless (= y 0)
                  (values x (1- y))))
               (:sdl-key-down
                (unless (= y (1- height))
                  (values x (1+ y))))
               (:sdl-key-home
                (if (eq modifier :control)
                    (values 0 0)
                    (values 0 y)))
               (:sdl-key-end
                (if (eq modifier :control)
                    (values (length (elt lines (1- height))) (1- height))
                    (unless (= x line-length)
                      (values line-length y))))
               (:sdl-key-pageup
                (unless (= y 0)
                  (if (> y 10)
                      (values x (- y 10))
                      (values x 0))))
               (:sdl-key-pagedown
                (unless (= y (1- height))
                  (if (< y (1- (- height 10)))
                      (values x (+ y 10))
                      (values x (1- height))))))))
       (when (and new-x new-y (> new-x (length (elt lines new-y))))
         (setf new-x (length (elt lines new-y))))
       (bind ((character-index (or (when (and new-character-index
                                              (not (= new-character-index character-index)))
                                     new-character-index)
                                   (when (and new-x new-y
                                              (or (not (= x new-x))
                                                  (not (= y new-y))))
                                     (iter (with line-begin-index = 0)
                                           (for line-y :from 0)
                                           (for line :in lines)
                                           (for line-end-index = (+ line-begin-index (length line) 1))
                                           (when (= new-y line-y)
                                             (return (+ line-begin-index new-x)))
                                           (setf line-begin-index line-end-index)))))
              (selection (when character-index `(the sequence-position (text/pos (the text/text ,?a) ,character-index)))))
         (when selection (make-operation/replace-selection document selection)))))
    (?a
     (bind ((character-index (case key
                               (:sdl-key-home
                                (when (eq modifier :control)
                                  0))
                               (:sdl-key-end
                                (when (eq modifier :control)
                                  (text/length text)))))
            (selection (when character-index `(the sequence-position (text/pos (the text/text (content-of (the document document))) ,character-index)))))
       (when selection (make-operation/replace-selection document selection))))))

(def (function e) text/read-operation (document text gesture)
  (or (gesture-case gesture
        ((gesture/keyboard/key-press :sdl-key-left)
         :domain "Text" :help "Moves the selection one character to the left"
         :operation (text/read-operation/replace-selection document text :sdl-key-left))
        ((gesture/keyboard/key-press :sdl-key-right)
         :domain "Text" :help "Moves the selection one character to the right"
         :operation (text/read-operation/replace-selection document text :sdl-key-right))
        ((gesture/keyboard/key-press :sdl-key-left :control)
         :domain "Text" :help "Moves the selection one word to the left"
         :operation (text/read-operation/replace-selection document text :sdl-key-left :control))
        ((gesture/keyboard/key-press :sdl-key-right :control)
         :domain "Text" :help "Moves the selection one word to the right"
         :operation (text/read-operation/replace-selection document text :sdl-key-right :control))
        ((gesture/keyboard/key-press :sdl-key-up)
         :domain "Text" :help "Moves the selection one line up"
         :operation (text/read-operation/replace-selection document text :sdl-key-up))
        ((gesture/keyboard/key-press :sdl-key-down)
         :domain "Text" :help "Moves the selection one line down"
         :operation (text/read-operation/replace-selection document text :sdl-key-down))
        ((gesture/keyboard/key-press :sdl-key-pageup)
         :domain "Text" :help "Moves the selection one page up"
         :operation (text/read-operation/replace-selection document text :sdl-key-pageup))
        ((gesture/keyboard/key-press :sdl-key-pagedown)
         :domain "Text" :help "Moves the selection pne page down"
         :operation (text/read-operation/replace-selection document text :sdl-key-pagedown))
        ((gesture/keyboard/key-press :sdl-key-home)
         :domain "Text" :help "Moves the selection to the beginning of the line"
         :operation (text/read-operation/replace-selection document text :sdl-key-home))
        ((gesture/keyboard/key-press :sdl-key-end)
         :domain "Text" :help "Moves the selection to the end of the line"
         :operation (text/read-operation/replace-selection document text :sdl-key-end))
        ((gesture/keyboard/key-press :sdl-key-home :control)
         :domain "Text" :help "Moves the selection to the beginning of the text"
         :operation (text/read-operation/replace-selection document text :sdl-key-home :control))
        ((gesture/keyboard/key-press :sdl-key-end :control)
         :domain "Text" :help "Moves the selection to the end of the text"
         :operation (text/read-operation/replace-selection document text :sdl-key-end :control))
        ((gesture/keyboard/key-press :sdl-key-delete)
         :domain "Text" :help "Deletes the character immediately following the selection"
         :operation (pattern-case (selection-of document)
                      ((the sequence-position (pos (the string ?a) ?b))
                       (when (< ?b (text/length (content-of document)))
                         (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,?b ,(1+ ?b))) "")))
                      ((the sequence-position (text/pos (the text/text ?a) ?b))
                       (when (< ?b (text/length (content-of document)))
                         (make-operation/sequence/replace-element-range document `(the sequence (text/subseq (the text/text ,?a) ,?b ,(1+ ?b))) "")))))
        ((gesture/keyboard/key-press :sdl-key-backspace)
         :domain "Text" :help "Deletes the character immediately preceding the selection"
         :operation (pattern-case (selection-of document)
                      ((the sequence-position (pos (the string ?a) ?b))
                       (when (> ?b 0)
                         (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the string ,?a) ,(1- ?b) ,?b)) "")
                                                        (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1- ?b))))))))
                      ((the sequence-position (text/pos (the text/text ?a) ?b))
                       (when (> ?b 0)
                         (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (text/subseq (the text/text ,?a) ,(1- ?b) ,?b)) "")
                                                        (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text ,?a) ,(1- ?b)))))))))))
      ;; TODO: move into gesture-case
      (cond ((and (typep gesture 'gesture/keyboard/key-press)
                  (null (set-difference (modifiers-of gesture) '(:shift)))
                  (or (graphic-char-p (character-of gesture))
                      (whitespace? (character-of gesture))))
             (bind ((character (character-of gesture))
                    (replacement (cond ((eq character #\Return)
                                        (string #\NewLine))
                                       (t (string character)))))
               (pattern-case (selection-of document)
                 ((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b))
                  (make-operation/number/replace-range document (selection-of document) replacement))
                 ((the sequence-position (pos (the string ?a) ?b))
                  (make-operation/compound (list (make-operation/sequence/replace-element-range document (selection-of document) replacement)
                                                 (make-operation/replace-selection document `(the sequence-position (pos (the string ,?a) ,(1+ ?b)))))))
                 ((the sequence-position (text/pos (the text/text ?a) ?b))
                  (make-operation/compound (list (make-operation/sequence/replace-element-range document (selection-of document) replacement)
                                                 (make-operation/replace-selection document `(the sequence-position (text/pos (the text/text ,?a) ,(1+ ?b)))))))))))))
