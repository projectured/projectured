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
              ;; TODO: when?
              :input input :input-reference (when input-reference (typed-reference (form-type input) input-reference)) :input-offset input-offset
              :output output :output-reference (typed-reference (form-type output) output-reference) :output-offset output-offset
              :length length))

;;;;;;
;;; Reference applier

#+nil
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

#+nil
(def forward-mapper iomap/text (iomap input-reference function)
  (bind ((string-input? (stringp (input-of iomap)))
         (string-output? (stringp (output-of iomap))))
    (if string-input?
        (pattern-case input-reference
          (((the sequence-position (pos (the string document) ?b)) . ?rest)
           (when (and (equal ?rest (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence-position (,(if string-output? 'pos 'text/pos) (the ,(if string-output? 'string 'text/text) document)
                                                                ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))))
                                       ,@(output-reference-of iomap)))))
          (((the sequence (subseq (the string ?a) ?b ?c)))
           (when (and (equal `(the string ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap)))
                      (<= (input-offset-of iomap) ?c (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence (,(if string-output? 'subseq 'text/subseq) ,(output-reference-of iomap)
                                                       ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))
                                                       ,(+ (- ?c (input-offset-of iomap)) (output-offset-of iomap)))))))))
        (pattern-case input-reference
          (((the sequence-position (text/pos (the text/text document) ?b)) . ?rest)
           (when (and (equal ?rest (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence-position (,(if string-output? 'pos 'text/pos) (the ,(if string-output? 'string 'text/text) document)
                                                                ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))))
                                       ,@(output-reference-of iomap)))))
          (((the sequence (text/subseq (the text/text ?a) ?b ?c)))
           (when (and (equal `(the text/text ,?a) (input-reference-of iomap))
                      (<= (input-offset-of iomap) ?b (+ (input-offset-of iomap) (length-of iomap)))
                      (<= (input-offset-of iomap) ?c (+ (input-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence (,(if string-output? 'subseq 'text/subseq) ,(output-reference-of iomap)
                                                       ,(+ (- ?b (input-offset-of iomap)) (output-offset-of iomap))
                                                       ,(+ (- ?c (input-offset-of iomap)) (output-offset-of iomap))))))))))))

;;;;;;
;;; Backward mapper

#+nil
(def backward-mapper iomap/text (iomap output-reference function)
  (bind ((string-input? (stringp (input-of iomap)))
         (string-output? (stringp (output-of iomap))))
    (if string-output?
        (pattern-case output-reference
          (((the character (elt (the string ?a) ?b)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the character (text/elt (the ,(if string-input? 'string 'text/text) document)
                                                                ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap)))))
          (((the sequence-position (pos (the string ?a) ?b)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence-position (text/pos (the ,(if string-input? 'string 'text/text) document)
                                                                        ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap)))))
          (((the sequence (subseq (the string ?a) ?b ?c)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap)))
                      (<= (output-offset-of iomap) ?c (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence (text/subseq (the ,(if string-input? 'string 'text/text) document)
                                                                  ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))
                                                                  ,(+ (- ?c (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap))))))
        (pattern-case output-reference
          (((the character (text/elt (the text/text ?a) ?b)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the character (,(if string-input? 'elt 'text/elt) (the ,(if string-input? 'string 'text/text) document)
                                                        ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap)))))
          (((the sequence-position (text/pos (the text/text ?a) ?b)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence-position (,(if string-input? 'pos 'text/pos) (the ,(if string-input? 'string 'text/text) document)
                                                                ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap)))))
          (((the sequence (text/subseq (the text/text ?a) ?b ?c)) . ?rest)
           (when (and (equal ?rest (output-reference-of iomap))
                      (<= (output-offset-of iomap) ?b (+ (output-offset-of iomap) (length-of iomap)))
                      (<= (output-offset-of iomap) ?c (+ (output-offset-of iomap) (length-of iomap))))
             (funcall function iomap `((the sequence (,(if string-input? 'subseq 'text/subseq) (the ,(if string-input? 'string 'text/text) document)
                                                       ,(+ (- ?b (output-offset-of iomap)) (input-offset-of iomap))
                                                       ,(+ (- ?c (output-offset-of iomap)) (input-offset-of iomap))))
                                       ,@(input-reference-of iomap)))))))))

;;;;;;
;;; Data structure

(def document text/base ()
  ())

(def document text/element (text/base)
  ((font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

(def document text/spacing (text/element)
  ((size :type number)
   (unit :type (member :pixel :space))))

(def document text/character (text/element)
  ((content :type character)))

(def document text/string (text/element)
  ((content :type string)))

(def document text/text (text/base document/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-text/spacing (size &key font (unit :pixel))
  (make-instance 'text/spacing
                 :size size
                 :font font
                 :unit unit))

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

(def (function e) make-text/text (elements &key selection)
  (make-instance 'text/text :elements (coerce elements 'vector) :selection selection))

;;;;;;
;;; Construction

(def (macro e) text/spacing (size &key font unit)
  `(make-text/spacing ,size :font ,(or font '*font/default*) :unit ,unit))

(def (macro e) text/character (content &key font font-color fill-color line-color)
  `(make-text/character ,content :font ,(or font '*font/default*) :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/string (content &key font font-color fill-color line-color)
  `(make-text/string ,content :font ,(or font '*font/default*) :font-color ,(or font-color '*color/default*) :fill-color ,fill-color :line-color ,line-color))

(def (macro e) text/newline ()
  `(make-text/string "
" :font *font/default* :font-color *color/default*))

(def (macro e) text/text ((&key selection) &body elements)
  `(make-text/text (list ,@elements) :selection ,selection))

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

(def (function e) text/substring* (text start-character-index &optional (end-character-index (text/length text)))
  (text/substring text
                  (text/element-index text start-character-index) (text/character-index text start-character-index)
                  (text/element-index text end-character-index) (text/character-index text end-character-index)))

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
           (incf element-index)
           (if (= element-index (length elements))
               (return (values element-index 0))
               (next-iteration))))))

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
  (bind ((elements (elements-of text)))
    (unless (zerop (length elements))
      (iter (with start-element-index = 0)
            (with start-character-index = 0)
            (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
            (funcall function start-element-index start-character-index end-element-index end-character-index)
            (while (< end-element-index (length elements)))
            (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
            (while (< start-element-index (length elements)))))))

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

(def (function e) text/push (text other)
  (setf (elements-of text) (concatenate 'vector (elements-of text) (elements-of other)))
  text)

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
(def function make-command-help-text (command)
  (bind ((gesture (gesture-of command))
         (modifier-text (gesture/describe-modifiers gesture))
         (gesture-text (gesture/describe-key gesture)))
    (list
     (text/string (or (domain-of command) "Unspecified") :font *font/default* :font-color *color/solarized/red*)
     (text/string " " :font *font/default* :font-color *color/black*)
     (text/string (string+ modifier-text gesture-text) :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)
     (text/string " " :font *font/default* :font-color *color/black*)
     (text/string (or (description-of command) "No description") :font *font/default* :font-color *color/black*)
     (text/newline))))

(def (function e) text/read-operation/replace-selection (text key &optional modifier)
  (pattern-case (selection-of text)
    (((the sequence-position (text/pos (the text/text ?a) ?character-index)) . ?rest)
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
                          (values (length (elt lines (1- y))) (1- y)))
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
              (selection (when character-index `((the sequence-position (text/pos (the text/text ,?a) ,character-index)) ,@?rest))))
         (when selection (make-operation/replace-selection text selection)))))
    (?a
     (bind ((character-index (case key
                               (:sdl-key-home
                                (when (eq modifier :control)
                                  0))
                               (:sdl-key-end
                                (when (eq modifier :control)
                                  (text/length text)))))
            (selection (when character-index `((the sequence-position (text/pos (the text/text document) ,character-index))))))
       (when selection (make-operation/replace-selection text selection))))))

(def (function e) text/read-operation (text gesture)
  (or (gesture-case gesture
        ((gesture/keyboard/key-press :sdl-key-left)
         :domain "Text" :help "Moves the selection one character to the left"
         :operation (text/read-operation/replace-selection text :sdl-key-left))
        ((gesture/keyboard/key-press :sdl-key-right)
         :domain "Text" :help "Moves the selection one character to the right"
         :operation (text/read-operation/replace-selection text :sdl-key-right))
        ((gesture/keyboard/key-press :sdl-key-left :control)
         :domain "Text" :help "Moves the selection one word to the left"
         :operation (text/read-operation/replace-selection text :sdl-key-left :control))
        ((gesture/keyboard/key-press :sdl-key-right :control)
         :domain "Text" :help "Moves the selection one word to the right"
         :operation (text/read-operation/replace-selection text :sdl-key-right :control))
        ((gesture/keyboard/key-press :sdl-key-up)
         :domain "Text" :help "Moves the selection one line up"
         :operation (text/read-operation/replace-selection text :sdl-key-up))
        ((gesture/keyboard/key-press :sdl-key-down)
         :domain "Text" :help "Moves the selection one line down"
         :operation (text/read-operation/replace-selection text :sdl-key-down))
        ((gesture/keyboard/key-press :sdl-key-pageup)
         :domain "Text" :help "Moves the selection one page up"
         :operation (text/read-operation/replace-selection text :sdl-key-pageup))
        ((gesture/keyboard/key-press :sdl-key-pagedown)
         :domain "Text" :help "Moves the selection one page down"
         :operation (text/read-operation/replace-selection text :sdl-key-pagedown))
        ((gesture/keyboard/key-press :sdl-key-home)
         :domain "Text" :help "Moves the selection to the beginning of the line"
         :operation (text/read-operation/replace-selection text :sdl-key-home))
        ((gesture/keyboard/key-press :sdl-key-end)
         :domain "Text" :help "Moves the selection to the end of the line"
         :operation (text/read-operation/replace-selection text :sdl-key-end))
        ((gesture/keyboard/key-press :sdl-key-home :control)
         :domain "Text" :help "Moves the selection to the beginning of the text"
         :operation (text/read-operation/replace-selection text :sdl-key-home :control))
        ((gesture/keyboard/key-press :sdl-key-end :control)
         :domain "Text" :help "Moves the selection to the end of the text"
         :operation (text/read-operation/replace-selection text :sdl-key-end :control))
        ((gesture/keyboard/key-press :sdl-key-delete)
         :domain "Text" :help "Deletes the character following the selection"
         :operation (pattern-case (selection-of text)
                      (((the sequence-position (pos (the string document) ?b)))
                       (when (< ?b (text/length text))
                         (make-operation/sequence/replace-element-range text `((the sequence (subseq (the string document) ,?b ,(1+ ?b)))) "")))
                      (((the sequence-position (text/pos (the text/text document) ?b)))
                       (when (< ?b (text/length text))
                         (make-operation/sequence/replace-element-range text `((the sequence (text/subseq (the text/text document) ,?b ,(1+ ?b)))) "")))))
        ((gesture/keyboard/key-press :sdl-key-backspace)
         :domain "Text" :help "Deletes the character preceding the selection"
         :operation (pattern-case (selection-of text)
                      (((the sequence-position (pos (the string document) ?b)))
                       (when (> ?b 0)
                         (make-operation/compound (list (make-operation/sequence/replace-element-range text `((the sequence (subseq (the string document) ,(1- ?b) ,?b))) "")
                                                        (make-operation/replace-selection text `((the sequence-position (pos (the string document) ,(1- ?b)))))))))
                      (((the sequence-position (text/pos (the text/text document) ?b)))
                       (when (> ?b 0)
                         (make-operation/compound (list (make-operation/sequence/replace-element-range text `((the sequence (text/subseq (the text/text document) ,(1- ?b) ,?b))) "")
                                                        (make-operation/replace-selection text `((the sequence-position (text/pos (the text/text document) ,(1- ?b))))))))))))
      ;; TODO: move into gesture-case
      (cond ((and (typep gesture 'gesture/keyboard/key-press)
                  (null (set-difference (modifiers-of gesture) '(:shift)))
                  (character-of gesture)
                  (or (graphic-char-p (character-of gesture))
                      (whitespace? (character-of gesture))))
             (bind ((character (character-of gesture))
                    (replacement (cond ((eq character #\Return)
                                        (string #\NewLine))
                                       (t (string character)))))
               (pattern-case (selection-of text)
                 (((the sequence-position (pos (the string (write-to-string (the number ?a))) ?b)))
                  (make-command gesture
                                (make-operation/number/replace-range text (selection-of text) replacement)
                                :domain "Text"
                                :description "Inserts a new character at the selection"))
                 (((the sequence-position (pos (the string ?a) ?b)))
                  (make-command gesture
                                (make-operation/compound (list (make-operation/sequence/replace-element-range text (selection-of text) replacement)
                                                               (make-operation/replace-selection text `(the sequence-position (pos (the string ,?a) ,(1+ ?b))))))
                                :domain "Text"
                                :description "Inserts a new character at the selection"))
                 (((the sequence-position (text/pos (the text/text document) ?b)) . ?rest)
                  (make-command gesture
                                (make-operation/compound (list (make-operation/sequence/replace-element-range text (selection-of text) replacement)
                                                               (make-operation/replace-selection text `((the sequence-position (text/pos (the text/text document) ,(1+ ?b))) ,@?rest))))
                                :domain "Text"
                                :description "Inserts a new character at the selection"))))))))
