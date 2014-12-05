;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

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

(def document text/text (text/base)
  ((elements :type sequence)))

;;;;;;
;;; Construction

(def function text/make-spacing (size &key font font-color fill-color line-color (unit :pixel))
  (make-instance 'text/spacing
                 :size size
                 :unit unit
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def function text/make-character (content &key font font-color fill-color line-color)
  (check-type content character)
  (make-instance 'text/character
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def function text/make-string (content &key font font-color fill-color line-color)
  (check-type content string)
  (make-instance 'text/string
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

(def function text/make-text (elements &key selection)
  (make-instance 'text/text :elements elements :selection selection))

;;;;;;
;;; Construction

(def macro text/spacing (size &key font font-color fill-color line-color unit)
  `(text/make-spacing ,size :unit ,unit :font ,(or font '*font/default*) :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def macro text/character (content &key font font-color fill-color line-color)
  `(text/make-character ,content :font ,(or font '*font/default*) :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def macro text/string (content &key font font-color fill-color line-color)
  `(text/make-string ,content :font ,(or font '*font/default*) :font-color ,(or font-color '*color/default*) :fill-color ,fill-color :line-color ,line-color))

(def macro text/newline (&key font font-color fill-color line-color)
  `(text/make-string "
" :font ,(or font '*font/default*) :font-color ,(or font-color '*color/default*) :fill-color ,fill-color :line-color ,line-color))

(def macro text/text ((&key selection) &body elements)
  `(text/make-text (list ,@elements) :selection ,selection))

;;;;;;
;;; Text API

(def function text/make-position (element index)
  (cons element index))

(def (function io) text/position-element (position)
  (car position))

(def (function io) text/position-index (position)
  (cdr position))

(def function text/position= (position-1 position-2)
  (and (eql (text/position-element position-1)
            (text/position-element position-2))
       (= (text/position-index position-1)
          (text/position-index position-2))))

(def function text/origin-position (text)
  (bind ((elements (elements-of text)))
    (etypecase elements
      (null nil)
      (computed-ll (text/make-position elements 0))
      (sequence (text/make-position 0 0)))))

(def function text/first-position (text)
  (bind ((elements (elements-of text)))
    (etypecase elements
      (null nil)
      (computed-ll (text/make-position (first-element elements) 0))
      (sequence (text/make-position 0 0)))))

(def function text/last-position (text)
  (bind ((elements (elements-of text)))
    (etypecase elements
      (null nil)
      (computed-ll (bind ((last-element (last-element elements)))
                     (text/make-position last-element (length (content-of (value-of last-element))))))
      (sequence (bind ((last-element (last-elt elements)))
                  (text/make-position (1- (length elements)) (length (content-of last-element))))))))

(def function text/previous-position (text position)
  (bind ((element (text/position-element position))
         (index (text/position-index position)))
    (if (integerp element)
        (if (zerop index)
            (unless (zerop element)
              (text/make-position (1- element) (1- (length (content-of (elt (elements-of text) (1- element)))))))
            (text/make-position element (1- index)))
        (if (zerop index)
            (awhen (previous-element-of element)
              (text/make-position it (1- (length (content-of (value-of it))))))
            (text/make-position element (1- index))))))

(def function text/next-position (text position)
  (bind ((element (text/position-element position))
         (index (text/position-index position)))
    (if (integerp element)
        (bind ((elements (elements-of text))
               (element-length (length (content-of (elt elements element)))))
          (if (< index (1- element-length))
              (text/make-position element (1+ index))
              (if (= element (1- (length elements)))
                  (if (< index element-length)
                      (text/make-position element (1+ index)))
                  (text/make-position (1+ element) 0))))
        (bind ((element-length (length (content-of (value-of element)))))
          (if (< index (1- element-length))
              (text/make-position element (1+ index))
              (aif (next-element-of element)
                   (text/make-position it 0)
                   (if (< index element-length)
                       (text/make-position element (1+ index)))))))))

(def function text/sibling-position (text position direction)
  (ecase direction
    (:backward (text/previous-position text position))
    (:forward (text/next-position text position))))

(def function text/line-start-position (text start-position)
  (iter (for position :initially start-position :then (text/previous-position text position))
        (while position)
        (for character = (text/previous-character text position))
        (while character)
        (until (char= character #\NewLine))
        (finally (return position))))

(def function text/line-end-position (text start-position)
  (iter (for position :initially start-position :then (text/next-position text position))
        (while position)
        (for character = (text/next-character text position))
        (while character)
        (until (char= character #\NewLine))
        (finally (return position))))

(def function text/relative-position (text start-position distance)
  (iter (repeat (abs distance))
        (for position :initially start-position :then (if (> distance 0)
                                                          (text/next-position text position)
                                                          (text/previous-position text position)))
        (while position)
        (finally (return position))))

(def function text/previous-character (text position)
  (bind ((element (text/position-element position))
         (index (text/position-index position))
         (string (content-of (text/element text element))))
    (if (> index 0)
        (elt string (1- index))
        (if (integerp element)
            (unless (zerop element)
              (last-elt (content-of (elt (elements-of text) (1- element)))))
            (awhen (previous-element-of element)
              (last-elt (content-of (value-of it))))))))

(def function text/next-character (text position)
  (bind ((element (text/position-element position))
         (index (text/position-index position))
         (string (content-of (text/element text element))))
    (if (< index (length string))
        (elt string index))))

(def function text/first-element (text)
  (bind ((elements (elements-of text)))
    (if (typep elements 'computed-ll)
        (first-element elements)
        0)))

(def function text/last-element (text)
  (bind ((elements (elements-of text)))
    (if (typep elements 'computed-ll)
        (last-element elements)
        (1- (length (last-elt elements))))))

(def function text/previous-element (text element)
  (if (integerp element)
      (when (> element 0)
        (1- element))
      (previous-element-of element)))

(def function text/next-element (text element)
  (if (integerp element)
      (when (< element (1- (length (elements-of text))))
        (1+ element))
      (next-element-of element)))

(def function text/sibling-element (text element direction)
  (ecase direction
    (:backward (text/previous-element text element))
    (:forward (text/next-element text element))))

(def function text/element (text element)
  (if (integerp element)
      (elt (elements-of text) element)
      (value-of element)))

;; TODO: what is this?
(def function text/subbox (text start-position end-position)
  (declare (ignore text start-position end-position))
  (not-yet-implemented))

(def function text/empty? (text)
  (emptyp (elements-of text)))

(def function text/newline? (element)
  (and (typep element 'text/string)
       (string= (content-of element) "
")))

;; TODO: optimize
(def function text/length (text &optional (start-position (text/first-position text)) (end-position (text/last-position text)))
  (iter (for position :initially start-position :then (text/next-position text position))
        (until (text/position= position end-position))
        (summing 1)))

(def function text/substring (text start-position end-position)
  (bind ((start-element (text/element text (text/position-element start-position)))
         (end-element (text/element text (text/position-element end-position))))
    (if (eq start-element end-element)
        (text/text ()
          (text/string (subseq (content-of start-element) (text/position-index start-position) (text/position-index end-position))
                       :font (font-of start-element)
                       :font-color (font-color-of start-element)
                       :fill-color (fill-color-of start-element)
                       :line-color (line-color-of start-element)))
        (text/make-text
         (append (bind ((string (subseq (content-of start-element) (text/position-index start-position) (length (content-of start-element)))))
                   (unless (zerop (length string))
                     (list (text/string string :font (font-of start-element) :font-color (font-color-of start-element) :fill-color (fill-color-of start-element) :line-color (line-color-of start-element)))))
                 (iter (for element :initially (text/next-element text (text/position-element start-position)) :then (text/next-element text element))
                       (until (eq element (text/position-element end-position)))
                       (collect (text/element text element)))
                 (bind ((string (subseq (content-of end-element) 0 (text/position-index end-position))))
                   (unless (zerop (length string))
                     (list (text/string string :font (font-of end-element) :font-color (font-color-of end-element) :fill-color (fill-color-of end-element) :line-color (line-color-of end-element))))))))))

(def function text/subseq (text start-character-index &optional (end-character-index (text/length text (text/origin-position text) (text/last-position text))))
  (text/substring text
                  (text/relative-position text (text/origin-position text) start-character-index)
                  (text/relative-position text (text/origin-position text) end-character-index)))

(def function text/find (text start-position test &key (direction :forward))
  (iter (for position :initially start-position :then (ecase direction
                                                        (:backward (text/previous-position text position))
                                                        (:forward (text/next-position text position))))
        (while position)
        (for character = (ecase direction
                           (:backward (text/previous-character text position))
                           (:forward (text/next-character text position))))
        (until (and character (funcall test character)))
        (finally (return position))))

(def function text/count (text character)
  (iter (for element :in-sequence (elements-of text))
        (summing
         (typecase element
           (text/string
            (funcall 'count character (content-of element)))
           (t 0)))))

(def function text/as-string (text)
  (with-output-to-string (stream)
    (iter (for element :in-sequence (elements-of text))
          (typecase element
            (text/string
             (write-string (content-of element) stream))))))

(def function text/split (text split-character)
  (iter (with elements = (elements-of text))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
        (collect (text/substring text start-element-index start-character-index end-element-index end-character-index))
        (while (< end-element-index (length elements)))
        (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
        (while (< start-element-index (length elements)))))

(def function text/map-split (text split-character function)
  (bind ((elements (elements-of text)))
    (unless (zerop (length elements))
      (iter (with start-element-index = 0)
            (with start-character-index = 0)
            (for (values end-element-index end-character-index) = (text/find text start-element-index start-character-index (lambda (character) (char= character split-character))))
            (funcall function start-element-index start-character-index end-element-index end-character-index)
            (while (< end-element-index (length elements)))
            (setf (values start-element-index start-character-index) (text/next-index text end-element-index end-character-index))
            (while (< start-element-index (length elements)))))))

(def function text/next-index (text element-index character-index)
  (bind ((element (elt (elements-of text) element-index)))
    (typecase element
      (text/string
       (if (< (1+ character-index) (length (content-of element)))
           (values element-index (1+ character-index))
           (values (1+ element-index) 0)))
      (t (values (1+ element-index) 0)))))

(def function text/concatenate (&rest texts)
  (text/make-text (apply #'concatenate 'vector (mapcar #'elements-of texts))))

(def function text/push (text other)
  (setf (elements-of text) (concatenate 'vector (elements-of text) (elements-of other)))
  text)

(def function text/consolidate (text)
  (text/make-text (iter (with last-string-element = nil)
                        (for element :in-sequence (elements-of text))
                        (if (and last-string-element
                                 (typep element 'text/string)
                                 (eq (font-of element) (font-of last-string-element))
                                 (eq (font-color-of element) (font-color-of last-string-element))
                                 (eq (fill-color-of element) (fill-color-of last-string-element))
                                 (eq (line-color-of element) (line-color-of last-string-element)))
                            (setf (content-of last-string-element) (concatenate 'string (content-of last-string-element) (content-of element)))
                            (collect (if (typep element 'text/string)
                                         (setf last-string-element (text/make-string (copy-seq (content-of element)) :font (font-of element) :font-color (font-color-of element) :fill-color (fill-color-of element) :line-color (line-color-of element)))
                                         (progn
                                           (setf last-string-element nil)
                                           element))
                              :result-type vector)))
                  :selection (selection-of text)))

(def function text/element-index (text index)
  (iter (for element-index :from 0)
        (for element :in-sequence (elements-of text))
        (typecase element
          (text/string (decf index (length (content-of element)))))
        (when (<= index 0)
          (return element-index))))

(def function text/character-index (text index)
  (iter (for element :in-sequence (elements-of text))
        (typecase element
          (text/string
           (for length = (length (content-of element)))
           (if (<= index length)
               (return index)
               (decf index length))))))

(def function text/index (text element-index character-index)
  (+ (iter (for index :from 0 :below element-index)
           (for element :in-sequence (elements-of text))
           (summing
            (typecase element
              (text/string
               (length (content-of element)))
              (t 0))))
     character-index))

(def function text/style (text &key font font-color fill-color line-color)
  (text/make-text (iter (for element :in (elements-of text))
                        (collect (text/string (content-of element)
                                              :font (or font (font-of element))
                                              :font-color (or font-color (font-color-of element))
                                              :fill-color (or fill-color (fill-color-of element))
                                              :line-color (or line-color (line-color-of element)))))))

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
     (text/string (or (description-of command) "No description") :font *font/default* :font-color *color/black*))))

(def function text/read-replace-selection-operation (text key &optional modifier)
  (bind ((new-position
          (pattern-case (selection-of text)
            (((the text/text (text/subseq (the text/text document) ?character-index ?character-index)))
             (bind ((character-index ?character-index)
                    (origin-position (text/origin-position text))
                    (old-position (text/relative-position text origin-position character-index))
                    (line-start-position (text/line-start-position text old-position))
                    (line-end-position (text/line-end-position text old-position))
                    (line-character-index (text/length text line-start-position old-position)))
               (ecase key
                 (:sdl-key-left
                  (if (eq modifier :control)
                      (or (awhen (text/previous-character text old-position)
                            (if (alphanumericp it)
                                (text/find text old-position (complement #'alphanumericp) :direction :backward)
                                (awhen (text/find text old-position #'alphanumericp :direction :backward)
                                  (text/find text it (complement #'alphanumericp) :direction :backward))))
                          (text/first-position text))
                      (text/previous-position text old-position)))
                 (:sdl-key-right
                  (if (eq modifier :control)
                      (or (awhen (text/next-character text old-position)
                            (if (alphanumericp it)
                                (text/find text old-position (complement #'alphanumericp) :direction :forward)
                                (awhen (text/find text old-position #'alphanumericp :direction :forward)
                                  (text/find text it (complement #'alphanumericp) :direction :forward))))
                          (text/last-position text))
                      (text/next-position text old-position)))
                 (:sdl-key-up
                  (awhen (text/previous-position text line-start-position)
                    (bind ((previous-line-start-position (text/line-start-position text it))
                           (previous-line-length (1- (text/length text previous-line-start-position line-start-position))))
                      (text/relative-position text line-start-position (1- (min 0 (- line-character-index previous-line-length)))))))
                 (:sdl-key-down
                  (awhen (text/next-position text line-end-position)
                    (bind ((next-line-end-position (text/line-end-position text it))
                           (next-line-length (1- (text/length text line-end-position next-line-end-position))))
                      (text/relative-position text line-end-position (1+ (min line-character-index next-line-length))))))
                 (:sdl-key-home
                  (if (eq modifier :control)
                      (text/first-position text)
                      line-start-position))
                 (:sdl-key-end
                  (if (eq modifier :control)
                      (text/last-position text)
                      line-end-position))
                 (:sdl-key-pageup
                  ;; TODO:
                  (not-yet-implemented))
                 (:sdl-key-pagedown
                  ;; TODO:
                  (not-yet-implemented)))))
            (?a
             (case key
               (:sdl-key-home
                (when (eq modifier :control)
                  (text/first-position text)))
               (:sdl-key-end
                (when (eq modifier :control)
                  (text/last-position text))))))))
    (when new-position
      ;; KLUDGE:
      (when (and (and (eq modifier :control)
                      (member key '(:sdl-key-home :sdl-key-end)))
                 (typep (text/position-element new-position) 'computed-ll))
        (setf (elements-of text) (text/position-element new-position)))
      (bind ((new-character-index (iter (with origin-position = (text/origin-position text))
                                        (for backward-position :initially origin-position :then (when backward-position (text/previous-position text backward-position)))
                                        (for forward-position :initially origin-position :then (when forward-position (text/next-position text forward-position)))
                                        (when (and backward-position (text/position= backward-position new-position))
                                          (return (- distance)))
                                        (when (and forward-position (text/position= forward-position new-position))
                                          (return distance))
                                        (summing 1 :into distance))))
        (make-operation/replace-selection text `((the text/text (text/subseq (the text/text document) ,new-character-index ,new-character-index))))))))

(def function text/read-operation (text gesture)
  (or (gesture-case gesture
        ((gesture/keyboard/key-press :sdl-key-left)
         :domain "Text" :description "Moves the selection one character to the left"
         :operation (text/read-replace-selection-operation text :sdl-key-left))
        ((gesture/keyboard/key-press :sdl-key-right)
         :domain "Text" :description "Moves the selection one character to the right"
         :operation (text/read-replace-selection-operation text :sdl-key-right))
        ((gesture/keyboard/key-press :sdl-key-left :control)
         :domain "Text" :description "Moves the selection one word to the left"
         :operation (text/read-replace-selection-operation text :sdl-key-left :control))
        ((gesture/keyboard/key-press :sdl-key-right :control)
         :domain "Text" :description "Moves the selection one word to the right"
         :operation (text/read-replace-selection-operation text :sdl-key-right :control))
        ((gesture/keyboard/key-press :sdl-key-up)
         :domain "Text" :description "Moves the selection one line up"
         :operation (text/read-replace-selection-operation text :sdl-key-up))
        ((gesture/keyboard/key-press :sdl-key-down)
         :domain "Text" :description "Moves the selection one line down"
         :operation (text/read-replace-selection-operation text :sdl-key-down))
        ((gesture/keyboard/key-press :sdl-key-pageup)
         :domain "Text" :description "Moves the selection one page up"
         :operation (text/read-replace-selection-operation text :sdl-key-pageup))
        ((gesture/keyboard/key-press :sdl-key-pagedown)
         :domain "Text" :description "Moves the selection one page down"
         :operation (text/read-replace-selection-operation text :sdl-key-pagedown))
        ((gesture/keyboard/key-press :sdl-key-home)
         :domain "Text" :description "Moves the selection to the beginning of the line"
         :operation (text/read-replace-selection-operation text :sdl-key-home))
        ((gesture/keyboard/key-press :sdl-key-end)
         :domain "Text" :description "Moves the selection to the end of the line"
         :operation (text/read-replace-selection-operation text :sdl-key-end))
        ((gesture/keyboard/key-press :sdl-key-home :control)
         :domain "Text" :description "Moves the selection to the beginning of the text"
         :operation (text/read-replace-selection-operation text :sdl-key-home :control))
        ((gesture/keyboard/key-press :sdl-key-end :control)
         :domain "Text" :description "Moves the selection to the end of the text"
         :operation (text/read-replace-selection-operation text :sdl-key-end :control))
        ((gesture/keyboard/key-press :sdl-key-delete)
         :domain "Text" :description "Deletes the character following the selection"
         :operation (pattern-case (selection-of text)
                      (((the text/text (text/subseq (the text/text document) ?b ?b)))
                       (when (< ?b (text/length text))
                         (make-operation/sequence/replace-element-range text `((the text/text (text/subseq (the text/text document) ,?b ,(1+ ?b)))) "")))))
        ((gesture/keyboard/key-press :sdl-key-delete :control)
         :domain "Text" :description "Deletes the word following the selection"
         :operation (pattern-case (selection-of text)
                      (((the text/text (text/subseq (the text/text document) ?b ?b)))
                       (bind (((:values element-index character-index) (text/find text (text/element-index text ?b) (text/character-index text ?b) (lambda (c) (not (alphanumericp c))))))
                         (when-bind index (text/index text element-index character-index)
                           (make-operation/sequence/replace-element-range text `((the text/text (text/subseq (the text/text document) ,?b ,index))) ""))))))
        ((gesture/keyboard/key-press :sdl-key-backspace)
         :domain "Text" :description "Deletes the character preceding the selection"
         :operation (pattern-case (selection-of text)
                      (((the text/text (text/subseq (the text/text document) ?b ?b)))
                       (when (> ?b 0)
                         (make-operation/sequence/replace-element-range text `((the text/text (text/subseq (the text/text document) ,(1- ?b) ,?b))) ""))))))
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
                 (((the text/text (text/subseq (the text/text document) ?b ?b)) . ?rest)
                  (make-command gesture
                                (make-operation/sequence/replace-element-range text (selection-of text) replacement)
                                :domain "Text"
                                :description "Inserts a new character at the selection"))))))))
