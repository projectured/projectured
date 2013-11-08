;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;; TODO: backward mapping is not completely correct due to the following:
;;;; TODO: "[" "null" "]" the positions between the delimiters and the content are ambiguous
;;;; TODO: there's no easy way around this problem currently, because the same cursor positions can be represented in multiple ways
;;;; TODO: what if we explicitely represent cursor positions too in the text domain?

;;;;;;
;;; Projection

(def projection json/nothing->tree/leaf ()
  ())

(def projection json/null->tree/leaf ()
  ())

(def projection json/boolean->tree/leaf ()
  ())

(def projection json/number->tree/leaf ()
  ())

(def projection json/string->tree/leaf ()
  ())

(def projection json/array->tree/node ()
  ())

(def projection json/object-entry->tree/node ()
  ())

(def projection json/object->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/json/nothing->tree/leaf ()
  (make-projection 'json/nothing->tree/leaf))

(def (function e) make-projection/json/null->tree/leaf ()
  (make-projection 'json/null->tree/leaf))

(def (function e) make-projection/json/boolean->tree/leaf ()
  (make-projection 'json/boolean->tree/leaf))

(def (function e) make-projection/json/number->tree/leaf ()
  (make-projection 'json/number->tree/leaf))

(def (function e) make-projection/json/string->tree/leaf ()
  (make-projection 'json/string->tree/leaf))

(def (function e) make-projection/json/array->tree/node ()
  (make-projection 'json/array->tree/node))

(def (function e) make-projection/json/object-entry->tree/node ()
  (make-projection 'json/object-entry->tree/node))

(def (function e) make-projection/json/object->tree/node ()
  (make-projection 'json/object->tree/node))

;;;;;;
;;; Construction

(def (macro e) json/nothing->tree/leaf ()
  '(make-projection/json/nothing->tree/leaf))

(def (macro e) json/null->tree/leaf ()
  '(make-projection/json/null->tree/leaf))

(def (macro e) json/boolean->tree/leaf ()
  '(make-projection/json/boolean->tree/leaf))

(def (macro e) json/number->tree/leaf ()
  '(make-projection/json/number->tree/leaf))

(def (macro e) json/string->tree/leaf ()
  '(make-projection/json/string->tree/leaf))

(def (macro e) json/array->tree/node ()
  '(make-projection/json/array->tree/node))

(def (macro e) json/object-entry->tree/node ()
  '(make-projection/json/object-entry->tree/node))

(def (macro e) json/object->tree/node ()
  '(make-projection/json/object->tree/node))

;;;;;;
;;; Printer

;; TODO: rename and move
(def function make-iomap/string->string (projection recursion input input-reference output output-reference string-content string-input-reference string-output-reference)
  (make-iomap/compound projection recursion input input-reference output output-reference
                       (list (make-iomap/recursive projection recursion input input-reference output output-reference)
                             (make-iomap/string string-content string-input-reference 0
                                                string-content string-output-reference 0
                                                (length string-content)))))

;; TODO: rename and move
(def function make-iomap/string->tree/leaf (projection recursion input input-reference output output-reference string-content string-input-reference)
  (make-iomap/string->string projection recursion input input-reference output output-reference
                             string-content string-input-reference `(content-of (the text/string (content-of (the tree/leaf ,output-reference))))))

(def printer json/nothing->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((string-content "")
         (output (make-tree/leaf (make-text/string string-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
    (make-iomap/string->tree/leaf projection recursion input input-reference output output-reference
                                  string-content `(value (the ,(form-type input) ,input-reference)))))

(def printer json/null->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((string-content "null")
         (output (make-tree/leaf (make-text/string string-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
    (make-iomap/string->tree/leaf projection recursion input input-reference output output-reference
                                  string-content `(value (the ,(form-type input) ,input-reference)))))

(def printer json/boolean->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((string-content (boolean-to-string (value-p input)))
         (output (make-tree/leaf (make-text/string string-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*))))
    (make-iomap/string->tree/leaf projection recursion input input-reference output output-reference
                                  string-content `(boolean-to-string (the boolean (value-p (the ,(form-type input) ,input-reference)))))))

(def printer json/number->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((string-content (write-to-string (value-of input)))
         (output (make-tree/leaf (make-text/string string-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*))))
    (make-iomap/string->tree/leaf projection recursion input input-reference output output-reference
                                  string-content `(write-to-string (the number (value-of (the ,(form-type input) ,input-reference)))))))

(def printer json/string->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((string-content (value-of input))
         (output (make-tree/leaf (make-text/string string-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                 :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/string->tree/leaf projection recursion input input-reference output output-reference
                                  string-content `(value-of (the ,(form-type input) ,input-reference)))))

(def printer json/array->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (deep-array (find-if-not (of-type '(or json/nothing json/null json/boolean json/number json/string)) (elements-of input)))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push element-iomap child-iomaps)
                                       (when (and deep-array (not (first-iteration-p)))
                                         ;; KLUDGE:
                                         (setf (indentation-of (output-of element-iomap)) 1))
                                       (collect (output-of element-iomap)))
                                 :opening-delimiter (make-text/string "[" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "]" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :indentation (indentation-of input))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/recursive projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer json/object-entry->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (key (key-of input))
         (iomap (recurse-printer recursion iomap (value-of input)
                                 `(value-of ,typed-input-reference)
                                 `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
         (key-node-reference `(elt (the list (children-of (the tree/node ,output-reference))) 0))
         (output (make-tree/node (list (make-tree/leaf (make-text/string key :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)
                                                       :opening-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :closing-delimiter (make-text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                       (output-of iomap))
                                 :separator (make-text/string " : " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :opening-delimiter (make-text/string "" :font *font/default* :font-color *color/default*)
                                 :closing-delimiter (make-text/string "" :font *font/default* :font-color *color/default*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list (make-iomap/object projection recursion input input-reference output output-reference)
                               iomap
                               (make-iomap/object* projection recursion input `(the string (key-of ,typed-input-reference))
                                                   key `(the string (content-of (the tree/leaf ,key-node-reference))))
                               (make-iomap/string key `(key-of ,typed-input-reference) 0
                                                  key `(content-of (the text/string (content-of (the tree/leaf ,key-node-reference)))) 0
                                                  (length key))))))

(def printer json/object->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for entry :in-sequence (entries-of input))
                                       (for entry-iomap = (recurse-printer recursion iomap entry
                                                                           `(elt (the list (entries-of ,typed-input-reference)) ,index)
                                                                           `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push entry-iomap child-iomaps)
                                       (unless (first-iteration-p)
                                         ;; KLUDGE:
                                         (setf (indentation-of (output-of entry-iomap)) 1))
                                       (collect (output-of entry-iomap)))
                                 :opening-delimiter (make-text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-text/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output output-reference
                         (list* (make-iomap/recursive projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

;; TODO: rename and move
(def function make-operation/replace (document reference replacement &optional (list-replacement (list replacement)))
  (pattern-case reference
    ((content-of (the document ?a))
     (make-operation/replace-content document replacement))
    ((elt (the list ?a) ?b)
     (make-operation/sequence/replace-element-range document reference list-replacement))
    (?a
     (make-operation/replace-target document reference replacement))))

;; TODO: rename and move
(def function make-operation/replace-foo (document replacement-reference selection-reference replacement &optional (list-replacement (list replacement)))
  (make-operation/compound (list (make-operation/replace document replacement-reference replacement list-replacement)
                                 (make-operation/replace-selection document selection-reference))))

(def function json/read-opeartion (projection-iomap gesture-queue document-iomap)
  (bind ((latest-gesture (first-elt (gestures-of gesture-queue)))
         (document (input-of document-iomap)))
    (cond ((key-press? latest-gesture :key :sdl-key-delete)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (value (the json/nothing ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))) 0))
                                       (json/nothing) nil))
          ((key-press? latest-gesture :character #\n)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (value (the json/null ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))) 0))
                                       (json/null)))
          ((key-press? latest-gesture :character #\f)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the json/boolean ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))))) 0))
                                       (json/boolean #f)))
          ((key-press? latest-gesture :character #\t)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (boolean-to-string (the boolean (value-p (the json/boolean ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))))) 0))
                                       (json/boolean #t)))
          ((and (typep latest-gesture 'gesture/keyboard/key-press)
                (digit-char-p (character-of latest-gesture)))
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (write-to-string (the number (value-of (the json/number ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))))) 0))
                                       (json/number (parse-integer (string (character-of latest-gesture))))))
          ((key-press? latest-gesture :character #\")
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       `(the sequence-position (pos (the string (value-of (the json/string ,(tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))) 0))
                                       (json/string "")))
          ((key-press? latest-gesture :character #\[)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       nil
                                       (json/array)))
          ((key-press? latest-gesture :character #\{)
           (make-operation/replace-foo document
                                       (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)
                                       nil
                                       (json/object)))
          ((key-press? latest-gesture :character #\,)
           (bind ((target (tree-replace (input-reference-of projection-iomap) (input-reference-of document-iomap) 'document)))
             (pattern-case target
               ((elt (the list ?a) ?b)
                (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the list ,?a) ,?b ,?b)) (list (json/nothing)))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (value (the json/nothing (elt (the list ,?a) ,?b)))) 0))))))
               ((value-of (the json/object-entry (elt (the list ?a) ?b)))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document `(the sequence (subseq (the list ,?a) ,?b ,?b)) (list (json/object-entry "" (json/nothing)))))))))))))

(def reader json/nothing->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (or (json/read-opeartion projection-iomap gesture-queue document-iomap)
      (operation/read-backward operation projection-iomap document-iomap)))

(def reader json/null->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (or (json/read-opeartion projection-iomap gesture-queue document-iomap)
      (operation/read-backward operation projection-iomap document-iomap)))

(def reader json/boolean->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (or (json/read-opeartion projection-iomap gesture-queue document-iomap)
      (operation/read-backward operation projection-iomap document-iomap)))

(def reader json/number->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (labels ((recurse (child-operation)
             (cond ((typep child-operation 'operation/compound)
                    (bind ((child-operations (mapcar #'recurse (elements-of child-operation))))
                      (unless (some 'null child-operations)
                        (make-operation/compound child-operations))))
                   ((typep child-operation 'operation/replace-selection)
                    (awhen (map-backward/clever (selection-of child-operation) projection-iomap document-iomap)
                      (make-operation/replace-selection (input-of document-iomap) (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)))))
                   ((and (typep child-operation 'operation/sequence/replace-element-range)
                         (every 'digit-char-p (replacement-of child-operation)))
                    (awhen (map-backward/clever (target-of child-operation) projection-iomap document-iomap)
                      (make-operation/number/replace-range (input-of document-iomap) (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation)))))))
    (or (recurse operation)
        (json/read-opeartion projection-iomap gesture-queue document-iomap)
        (operation/read-backward operation projection-iomap document-iomap))))

(def reader json/string->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection recursion printer-iomap))
  (labels ((recurse (child-operation)
             (cond ((typep child-operation 'operation/compound)
                    (bind ((child-operations (mapcar #'recurse (elements-of child-operation))))
                      (unless (some 'null child-operations)
                        (make-operation/compound child-operations))))
                   ((typep child-operation 'operation/replace-selection)
                    (awhen (map-backward/clever (selection-of child-operation) projection-iomap document-iomap)
                      (make-operation/replace-selection (input-of document-iomap) (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)))))
                   ((typep child-operation 'operation/sequence/replace-element-range)
                    (awhen (map-backward/clever (target-of child-operation) projection-iomap document-iomap)
                      (make-operation/sequence/replace-element-range (input-of document-iomap) (tree-replace it `(the document ,(input-reference-of document-iomap)) '(the document document)) (replacement-of child-operation)))))))
    (or (recurse operation)
        (json/read-opeartion projection-iomap gesture-queue document-iomap)
        (operation/read-backward operation projection-iomap document-iomap))))

(def reader json/array->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (bind ((selection (tree-replace (selection-of (input-of document-iomap)) '(the document document) `(the document ,(input-reference-of document-iomap)))))
    (or (iter (for index :from 0 :below (length (elements-of (input-of projection-iomap))))
              (for child-iomap = (elt (child-iomaps-of projection-iomap) (1+ index)))
              (for child-operation =
                   (when (tree-search selection (input-reference-of child-iomap))
                     (recurse-reader recursion printer-iomap child-iomap gesture-queue operation document-iomap)))
              (when child-operation
                (return child-operation)))
        (json/read-opeartion projection-iomap gesture-queue document-iomap)
        (operation/read-backward operation projection-iomap document-iomap))))

(def reader json/object-entry->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation document-iomap))

(def reader json/object->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection))
  (bind ((selection (tree-replace (selection-of (input-of document-iomap)) '(the document document) `(the document ,(input-reference-of document-iomap)))))
    (or (iter (for index :from 0 :below (length (entries-of (input-of projection-iomap))))
              (for child-iomap = (elt (child-iomaps-of projection-iomap) (1+ index)))
              (for child-operation =
                   (when (tree-search selection (input-reference-of child-iomap))
                     (recurse-reader recursion printer-iomap child-iomap gesture-queue operation document-iomap)))
              (when child-operation
                (return child-operation)))
        (json/read-opeartion projection-iomap gesture-queue document-iomap)
        (operation/read-backward operation projection-iomap document-iomap))))
