;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) tree->string ()
  ((delimiter-provider :type function)
   (separator-provider :type function)
   (indentation-provider :type function)))

;;;;;;
;;; Construction

(def (function e) make-projection/tree->string (&key delimiter-provider separator-provider indentation-provider)
  (make-projection 'tree->string
                   :delimiter-provider (or delimiter-provider (make-delimiter-provider "(" ")"))
                   :separator-provider (or separator-provider (make-separator-provider " "))
                   :indentation-provider (or indentation-provider (make-indentation-provider :indentation-width 1 :wrap-from 0 :wrap-last-levels #f))))

(def (function e) make-separator-provider (separator)
  (lambda (iomap previous-child-reference next-child-reference)
    (declare (ignore iomap previous-child-reference next-child-reference))
    separator))

(def (function e) make-delimiter-provider (opening-delimiter closing-delimiter)
  (lambda (iomap reference)
    (declare (ignore iomap))
    (pattern-case reference
      ((?or (opening-delimiter ?node)
            (closing-delimiter ?node))
       (bind ((delimiter (first reference)))
         (ecase delimiter
           (opening-delimiter opening-delimiter)
           (closing-delimiter closing-delimiter)))))))

(def (function e) make-indentation-provider (&key (indentation-width 1) (wrap-from 0) (wrap-last-levels #f))
  (lambda (iomap previous-child-reference next-child-reference parent-node)
    (declare (ignore iomap previous-child-reference))
    (pattern-case next-child-reference
      ((the ?a (elt (the ?type (?if (subtypep ?type 'sequence)) (children-of (the tree/node ?b))) ?c))
       (when (and (> ?c wrap-from)
                  (or wrap-last-levels (some (of-type 'tree/node) (children-of parent-node))))
         indentation-width)))))

;;;;;;
;;; Construction

(def (macro e) tree->string (&key delimiter-provider separator-provider indentation-provider)
  `(make-projection/tree->string :delimiter-provider ,delimiter-provider
                                 :separator-provider ,separator-provider
                                 :indentation-provider ,indentation-provider))

;;;;;;
;;; Printer

(def printer tree->string (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-adjustable-string ""))
         (temporary (with-output-to-string (stream)
                      (bind ((line-index nil)
                             (string-reference nil)
                             (line-position 0))
                        (labels ((next-line (indentation typed-input-reference)
                                   (when line-index
                                     (push (make-iomap/string* nil `(the string (new-line ,typed-input-reference)) 0
                                                               output `(the string ,string-reference) (file-position stream)
                                                               1)
                                           child-iomaps)
                                     (terpri stream)
                                     (setf line-position (file-position stream)))
                                   (bind ((indentation-string (make-string-of-spaces indentation)))
                                     (unless (string= indentation-string "")
                                       (push (make-iomap/string indentation-string `(indentation ,typed-input-reference ,indentation-string) 0
                                                                output string-reference (file-position stream)
                                                                indentation)
                                             child-iomaps)
                                       (write-string indentation-string stream)))
                                   (if line-index
                                       (incf line-index)
                                       (setf line-index 0))
                                   (setf string-reference output-reference))
                                 (recurse (input input-reference parent-indentation)
                                   (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                                     (awhen (or (opening-delimiter-of input)
                                                (funcall (delimiter-provider-of projection) iomap `(opening-delimiter ,typed-input-reference)))
                                       (push (make-iomap/string it `(opening-delimiter ,typed-input-reference ,it) 0
                                                                output string-reference (file-position stream)
                                                                (length it))
                                             child-iomaps)
                                       (write-string it stream))
                                     (etypecase input
                                       (tree/node
                                        (when (expanded-p input)
                                          (iter (with children = (children-of input))
                                                (for index :from 0)
                                                (for child :in-sequence children)
                                                (for child-path = `(elt (the ,(form-type children) (children-of ,typed-input-reference)) ,index))
                                                (for child-reference = `(the ,(form-type child) ,child-path))
                                                (for previous-child-reference :previous child-reference)
                                                (for indentation = (funcall (indentation-provider-of projection) iomap previous-child-reference child-reference input))
                                                (unless (first-iteration-p)
                                                  (awhen (or (separator-of input)
                                                             (funcall (separator-provider-of projection) iomap previous-child-reference child-reference))
                                                    (push (make-iomap/string it `(separator ,previous-child-reference ,child-reference ,it) 0
                                                                             output string-reference (file-position stream)
                                                                             (length it))
                                                          child-iomaps)
                                                    (write-string it stream)))
                                                (when indentation
                                                  (next-line (+ parent-indentation indentation) child-reference))
                                                (recurse child child-path (- (file-position stream) line-position))
                                                (finally
                                                 (when-bind indentation (funcall (indentation-provider-of projection) iomap child-reference nil input)
                                                   (next-line indentation previous-child-reference))))))
                                       (tree/leaf
                                        (push (make-iomap/string* input `(the string (content-of ,typed-input-reference)) 0
                                                                  output `(the string ,string-reference) (file-position stream)
                                                                  (length (content-of input))) child-iomaps)
                                        (iter (for line :in (split-sequence #\NewLine (content-of input)))
                                              (unless (first-iteration-p)
                                                (next-line parent-indentation typed-input-reference))
                                              (write-string line stream))))
                                     (awhen (or (closing-delimiter-of input)
                                                (funcall (delimiter-provider-of projection) iomap `(closing-delimiter ,typed-input-reference)))
                                       (push (make-iomap/string it `(closing-delimiter ,typed-input-reference ,it) 0
                                                                output string-reference (file-position stream)
                                                                (length it))
                                             child-iomaps)
                                       (write-string it stream)))))
                          (next-line 0 typed-input-reference)
                          (recurse input input-reference 0)
                          (setf line-index nil)
                          (next-line 0 typed-input-reference))))))
    (adjust-array output (length temporary))
    (replace output temporary)
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def function find-tree-node-reference (reference)
  (pattern-case reference
    ((the sequence-position (pos (the string (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                                  ((?or opening-delimiter closing-delimiter) (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)
                                                  (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                             (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b))) ?c))
     `(the ,?type ,?a))))

(def function find-tree-node-parent-reference (reference)
  (pattern-case reference
    ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
     `(the tree/node ,?a))))

(def reader tree->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore recursion printer-iomap))
  (bind ((latest-gesture (first (gestures-of gesture-queue))))
    (cond ((typep operation 'operation/sequence/replace-element-range)
           (bind ((tree-reference nil))
             ;; KLUDGE:
             (map-backward projection-iomap (tree-replace (target-of operation) '(the document document) `(the document ,(third (second (output-reference-of projection-iomap)))))
                           (lambda (iomap reference)
                             (declare (ignore iomap))
                             (setf tree-reference reference)))
             (pattern-case tree-reference
               ((the sequence (subseq (the string (opening-delimiter (the tree/node (elt ?a ?b)) ?c)) 0 1))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)) nil)
                                               (make-operation/replace-selection document nil))))
               (?a operation))))
          ((key-press? latest-gesture :key :sdl-key-up :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-parent-reference (find-tree-node-reference (selection-of document)))
             (make-operation/replace-selection document `(the sequence-position (pos (the string (opening-delimiter ,node-reference "(")) 0)))))
          ((key-press? latest-gesture :key :sdl-key-down :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             ;; TODO:
             (make-operation/replace-selection document nil)))
          ((key-press? latest-gesture :key :sdl-key-left :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             ;; TODO:
             (make-operation/replace-selection document nil)))
          ((key-press? latest-gesture :key :sdl-key-right :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             ;; TODO:
             (make-operation/replace-selection document nil)))
          ((key-press? latest-gesture :key :sdl-key-delete :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            nil)
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (elt ,?a ,(1+ ?b))) ,?c))))))))
          ((key-press? latest-gesture :character #\( :modifiers '(:sdl-key-mod-lshift :sdl-key-mod-lctrl))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            (list (make-tree/node nil)))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (elt ,?a ,(1+ ?b))) ,?c))))))))
          ((key-press? latest-gesture :character #\" :modifiers '(:sdl-key-mod-lshift :sdl-key-mod-lctrl))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            (list (make-tree/leaf "")))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt ,?a ,?b)))) 0))))))))
          ((key-press? latest-gesture :key :sdl-key-u :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)))) ?d))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                               (list value))))
             ((the sequence-position (pos (the string ((?or opening-delimiter closing-delimiter) (the tree/node (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)) ?d)) ?e))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                               (list value))))))
          ((key-press? latest-gesture :key :sdl-key-w :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (bind ((value (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                               (list (make-tree/node (list value))))))))
          ((key-press? latest-gesture :key :sdl-key-t :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (bind ((value-1 (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))))
                     (value-2 (eval-reference document (tree-replace `(elt ,?a ,(1+ ?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(+ ?b 2))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                               (list value-2 value-1))))))
          ((and (key-press? latest-gesture :key :sdl-key-i :modifier :sdl-key-mod-lctrl)
                (typep (indentation-provider-of projection) 'alternative-function))
           (make-operation/select-next-alternative (indentation-provider-of projection)))
          ((and (key-press? latest-gesture :key :sdl-key-s :modifier :sdl-key-mod-lctrl)
                (typep (separator-provider-of projection) 'alternative-function))
           (make-operation/select-next-alternative (separator-provider-of projection)))
          ((and (key-press? latest-gesture :key :sdl-key-d :modifier :sdl-key-mod-lctrl)
                (typep (delimiter-provider-of projection) 'alternative-function))
           (make-operation/select-next-alternative (delimiter-provider-of projection)))
          (t operation))))
