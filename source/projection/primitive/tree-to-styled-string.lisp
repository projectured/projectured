;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

;; TODO: do we need these providers at all? should we rather always rely on input?
(def (projection e) tree/base->styled-string ()
  ((indentation-provider :type function)
   (delimiter-provider :type function)))

(def (projection e) tree/leaf->styled-string (tree/base->styled-string)
  ())

(def (projection e) tree/node->styled-string (tree/base->styled-string)
  ((separator-provider :type function)))

(def (projection e) t->tree/leaf ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/tree/leaf->styled-string (&key indentation-provider delimiter-provider)
  (make-projection 'tree/leaf->styled-string
                   :indentation-provider indentation-provider
                   :delimiter-provider delimiter-provider))

(def (function e) make-projection/tree/node->styled-string (&key indentation-provider delimiter-provider separator-provider)
  (make-projection 'tree/node->styled-string
                   :indentation-provider indentation-provider
                   :delimiter-provider delimiter-provider
                   :separator-provider separator-provider))

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

(def (macro e) tree/leaf->styled-string (&key indentation-provider delimiter-provider)
  `(make-projection/tree/leaf->styled-string :indentation-provider ,indentation-provider
                                             :delimiter-provider ,delimiter-provider))

(def (macro e) tree/node->styled-string (&key indentation-provider delimiter-provider separator-provider)
  `(make-projection/tree/node->styled-string :indentation-provider ,indentation-provider
                                             :delimiter-provider ,delimiter-provider
                                             :separator-provider ,separator-provider))

;;;;;;
;;; Printer

(def printer t->tree/leaf (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output (make-tree/leaf input)))
    (make-iomap/recursive projection recursion input input-reference output output-reference nil)))

;; TODO: KLUDGE: copied, needs factoring
(def printer tree/leaf->styled-string (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (delimiter-iomaps nil)
         (separator-iomaps nil)
         (indentation-iomaps nil)
         (child-iomaps nil)
         (output-content (bind ((line-index nil)
                                (string-position 0)
                                (element-index 0)
                                (line-position 0)
                                (elements nil))
                           (labels ((write-element (element)
                                      (push element elements)
                                      (incf element-index))
                                    (write-styled-string (element)
                                      (write-element element)
                                      (incf string-position (length (content-of element))))
                                    (next-line (indentation typed-input-reference)
                                      (when line-index
                                        (bind ((content (string #\NewLine)))
                                          (push (make-iomap/string content `(new-line ,typed-input-reference) 0
                                                                   content `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                   1)
                                                indentation-iomaps)
                                          (write-styled-string (make-text/string content :font *font/default* :font-color *color/default*)))
                                        (setf line-position string-position))
                                      (bind ((indentation-string (make-string-of-spaces indentation)))
                                        (unless (string= indentation-string "")
                                          (push (make-iomap/string indentation-string `(indentation ,typed-input-reference ,indentation-string) 0
                                                                   indentation-string `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                   indentation)
                                                indentation-iomaps)
                                          (write-styled-string (make-text/string indentation-string :font *font/default* :font-color *color/default*))))
                                      (if line-index
                                          (incf line-index)
                                          (setf line-index 0)))
                                    (recurse (input input-reference parent-indentation)
                                      (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                                        (awhen (or (opening-delimiter-of input)
                                                   (awhen (delimiter-provider-of projection)
                                                     (funcall it iomap `(opening-delimiter ,typed-input-reference))))
                                          (etypecase it
                                            (string
                                             (push (make-iomap/string it `(opening-delimiter ,typed-input-reference ,it) 0
                                                                      it `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length it))
                                                   delimiter-iomaps)
                                             (write-styled-string (make-text/string it :font *font/default* :font-color *color/default*)))
                                            (text/string
                                             (push (make-iomap/string (content-of it) `(content-of (the text/string (opening-delimiter ,typed-input-reference ,(content-of it)))) 0
                                                                      (content-of it) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length (content-of it)))
                                                   delimiter-iomaps)
                                             (write-styled-string it))))
                                        (bind ((content (content-of input)))
                                          (etypecase content
                                            (string
                                             (iter (for line :in (split-sequence #\NewLine content))
                                                   (unless (first-iteration-p)
                                                     (next-line parent-indentation typed-input-reference))
                                                   (push (make-iomap/string content `(content-of ,typed-input-reference) 0
                                                                            line `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                            (length line))
                                                         child-iomaps)
                                                   (write-styled-string (make-text/string line :font *font/default* :font-color *color/default*))))
                                            (text/string
                                             (push (make-iomap/string (content-of content) `(content-of (the text/string (content-of ,typed-input-reference))) 0
                                                                      (content-of content) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length (content-of content)))
                                                   child-iomaps)
                                             (write-styled-string content))
                                            (image/image
                                             (write-element content))
                                            (text/text
                                                (iter (for element :in-sequence (elements-of content))
                                                      (etypecase element
                                                        (text/string
                                                         (write-styled-string element))
                                                        (image/image
                                                         (write-element element)))))))
                                        (awhen (or (closing-delimiter-of input)
                                                   (awhen (delimiter-provider-of projection)
                                                     (funcall it iomap `(closing-delimiter ,typed-input-reference))))
                                          (etypecase it
                                            (string
                                             (push (make-iomap/string it `(closing-delimiter ,typed-input-reference ,it) 0
                                                                      it `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length it))
                                                   delimiter-iomaps)
                                             (write-styled-string (make-text/string it :font *font/default* :font-color *color/default*)))
                                            (text/string
                                             (push (make-iomap/string (content-of it) `(content-of (the text/string (closing-delimiter ,typed-input-reference ,(content-of it)))) 0
                                                                      (content-of it) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length (content-of it)))
                                                   delimiter-iomaps)
                                             (write-styled-string it)))))))
                             (next-line 0 typed-input-reference)
                             (if input
                                 (recurse input input-reference 0)
                                 (push (make-iomap/object* projection recursion
                                                           input `(the null ,input-reference)
                                                           nil `(the sequence-position (pos (the string ,output-reference) 0)))
                                       child-iomaps))
                             (setf line-index nil)
                             (next-line 0 typed-input-reference)
                             (nreverse elements))))
         (output (make-text/text output-content)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference))
                                  (nreverse indentation-iomaps)
                                  (nreverse separator-iomaps)
                                  (nreverse delimiter-iomaps)
                                  (nreverse child-iomaps)))))

(def printer tree/node->styled-string (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (delimiter-iomaps nil)
         (separator-iomaps nil)
         (indentation-iomaps nil)
         (child-iomaps nil)
         (output-content (bind ((line-index nil)
                                (string-position 0)
                                (element-index 0)
                                (line-position 0)
                                (elements nil))
                           (labels ((write-element (element)
                                      (push element elements)
                                      (incf element-index))
                                    (write-styled-string (element)
                                      (write-element element)
                                      (incf string-position (length (content-of element))))
                                    (next-line (indentation typed-input-reference)
                                      (when line-index
                                        (bind ((content (string #\NewLine)))
                                          (push (make-iomap/string content `(new-line ,typed-input-reference) 0
                                                                   content `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                   1)
                                                indentation-iomaps)
                                          (write-styled-string (make-text/string content :font *font/default* :font-color *color/default*)))
                                        (setf line-position string-position))
                                      (bind ((indentation-string (make-string-of-spaces indentation)))
                                        (unless (string= indentation-string "")
                                          (push (make-iomap/string indentation-string `(indentation ,typed-input-reference ,indentation-string) 0
                                                                   indentation-string `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                   indentation)
                                                indentation-iomaps)
                                          (write-styled-string (make-text/string indentation-string :font *font/default* :font-color *color/default*))))
                                      (if line-index
                                          (incf line-index)
                                          (setf line-index 0)))
                                    (recurse (input input-reference parent-indentation)
                                      (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                                        (awhen (or (opening-delimiter-of input)
                                                   (awhen (delimiter-provider-of projection)
                                                     (funcall it iomap `(opening-delimiter ,typed-input-reference))))
                                          (etypecase it
                                            (string
                                             (push (make-iomap/string it `(opening-delimiter ,typed-input-reference ,it) 0
                                                                      it `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length it))
                                                   delimiter-iomaps)
                                             (write-styled-string (make-text/string it :font *font/default* :font-color *color/default*)))
                                            (text/string
                                             (push (make-iomap/string (content-of it) `(content-of (the text/string (opening-delimiter ,typed-input-reference ,(content-of it)))) 0
                                                                      (content-of it) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length (content-of it)))
                                                   delimiter-iomaps)
                                             (write-styled-string it))))
                                        (etypecase input
                                          (tree/node
                                           (if (expanded-p input)
                                               (iter (with children = (children-of input))
                                                     (for index :from 0)
                                                     (for child :in-sequence children)
                                                     (for child-path = `(elt (the ,(form-type children) (children-of ,typed-input-reference)) ,index))
                                                     (for child-reference = `(the ,(form-type child) ,child-path))
                                                     (for previous-child-reference :previous child-reference)
                                                     (for indentation = (or (indentation-of child)
                                                                            (awhen (indentation-provider-of projection)
                                                                              (funcall it iomap previous-child-reference child-reference input))))
                                                     (unless (first-iteration-p)
                                                       (awhen (or (separator-of input)
                                                                  (awhen (separator-provider-of projection)
                                                                    (funcall it iomap previous-child-reference child-reference)))
                                                         (etypecase it
                                                           (string
                                                            (push (make-iomap/string it `(separator ,previous-child-reference ,child-reference ,it) 0
                                                                                     it `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                                     (length it))
                                                                  separator-iomaps)
                                                            (write-styled-string (make-text/string it :font *font/default* :font-color *color/default*)))
                                                           (text/string
                                                            ;; KLUDGE: unfortunatly iomap/object of later stages can't substitute both arguments of separator, so backward/forward mapping won't work this way
                                                            (push (make-iomap/string (content-of it) `(content-of (the text/string (separator nil #+nil ,previous-child-reference ,child-reference ,(content-of it)))) 0
                                                                                     (content-of it) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                                     (length (content-of it)))
                                                                  separator-iomaps)
                                                            (write-styled-string it)))))
                                                     (when indentation
                                                       (next-line (+ parent-indentation indentation) child-reference))
                                                     (recurse child child-path (- string-position line-position))
                                                     (finally
                                                      ;; TODO: this caused indentation problems in JSON, XML ... delete?
                                                      #+nil
                                                      (when-bind indentation
                                                          (or (indentation-of input)
                                                              (awhen (indentation-provider-of projection)
                                                                (funcall it iomap child-reference nil input)))
                                                        (next-line indentation previous-child-reference))))
                                               (write-styled-string (make-text/string "..." :font *font/default* :font-color *color/default*))))
                                          (tree/leaf
                                           (bind ((content (output-of (recurse-printer recursion iomap (content-of input) `(content-of typed-input-reference) output-reference))))
                                             (etypecase content
                                               (string
                                                (iter (for line :in (split-sequence #\NewLine content))
                                                      (unless (first-iteration-p)
                                                        (next-line parent-indentation typed-input-reference))
                                                      (push (make-iomap/string content `(content-of ,typed-input-reference) 0
                                                                               line `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                               (length line))
                                                            child-iomaps)
                                                      (write-styled-string (make-text/string line :font *font/default* :font-color *color/default*))))
                                               (text/string
                                                (push (make-iomap/string (content-of content) `(content-of (the text/string (content-of ,typed-input-reference))) 0
                                                                         (content-of content) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                         (length (content-of content)))
                                                      child-iomaps)
                                                (write-styled-string content))
                                               (text/text
                                                 (iter (for element :in-sequence (elements-of content))
                                                       (etypecase element
                                                         (text/string
                                                          (write-styled-string element))
                                                         (image/image
                                                          (write-element element)))))
                                               (image/image
                                                (write-element content))))))
                                        (awhen (or (closing-delimiter-of input)
                                                   (awhen (delimiter-provider-of projection)
                                                     (funcall it iomap `(closing-delimiter ,typed-input-reference))))
                                          (etypecase it
                                            (string
                                             (push (make-iomap/string it `(closing-delimiter ,typed-input-reference ,it) 0
                                                                      it `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length it))
                                                   delimiter-iomaps)
                                             (write-styled-string (make-text/string it :font *font/default* :font-color *color/default*)))
                                            (text/string
                                             (push (make-iomap/string (content-of it) `(content-of (the text/string (closing-delimiter ,typed-input-reference ,(content-of it)))) 0
                                                                      (content-of it) `(content-of (the text/string (elt (the list (elements-of (the text/text ,output-reference))) ,element-index))) 0
                                                                      (length (content-of it)))
                                                   delimiter-iomaps)
                                             (write-styled-string it)))))))
                             (next-line 0 typed-input-reference)
                             (if input
                                 (recurse input input-reference 0)
                                 (push (make-iomap/object* projection recursion
                                                           input `(the null ,input-reference)
                                                           nil `(the sequence-position (pos (the string ,output-reference) 0)))
                                       child-iomaps))
                             (setf line-index nil)
                             (next-line 0 typed-input-reference)
                             (nreverse elements))))
         (output (make-text/text output-content)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (append (list (make-iomap/object projection recursion input input-reference output output-reference))
                                  (nreverse indentation-iomaps)
                                  (nreverse separator-iomaps)
                                  (nreverse delimiter-iomaps)
                                  (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def function find-tree-node-reference (reference)
  (pattern-case reference
    ((the sequence-position (pos (the string (?or (content-of (the ?type (?if (eq ?type 'tree/leaf)) ?a))
                                                  ((?or opening-delimiter closing-delimiter) (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)
                                                  (separator (the ?type (?if (subtypep ?type 'tree/base)) ?a)
                                                             (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b)
                                                  (indentation (the ?type (?if (subtypep ?type 'tree/base)) ?a) ?b))) ?c))
     `(the ,?type ,?a))))

(def function find-tree-node-parent-reference (reference)
  (pattern-case reference
    ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
     `(the tree/node ,?a))))

(def reader t->tree/leaf (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader tree/leaf->styled-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader tree/node->styled-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
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
             (bind ((node (eval-reference document node-reference)))
               (when (and (typep node 'tree/node)
                          (> (length (children-of node)) 0))
                 (bind ((first-child (elt (children-of node) 0)))
                   (make-operation/replace-selection document
                                                     (etypecase first-child
                                                       (tree/leaf `(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of ,node-reference)) 0)))) 0)))
                                                       (tree/node `(the sequence-position (pos (the string (opening-delimiter (the tree/node (elt (the list (children-of ,node-reference)) 0)) "(")) 0))))))))))
          ((key-press? latest-gesture :key :sdl-key-left :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             (pattern-case node-reference
               ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                (when (> ?b 0)
                  (bind ((parent-node (eval-reference document ?a))
                         (type (type-of (elt (children-of parent-node) (1- ?b)))))
                    (make-operation/replace-selection document
                                                      (ecase type
                                                        (tree/leaf `(the sequence-position (pos (the string (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))))) 0)))
                                                        (tree/node `(the sequence-position (pos (the string (opening-delimiter (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1- ?b))) "(")) 0)))))))))))
          ((key-press? latest-gesture :key :sdl-key-right :modifier :sdl-key-mod-lalt)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             (pattern-case node-reference
               ((the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node ?a))) ?b))
                (bind ((parent-node (eval-reference document ?a)))
                  (when (< ?b (1- (length (children-of parent-node))))
                    (bind ((type (type-of (elt (children-of parent-node) (1+ ?b)))))
                      (make-operation/replace-selection document
                                                        (ecase type
                                                          (tree/leaf `(the sequence-position (pos (the string (content-of (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) 0)))
                                                          (tree/node `(the sequence-position (pos (the string (opening-delimiter (the ,type (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))) "(")) 0))))))))))))
          ((key-press? latest-gesture :key :sdl-key-delete :modifier :sdl-key-mod-lctrl)
           (when-bind node-reference (find-tree-node-reference (selection-of document))
             (pattern-case node-reference
               ((the ?type (?if (subtypep ?type 'tree/base)) (elt ?a ?b))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                              nil)
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt ,?a ,?b)))) 0))))))
               ((the ?type (?if (subtypep ?type 'tree/base)) (content-of (the document ?a)))
                (make-operation/compound (list (make-operation/replace-content document nil)
                                               (make-operation/replace-selection document `(the null (content-of (the document ,?a))))))))))
          ((key-press? latest-gesture :character #\( :modifiers '(:sdl-key-mod-lshift :sdl-key-mod-lctrl))
           (pattern-case (selection-of document)
             ;; content of a leaf being a child of a node
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            (list (make-tree/node nil)))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (opening-delimiter (the tree/node (elt ,?a ,?b)) "(")) 1))))))
             ;; empty content of a document
             ((the null (content-of (the document ?a)))
              (make-operation/compound (list (make-operation/replace-content document (make-tree/node nil))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (opening-delimiter (the tree/node (content-of (the document document))) "(")) 1))))))))
          ((key-press? latest-gesture :character #\" :modifiers '(:sdl-key-mod-lshift :sdl-key-mod-lctrl))
           (pattern-case (selection-of document)
             ;; content of a leaf being a child of a node
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            (list (make-tree/leaf "")))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt ,?a ,?b)))) 0))))))
             ;; delimiter of a node
             ((the sequence-position (pos (the string (opening-delimiter (the tree/node ?a) "(")) ?b))
              (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq (the list (children-of (the tree/node ,?a))) 0 0)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                            (list (make-tree/leaf "")))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,?a))) 0)))) 0))))))
             ;; empty content of a document
             ((the null (content-of (the document ?a)))
              (make-operation/compound (list (make-operation/replace-content document (make-tree/leaf ""))
                                             (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (content-of (the document document))))) 0))))))))
          ;; move leaf/node up
          ((key-press? latest-gesture :key :sdl-key-u :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ;; content of a leaf being a child of a node that is a child of another node
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)))) ?d))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                              (list value))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt ,?a ,?b)))) ,?d)))))))
             ;; delimiter of a leaf/node being a child of a node that is a child of another node
             ((the sequence-position (pos (the string (?delimiter (?if (member ?delimiter '(opening-delimiter closing-delimiter))) (the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node (elt ?a ?b)))) ?c)) ?d)) ?e))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (elt ,?a ,?b)))) ,?c) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                              (list value))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (,?delimiter (the ,?type (elt ,?a ,?b)) ,?d)) ,?e)))))))
             ;; TODO: indentation of a leaf/node being a child of a node that is a child of another node
             ;; TODO: content of a leaf being a child of a node that is the content of a document
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (content-of (the document ?a))))) ?b)))) ?c))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (content-of (the document ,?a))))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/replace-content document value)
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (content-of (the document ,?a))))) ,?c)))))))
             ;; TODO: delimiter of a leaf/node being a child of a node that is the content of a document
             ((the sequence-position (pos (the string (?delimiter (?if (member ?delimiter '(opening-delimiter closing-delimiter))) (the ?type (?if (subtypep ?type 'tree/base)) (elt (the list (children-of (the tree/node (content-of (the document ?a))))) ?b)) ?c)) ?d))
              (bind ((value (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node (content-of (the document ,?a))))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/replace-content document value)
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (,?delimiter (the ,?type (content-of (the document ,?a))) ,?c)) ,?d)))))))
             ;; TODO: indentation of a leaf/node being a child of a node that is the content of a document
             ))
          ;; wrap leaf/node
          ((key-press? latest-gesture :key :sdl-key-w :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ;; content of a leaf being a child of a node
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (bind ((value (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                              (list (make-tree/node (list value))))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (elt ,?a ,?b)))) 0)))) ,?c)))))))
             ;; delimiter of a leaf/node being a child of a node
             ((the sequence-position (pos (the string (?delimiter (?if (member ?delimiter '(opening-delimiter closing-delimiter))) (the ?type (?if (subtypep ?type 'tree/base)) (elt ?a ?b)) ?c)) ?d))
              (bind ((value (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,?b ,(1+ ?b))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                              (list (make-tree/node (list value))))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (,?delimiter (the ,?type (elt (the list (children-of (the tree/node (elt ,?a ,?b)))) 0)) ,?c)) ,?d)))))))
             ;; TODO: indentation of a leaf/node being a child of a node
             ;; content of a leaf being the content of a document
             ((the sequence-position (pos (the string (content-of (the tree/leaf (content-of (the document ?a))))) ?b))
              (bind ((value (eval-reference document (tree-replace `(content-of (the document ,?a)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/replace-content document (make-tree/node (list value)))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node (content-of (the document ,?a))))) 0)))) ,?b)))))))
             ;; delimiter of a leaf/node being the content of a document
             ((the sequence-position (pos (the string (?delimiter (?if (member ?delimiter '(opening-delimiter closing-delimiter))) (the ?type (?if (subtypep ?type 'tree/base)) (content-of (the document ?a))) ?b)) ?c))
              (bind ((value (eval-reference document (tree-replace `(content-of (the document ,?a)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                (make-operation/compound (list (make-operation/replace-content document (make-tree/node (list value)))
                                               (make-operation/replace-selection document `(the sequence-position (pos (the string (,?delimiter (the ,?type (elt (the list (children-of (the tree/node (content-of (the document ,?a))))) 0)) ,?b)) ,?c)))))))
             ;; TODO: indentation of a leaf/node being the content of a document
             ))
          ;; transpose
          ((key-press? latest-gesture :key :sdl-key-t :modifier :sdl-key-mod-lctrl)
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ?a))) ?b)))) ?c))
              (when (< ?b (1- (length (children-of (eval-reference document ?a)))))
                (bind ((value-1 (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node ,?a))) ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))))
                       (value-2 (eval-reference document (tree-replace `(elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                  (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq (the list (children-of (the tree/node ,?a))) ,?b ,(+ ?b 2))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                (list value-2 value-1))
                                                 (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt (the list (children-of (the tree/node ,?a))) ,(1+ ?b))))) ,?c))))))))))
          ;; reverse transpose
          ((key-press? latest-gesture :key :sdl-key-t :modifiers '(:sdl-key-mod-lshift :sdl-key-mod-lctrl))
           (pattern-case (selection-of document)
             ((the sequence-position (pos (the string (content-of (the tree/leaf (elt ?a ?b)))) ?c))
              (when (> ?b 0)
                (bind ((value-1 (eval-reference document (tree-replace `(elt ,?a ,(1- ?b)) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))))
                       (value-2 (eval-reference document (tree-replace `(elt ,?a ,?b) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document)))))
                  (make-operation/compound (list (make-operation/sequence/replace-element-range document (tree-replace `(the sequence (subseq ,?a ,(- ?b 1) ,(+ ?b 1))) `(the document ,(third (second (input-reference-of projection-iomap)))) '(the document document))
                                                                                                (list value-2 value-1))
                                                 (make-operation/replace-selection document `(the sequence-position (pos (the string (content-of (the tree/leaf (elt ,?a ,(1- ?b))))) ,?c))))))))))
          ;; collapse/expand
          ((key-press? latest-gesture :key :sdl-key-tab :modifier :sdl-key-mod-lctrl)
           (awhen (find-tree-node-parent-reference (find-tree-node-reference (selection-of document)))
             (make-operation/tree/toggle-node document it)))
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
