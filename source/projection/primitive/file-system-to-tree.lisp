;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection file-system/file->tree/leaf ()
  ())

(def projection file-system/directory->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/file-system/file->tree/leaf ()
  (make-projection 'file-system/file->tree/leaf))

(def (function e) make-projection/file-system/directory->tree/node ()
  (make-projection 'file-system/directory->tree/node))

;;;;;;
;;; Construction

(def (macro e) file-system/file->tree/leaf ()
  '(make-projection/file-system/file->tree/leaf))

(def (macro e) file-system/directory->tree/node ()
  '(make-projection/file-system/directory->tree/node))

;;;;;;
;;; Printer

(def printer file-system/file->tree/leaf (projection recursion input input-reference)
  (bind ((pathname (pathname-of input))
         (output-selection (pattern-case (selection-of input)
                             (((the string (subseq (the string document) ?start-index ?end-index))
                               (the string (file-namestring (the pathname document)))
                               (the pathname (pathname-of (the file-system/file document))))
                              `((the text/text (text/subseq (the text/text document) ,?start-index ,?end-index))
                                (the text/text (content-of (the tree/leaf document)))))))
         (output (tree/leaf (:selection output-selection)
                   (text/text (:selection (butlast output-selection 1))
                     ;; TODO: icon
                     (image/image () (resource-pathname "image/file.png"))
                     (text/string (file-namestring pathname) :font *font/ubuntu/regular/18* :font-color *color/solarized/blue*)))))
    (make-iomap/compound projection recursion input input-reference output nil)))

(def printer file-system/directory->tree/node (projection recursion input input-reference)
  (bind ((pathname (pathname-of input))
         (element-iomaps (iter (for index :from 0)
                               (for element :in-sequence (elements-of input))
                               (for element-iomap = (recurse-printer recursion element
                                                                     `((elt (the sequence (elements-of document)) ,index)
                                                                       ,@(typed-reference (form-type input) input-reference))))
                               (setf (indentation-of (output-of element-iomap)) 5)
                               (collect element-iomap)))
         (output-selection (pattern-case (reverse (selection-of input))
                             (((the sequence (elements-of (the file-system/directory document)))
                               (the ?type (elt (the sequence document) ?index))
                               . ?rest)
                              (bind ((element-iomap (elt element-iomaps ?index))
                                     (element-output (output-of element-iomap)))
                                (append (selection-of element-output)
                                        `((the ,(form-type element-output) (elt (the sequence document) ,(1+ ?index)))
                                          (the sequence (children-of (the tree/node document)))))))))
         (output (make-tree/node (list* (tree/leaf (:selection (butlast output-selection 2))
                                          (text/text ()
                                            ;; TODO: icon
                                            (image/image () (resource-pathname "image/directory.png"))
                                            (text/spacing 5 :unit :pixel)
                                            (text/string (last-elt (pathname-directory pathname)) :font *font/ubuntu/regular/18* :font-color *color/solarized/red*)))
                                        (mapcar 'output-of element-iomaps))
                                 :selection output-selection)))
    (make-iomap/compound projection recursion input input-reference output element-iomaps)))

;;;;;;
;;; Reader

(def reader file-system/file->tree/leaf (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (selection-of operation)
                                                  (((the text/text (text/subseq (the text/text document) ?start-index ?end-index))
                                                    (the text/text (content-of (the tree/leaf document))))
                                                   `((the string (subseq (the string document) ,?start-index ,?end-index))
                                                     (the string (file-namestring (the pathname document)))
                                                     (the pathname (pathname-of (the file-system/file document)))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/node (printer-output (the file-system/file document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-o :control)
                       :domain "File System" :description "Opens the selected file"
                       :operation (bind ((pathname (pathname-of printer-input))
                                         (document (deserialize-document pathname))
                                         (open-target (open-target-of printer-input)))
                                    (make-operation/functional (lambda ()
                                                                 (appendf (selector-element-pairs-of open-target)
                                                                          (list (list (widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
                                                                                        (text/text ()
                                                                                          (text/string (file-namestring pathname) :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
                                                                                      (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1075 689) :margin (make-inset :all 5))
                                                                                        (document/document (:filename pathname)
                                                                                          document))))))))))
                    input)))

(def reader file-system/directory->tree/node (projection recursion input printer-iomap)
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (awhen (labels ((recurse (operation)
                                      (typecase operation
                                        (operation/quit operation)
                                        (operation/functional operation)
                                        (operation/replace-selection
                                         (awhen (pattern-case (reverse (selection-of operation))
                                                  (((the sequence (children-of (the tree/node document)))
                                                    (the ?child-type (elt (the sequence document) ?child-index))
                                                    . ?rest)
                                                   (bind ((element (elt (elements-of printer-input) (1- ?child-index)))
                                                          (input-operation (make-operation/replace-selection element (reverse ?rest)))
                                                          (output-operation (operation-of (recurse-reader recursion (make-command (gesture-of input) input-operation :domain (domain-of input) :description (description-of input)) (elt (child-iomaps-of printer-iomap) (1- ?child-index))))))
                                                     (append (selection-of output-operation)
                                                             `((the file-system/file (elt (the sequence document) ,(1- ?child-index)))
                                                               (the sequence (elements-of (the file-system/directory document)))))))
                                                  (?a
                                                   (append (selection-of operation) `((the tree/node (printer-output (the file-system/directory document) ,projection ,recursion))))))
                                           (make-operation/replace-selection printer-input it)))
                                        (operation/compound
                                         (bind ((operations (mapcar #'recurse (elements-of operation))))
                                           (unless (some 'null operations)
                                             (make-operation/compound operations)))))))
                             (recurse (operation-of input)))
                      (make-command (gesture-of input) it
                                    :domain (domain-of input)
                                    :description (description-of input)))
                    (pattern-case (reverse (selection-of printer-input))
                      (((the sequence (elements-of (the file-system/directory document)))
                        (the ?type (elt (the sequence document) ?index))
                        . ?rest)
                       (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) ?index))))
                    input)))
