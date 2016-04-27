;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection workbench/workbench->widget/shell ()
  ())

(def projection workbench/page->widget/tabbed-pane ()
  ())

(def projection workbench/navigator->widget/scroll-pane ()
  ())

(def projection workbench/console->widget/scroll-pane ()
  ())

(def projection workbench/descriptor->widget/scroll-pane ()
  ())

(def projection workbench/operator->widget/scroll-pane ()
  ())

(def projection workbench/searcher->widget/scroll-pane ()
  ())

(def projection workbench/evaluator->widget/scroll-pane ()
  ())

(def projection workbench/document->widget/scroll-pane ()
  ())

;;;;;;
;;; Construction

(def function make-projection/workbench/workbench->widget/shell ()
  (make-projection 'workbench/workbench->widget/shell))

(def function make-projection/workbench/page->widget/tabbed-pane ()
  (make-projection 'workbench/page->widget/tabbed-pane))

(def function make-projection/workbench/navigator->widget/scroll-pane ()
  (make-projection 'workbench/navigator->widget/scroll-pane))

(def function make-projection/workbench/console->widget/scroll-pane ()
  (make-projection 'workbench/console->widget/scroll-pane))

(def function make-projection/workbench/descriptor->widget/scroll-pane ()
  (make-projection 'workbench/descriptor->widget/scroll-pane))

(def function make-projection/workbench/operator->widget/scroll-pane ()
  (make-projection 'workbench/operator->widget/scroll-pane))

(def function make-projection/workbench/searcher->widget/scroll-pane ()
  (make-projection 'workbench/searcher->widget/scroll-pane))

(def function make-projection/workbench/evaluator->widget/scroll-pane ()
  (make-projection 'workbench/evaluator->widget/scroll-pane))

(def function make-projection/workbench/document->widget/scroll-pane ()
  (make-projection 'workbench/document->widget/scroll-pane))

;;;;;;
;;; Construction

(def macro workbench/workbench->widget/shell ()
  '(make-projection/workbench/workbench->widget/shell))

(def macro workbench/page->widget/tabbed-pane ()
  '(make-projection/workbench/page->widget/tabbed-pane))

(def macro workbench/navigator->widget/scroll-pane ()
  '(make-projection/workbench/navigator->widget/scroll-pane))

(def macro workbench/console->widget/scroll-pane ()
  '(make-projection/workbench/console->widget/scroll-pane))

(def macro workbench/descriptor->widget/scroll-pane ()
  '(make-projection/workbench/descriptor->widget/scroll-pane))

(def macro workbench/operator->widget/scroll-pane ()
  '(make-projection/workbench/operator->widget/scroll-pane))

(def macro workbench/searcher->widget/scroll-pane ()
  '(make-projection/workbench/searcher->widget/scroll-pane))

(def macro workbench/evaluator->widget/scroll-pane ()
  '(make-projection/workbench/evaluator->widget/scroll-pane))

(def macro workbench/document->widget/scroll-pane ()
  '(make-projection/workbench/document->widget/scroll-pane))

;;;;;;
;;; Forward mapper

(def forward-mapper workbench/workbench->widget/shell ()
  (reference-case -reference-
    (((the workbench/page (navigation-page-of (the workbench/workbench document)))
      . ?rest)
     (values `((the widget/split-pane (content-of (the widget/shell document)))
               (the sequence (elements-of (the widget/split-pane document)))
               (the widget/tabbed-pane (elt (the sequence document) 0)))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 0)))
    (((the workbench/page (editing-page-of (the workbench/workbench document)))
      . ?rest)
     (values `((the widget/split-pane (content-of (the widget/shell document)))
               (the sequence (elements-of (the widget/split-pane document)))
               (the widget/split-pane (elt (the sequence document) 1))
               (the sequence (elements-of (the widget/split-pane document)))
               (the widget/tabbed-pane (elt (the sequence document) 0)))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 1)))
    (((the workbench/page (information-page-of (the workbench/workbench document)))
      . ?rest)
     (values `((the widget/split-pane (content-of (the widget/shell document)))
               (the sequence (elements-of (the widget/split-pane document)))
               (the widget/split-pane (elt (the sequence document) 1))
               (the sequence (elements-of (the widget/split-pane document)))
               (the widget/tabbed-pane (elt (the sequence document) 1)))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 2)))))

(def forward-mapper workbench/page->widget/tabbed-pane ()
  (reference-case -reference-
    (((the sequence (elements-of (the workbench/page document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
                 (the sequence (elt (the sequence document) ,?index))
                 (the ,(document-type (output-of element-iomap)) (elt (the sequence document) 1)))
               ?rest
               element-iomap)))))

(def forward-mapper workbench/navigator->widget/scroll-pane ()
  (reference-case -reference-
    (((the sequence (folders-of (the workbench/navigator document)))
      (the file-system/directory (elt (the sequence document) ?index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the widget/composite (content-of (the widget/scroll-pane document)))
                 (the sequence (elements-of (the widget/composite document)))
                 (the ,(document-type (output-of child-iomap)) (elt (the sequence document) ,?index)))
               ?rest
               child-iomap)))))

(def forward-mapper workbench/console->widget/scroll-pane ()
  nil)

(def forward-mapper workbench/descriptor->widget/scroll-pane ()
  nil)

(def forward-mapper workbench/operator->widget/scroll-pane ()
  nil)

(def forward-mapper workbench/searcher->widget/scroll-pane ()
  nil)

(def forward-mapper workbench/evaluator->widget/scroll-pane ()
  (reference-case -reference-
    (((the evaluator/toplevel (content-of (the workbench/evaluator document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (values `((the ,(document-type (output-of content-iomap)) (content-of (the widget/scroll-pane document))))
               ?rest
               content-iomap)))))

(def forward-mapper workbench/document->widget/scroll-pane ()
  (reference-case -reference-
    (((the ?type (content-of (the workbench/document document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (values `((the ,(document-type (output-of content-iomap)) (content-of (the widget/scroll-pane document))))
               ?rest
               content-iomap)))))

;;;;;;
;;; Backward mapper

(def backward-mapper workbench/workbench->widget/shell ()
  (reference-case -reference-
    (((the widget/split-pane (content-of (the widget/shell document)))
      (the sequence (elements-of (the widget/split-pane document)))
      (the widget/tabbed-pane (elt (the sequence document) 0))
      . ?rest)
     (values '((the workbench/page (navigation-page-of (the workbench/workbench document))))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 0)))
    (((the widget/split-pane (content-of (the widget/shell document)))
      (the sequence (elements-of (the widget/split-pane document)))
      (the widget/split-pane (elt (the sequence document) 1))
      (the sequence (elements-of (the widget/split-pane document)))
      (the widget/tabbed-pane (elt (the sequence document) 0))
      . ?rest)
     (values '((the workbench/page (editing-page-of (the workbench/workbench document))))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 1)))
    (((the widget/split-pane (content-of (the widget/shell document)))
      (the sequence (elements-of (the widget/split-pane document)))
      (the widget/split-pane (elt (the sequence document) 1))
      (the sequence (elements-of (the widget/split-pane document)))
      (the widget/tabbed-pane (elt (the sequence document) 1))
      . ?rest)
     (values '((the workbench/page (information-page-of (the workbench/workbench document))))
             ?rest
             (elt (child-iomaps-of -printer-iomap-) 2)))))

(def backward-mapper workbench/page->widget/tabbed-pane ()
  (reference-case -reference-
    (((the sequence (selector-element-pairs-of (the widget/tabbed-pane document)))
      (the sequence (elt (the sequence document) ?index))
      (the ?type (elt (the sequence document) 1))
      . ?rest)
     (bind ((element-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (elements-of (the workbench/page document)))
                 (the ,(document-type (input-of element-iomap)) (elt (the sequence document) ,?index)))
               ?rest
               element-iomap)))))

(def backward-mapper workbench/navigator->widget/scroll-pane ()
  (reference-case -reference-
    (((the widget/composite (content-of (the widget/scroll-pane document)))
      (the sequence (elements-of (the widget/composite document)))
      (the ?type (elt (the sequence document) ?index))
      . ?rest)
     (bind ((child-iomap (elt (child-iomaps-of -printer-iomap-) ?index)))
       (values `((the sequence (folders-of (the workbench/navigator document)))
                 (the ,(document-type (input-of child-iomap)) (elt (the sequence document) ,?index)))
               ?rest
               child-iomap)))))

(def backward-mapper workbench/console->widget/scroll-pane ()
  nil)

(def backward-mapper workbench/descriptor->widget/scroll-pane ()
  nil)

(def backward-mapper workbench/operator->widget/scroll-pane ()
  nil)

(def backward-mapper workbench/searcher->widget/scroll-pane ()
  nil)

(def backward-mapper workbench/evaluator->widget/scroll-pane ()
  (reference-case -reference-
    (((the ?type (content-of (the widget/scroll-pane document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (values `((the ,(document-type (input-of content-iomap)) (content-of (the workbench/evaluator document))))
               ?rest
               content-iomap)))))

(def backward-mapper workbench/document->widget/scroll-pane ()
  (reference-case -reference-
    (((the ?type (content-of (the widget/scroll-pane document)))
      . ?rest)
     (bind ((content-iomap (content-iomap-of -printer-iomap-)))
       (values `((the ,(document-type (input-of content-iomap)) (content-of (the workbench/document document))))
               ?rest
               content-iomap)))))

;;;;;;
;;; Printer

(def printer workbench/workbench->widget/shell ()
  (bind ((navigation-page-iomap (as (recurse-printer -recursion- (navigation-page-of -input-)
                                                     `((navigation-page-of (the workbench/workbench document))
                                                       ,@(typed-reference (document-type -input-) -input-reference-)))))
         (editing-page-iomap (as (recurse-printer -recursion- (editing-page-of -input-)
                                                   `((editing-page-of (the workbench/workbench document))
                                                     ,@(typed-reference (document-type -input-) -input-reference-)))))
         (information-page-iomap (as (recurse-printer -recursion- (information-page-of -input-)
                                                      `((information-page-of (the workbench/workbench document))
                                                        ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (widget/shell (:size (make-2d 1280 720)
                                :border (make-inset :all 5)
                                :border-color *color/solarized/background/lighter*
                                :selection (as (subseq* (take-cc (va output-selection) 1) 0 1)))
                   (widget/split-pane (:orientation :horizontal :selection (as (subseq* (take-cc (va output-selection) 3) 1 3)))
                     (as (output-of (va navigation-page-iomap)))
                     (widget/split-pane (:orientation :vertical :selection (as (subseq* (take-cc (va output-selection) 5) 3 5)))
                       (as (output-of (va editing-page-iomap)))
                       (as (output-of (va information-page-iomap))))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output (as (list (va navigation-page-iomap) (va editing-page-iomap) (va information-page-iomap))))))

(def printer workbench/page->widget/tabbed-pane ()
  (bind ((document-iomaps (as (iter (for index :from 0)
                                    (for document :in-sequence (elements-of -input-))
                                    (collect (recurse-printer -recursion- document
                                                              `((elt (the sequence document) ,index)
                                                                (the sequence (elements-of (the workbench/page document)))
                                                                ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (make-widget/tabbed-pane (as (iter (for document :in-sequence (elements-of -input-))
                                                    (for document-iomap :in (va document-iomaps))
                                                    (collect (list (text/text () (text/string (title-of document) :font *font/liberation/sans/regular/24* :font-color *color/black*))
                                                                   (output-of document-iomap)))))
                                          :border (make-inset :all 5)
                                          :border-color *color/solarized/content/lighter*
                                          :selection (as (subseq* (take-cc (va output-selection) 3) 0 3)))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output document-iomaps)))

(def printer workbench/navigator->widget/scroll-pane ()
  (bind ((folder-iomaps (as (iter (for index :from 0)
                                  (for folder :in-sequence (folders-of -input-))
                                  (collect (recurse-printer -recursion- folder
                                                            `((elt (the sequence document) ,index)
                                                              (the sequence (folders-of (the workbench/navigator document)))
                                                              ,@(typed-reference (document-type -input-) -input-reference-)))))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (widget/scroll-pane (:size (make-2d 224 655)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*
                                      :selection (as (subseq* (take-cc (va output-selection) 1) 0 1)))
                   (make-widget/composite 0 (as (mapcar 'output-of (va folder-iomaps)))
                                          :selection (as (subseq* (take-cc (va output-selection) 3) 1 3))))))
    (make-iomap/compound -projection- -recursion- -input- -input-reference- output folder-iomaps)))

(def printer workbench/console->widget/scroll-pane ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the workbench/document document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (widget/scroll-pane (:size (make-2d 1000 130)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*
                                      :selection (as (subseq* (take-cc (va output-selection) 1) 0 1)))
                   (as (output-of (va content-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer workbench/descriptor->widget/scroll-pane ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the workbench/document document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output (widget/scroll-pane (:size (make-2d 1000 130)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*)
                   (as (output-of (va content-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer workbench/operator->widget/scroll-pane ()
  (bind ((output (widget/scroll-pane (:size (make-2d 1000 130)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*)
                   (make-graphics/canvas nil 0))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output nil)))

(def printer workbench/searcher->widget/scroll-pane ()
  (bind ((output (widget/scroll-pane (:size (make-2d 1000 130)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*)
                   (make-graphics/canvas nil 0))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output nil)))

(def printer workbench/evaluator->widget/scroll-pane ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the workbench/document document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (widget/scroll-pane (:size (make-2d 1000 130)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*
                                      :selection (as (subseq* (take-cc (va output-selection) 1) 0 1)))
                   (as (output-of (va content-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def printer workbench/document->widget/scroll-pane ()
  (bind ((content-iomap (as (recurse-printer -recursion- (content-of -input-)
                                             `((content-of (the workbench/document document))
                                               ,@(typed-reference (document-type -input-) -input-reference-)))))
         (output-selection (as (print-selection -printer-iomap-)))
         (output (widget/scroll-pane (:size (make-2d 1000 461)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*
                                      :selection (as (subseq* (take-cc (va output-selection) 1) 0 1)))
                   (as (output-of (va content-iomap))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

;;;;;;
;;; Reader

(def reader workbench/workbench->widget/shell ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-f1)
                     :domain "Workbench" :description "Selects navigation page"
                     :operation (make-operation/replace-selection '((the workbench/page (navigation-page-of (the workbench/workbench document))))))
                    ((make-key-press-gesture :scancode-f2)
                     :domain "Workbench" :description "Selects editing page"
                     :operation (make-operation/replace-selection '((the workbench/page (editing-page-of (the workbench/workbench document))))))
                    ((make-key-press-gesture :scancode-f3)
                     :domain "Workbench" :description "Selects information page"
                     :operation (make-operation/replace-selection '((the workbench/page (information-page-of (the workbench/workbench document))))))
                    ((make-key-press-gesture :scancode-return)
                     :domain "File system" :description "Opens the selected document"
                     :operation (reference-case (reverse-cc (get-selection -printer-input-))
                                  (((the string (subseq (the string document) ?start-index ?end-index))
                                    (the string (file-namestring (the pathname document)))
                                    (the pathname (pathname-of (the file-system/file document)))
                                    . ?rest)
                                   (bind ((elements-length (length (elements-of (editing-page-of -printer-input-))))
                                          (filename (pathname-of (eval-reference -printer-input- (flatten-reference (reverse ?rest)))))
                                          (document (workbench/document (:title (file-namestring filename) :filename filename))))
                                     (make-operation/compound (list (make-operation/sequence/replace-range `((the workbench/page (editing-page-of (the workbench/workbench document)))
                                                                                                             (the sequence (elements-of (the workbench/page document)))
                                                                                                             (the sequence (subseq (the sequence document) ,elements-length ,elements-length)))
                                                                                                           (list document))
                                                                    (make-operation/load-document document filename))))))))
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/page->widget/tabbed-pane ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-t :control)
                     :domain "Workbench" :description "Inserts a new empty document"
                     :operation (bind ((elements-length (length (elements-of -printer-input-))))
                                  (make-operation/compound (list (make-operation/functional (lambda ()
                                                                                              (setf (elements-of -printer-input-) (append (elements-of -printer-input-)
                                                                                                                                          (list (workbench/document (:title "Untitled" :selection '((the document/nothing (content-of (the workbench/document document)))))
                                                                                                                                                  (document/nothing ())))))))
                                                                 (make-operation/replace-selection `((the sequence (elements-of (the workbench/page document)))
                                                                                                     (the workbench/document (elt (the sequence document) ,elements-length))))))))
                    ((make-key-press-gesture :scancode-f4 :control)
                     :domain "Workbench" :description "Closes the current document"
                     :operation (make-operation/functional (lambda ()
                                                             (setf (elements-of -printer-input-) (rest (elements-of -printer-input-)))))))
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/navigator->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/console->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/descriptor->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/operator->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/searcher->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/evaluator->widget/scroll-pane ()
  (merge-commands (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))

(def reader workbench/document->widget/scroll-pane ()
  (merge-commands (gesture-case -gesture-
                    ((make-key-press-gesture :scancode-s :control)
                     :domain "Workbench" :description "Saves the currently edited document."
                     :operation (make-operation/save-document -printer-input- (filename-of -printer-input-)))
                    ((make-key-press-gesture :scancode-l :control)
                     :domain "Workbench" :description "Loads the previously saved document."
                     :operation (make-operation/load-document -printer-input- (filename-of -printer-input-))))
                  (command/read-selection -recursion- -input- -printer-iomap-)
                  (command/read-backward -recursion- -input- -printer-iomap-)
                  (make-nothing-command -gesture-)))
