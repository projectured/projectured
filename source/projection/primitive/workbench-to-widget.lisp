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

(def projection workbench/navigator->widget/title-pane ()
  ())

(def projection workbench/console->widget/title-pane ()
  ())

(def projection workbench/editor->widget/tabbed-pane ()
  ())

(def projection workbench/document->widget/scroll-pane ()
  ())

;;;;;;
;;; Construction

(def function make-projection/workbench/workbench->widget/shell ()
  (make-projection 'workbench/workbench->widget/shell))

(def function make-projection/workbench/navigator->widget/title-pane ()
  (make-projection 'workbench/navigator->widget/title-pane))

(def function make-projection/workbench/console->widget/title-pane ()
  (make-projection 'workbench/console->widget/title-pane))

(def function make-projection/workbench/editor->widget/tabbed-pane ()
  (make-projection 'workbench/editor->widget/tabbed-pane))

(def function make-projection/workbench/document->widget/scroll-pane ()
  (make-projection 'workbench/document->widget/scroll-pane))

;;;;;;
;;; Construction

(def macro workbench/workbench->widget/shell ()
  '(make-projection/workbench/workbench->widget/shell))

(def macro workbench/navigator->widget/title-pane ()
  '(make-projection/workbench/navigator->widget/title-pane))

(def macro workbench/console->widget/title-pane ()
  '(make-projection/workbench/console->widget/title-pane))

(def macro workbench/editor->widget/tabbed-pane ()
  '(make-projection/workbench/editor->widget/tabbed-pane))

(def macro workbench/document->widget/scroll-pane ()
  '(make-projection/workbench/document->widget/scroll-pane))

;;;;;;
;;; Printer

(def printer workbench/workbench->widget/shell (projection recursion input input-reference)
  (bind ((navigator-iomap (recurse-printer recursion (navigator-of input) `((navigator-of (the workbench/workbench document))
                                                                            ,@(typed-reference (form-type input) input-reference))))
         (console-iomap (recurse-printer recursion (console-of input) `((console-of (the workbench/workbench document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (editor-iomap (recurse-printer recursion (editor-of input) `((editor-of (the workbench/workbench document))
                                                                      ,@(typed-reference (form-type input) input-reference))))
         (output (widget/shell (:border (make-inset :all 5)
                                :border-color *color/gray191*)
                   (widget/split-pane (:selection '((the sequence (elements-of (the widget/split-pane document)))
                                                    (the widget/split-pane (elt (the sequence document) 1))))
                     (output-of navigator-iomap)
                     (widget/split-pane (:orientation :vertical
                                         :selection '((the sequence (elements-of (the widget/split-pane document)))
                                                      (the widget/tabbed-pane (elt (the sequence document) 0))))
                       (output-of editor-iomap)
                       (output-of console-iomap))))))
    (make-iomap/compound projection recursion input input-reference output (list navigator-iomap console-iomap editor-iomap))))

(def printer workbench/navigator->widget/title-pane (projection recursion input input-reference)
  (bind ((folder-iomaps (iter (for index :from 0)
                              (for folder :in-sequence (folders-of input))
                              (collect (recurse-printer recursion folder `((elt (the sequence document) ,index)
                                                                           (the sequence (folders-of (the workbench/navigator document)))
                                                                           ,@(typed-reference (form-type input) input-reference))))))
         (output (widget/title-pane (:title (text/text () (text/string "Navigator" :font *font/ubuntu/regular/24*))
                                     :title-fill-color *color/solarized/content/lighter*
                                     :border (make-inset :all 5)
                                     :border-color *color/solarized/content/lighter*)
                   (widget/scroll-pane (:size (make-2d 224 659)
                                        :padding (make-inset :all 5)
                                        :padding-color *color/white*)
                     (make-widget/composite (make-2d 0 0) (mapcar 'output-of folder-iomaps))))))
    (make-iomap/compound projection recursion input input-reference output folder-iomaps)))

(def printer workbench/console->widget/title-pane (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the workbench/document document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (output (widget/title-pane (:title (text/text () (text/string "Console" :font *font/ubuntu/regular/24*))
                                     :title-fill-color *color/solarized/content/lighter*
                                     :border (make-inset :all 5)
                                     :border-color *color/solarized/content/lighter*)
                   (widget/scroll-pane (:size (make-2d 1000 141)
                                        :padding (make-inset :all 5)
                                        :padding-color *color/white*)
                     (output-of content-iomap)))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

(def printer workbench/editor->widget/tabbed-pane (projection recursion input input-reference)
  (bind ((document-iomaps (iter (for index :from 0)
                                (for document :in-sequence (documents-of input))
                                (collect (recurse-printer recursion document `((elt (the sequence document) ,index)
                                                                               (the sequence (documents-of (the workbench/editor document)))
                                                                               ,@(typed-reference (form-type input) input-reference))))))
         (output (make-widget/tabbed-pane (iter (for document :in-sequence (documents-of input))
                                                (for document-iomap :in document-iomaps)
                                                (collect (list (text/text () (text/string (title-of document) :font *font/ubuntu/regular/24*))
                                                               (output-of document-iomap))))
                                          0
                                          :border (make-inset :all 5)
                                          :border-color *color/solarized/content/lighter*)))
    (make-iomap/compound projection recursion input input-reference output document-iomaps)))

(def printer workbench/document->widget/scroll-pane (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the workbench/document document))
                                                                        ,@(typed-reference (form-type input) input-reference))))
         (output (widget/scroll-pane (:size (make-2d 1000 461)
                                      :padding (make-inset :all 5)
                                      :padding-color *color/white*)
                   (output-of content-iomap))))
    (make-iomap/compound projection recursion input input-reference output (list content-iomap))))

;;;;;;
;;; Reader

(def reader workbench/workbench->widget/shell (projection recursion input printer-iomap)
  (declare (ignore projection))
  ;; TODO: check selection
  (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 2)))

(def reader workbench/navigator->widget/title-pane (projection recursion input printer-iomap)
  (declare (ignore projection))
  (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))

(def reader workbench/console->widget/title-pane (projection recursion input printer-iomap)
  (declare (ignore projection))
  (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))

(def reader workbench/editor->widget/tabbed-pane (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-t :control)
                       :domain "Workbench" :description "Inserts a new empty document"
                       :operation (make-operation/functional (lambda ()
                                                               (setf (documents-of printer-input) (append (documents-of printer-input)
                                                                                                          (list (workbench/document (:title "Untitled")
                                                                                                                  (document/nothing)))))))))
                    (gesture-case (gesture-of input)
                      ((gesture/keyboard/key-press :sdl-key-f4 :control)
                       :domain "Workbench" :description "Closes the current document"
                       :operation (make-operation/functional (lambda ()
                                                               (setf (documents-of printer-input) (rest (documents-of printer-input)))))))
                    (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))))

(def reader workbench/document->widget/scroll-pane (projection recursion input printer-iomap)
  (declare (ignore projection))
  (recurse-reader recursion input (elt (child-iomaps-of printer-iomap) 0)))
