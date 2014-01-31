;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

#+nil
(def projection document->graphics ()
  ())

;;;;;;
;;; Construction

#+nil
(def (function e) make-projection/document->graphics ()
  (make-projection 'document->graphics))

;;;;;;
;;; Construction

#+nil
(def (macro e) document->graphics ()
  '(make-projection/document->graphics))

;;;;;;
;;; Printer

#+nil
(def printer document->graphics (projection recursion input input-reference)
  (bind ((input-content (content-of input))
         (content-iomap-cs (as (recurse-printer recursion input-content
                                                `((content-of (the document document))
                                                  ,@(typed-reference (form-type input) input-reference)))))
         (content-iomap (computed-state-value* content-iomap-cs))
         (output-content (output-of content-iomap))
         (input-selection (selection-of input)))
    (pattern-case input-selection
      (((?or (the character (elt (the string ?a) ?b))
             (the sequence-position (pos (the string ?a) ?b))
             (the sequence-position (text/pos (the text/text ?a) ?b)))
        . ?rest)
       (bind ((selection-graphics-cs
               (as (bind ((graphics-reference nil))
                     (map-forward content-iomap input-selection
                                  (lambda (content-iomap reference)
                                    (declare (ignore content-iomap))
                                    (setf graphics-reference reference)))
                     (pattern-case graphics-reference
                       (((the character (elt (the string (text-of (the graphics/text (elt (the sequence (elements-of (the graphics/canvas ?a))) ?b)))) ?c)))
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (offset-text (subseq (text-of text-graphics) 0 ?c))
                               (text (subseq (text-of text-graphics) ?c (1+ ?c)))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text offset-text font))))
                          (make-graphics/rectangle (+ location (make-2d offset 0)) (measure-text text font) :fill-color *color/solarized/background/light*)))
                       (((the sequence-position (pos (the string document) ?c))
                         (the string (text-of (the graphics/text document)))
                         (the graphics/text (elt (the sequence document) ?b))
                         (the sequence (elements-of (the graphics/canvas document)))
                         . ?rest)
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (text (subseq (text-of text-graphics) 0 ?c))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text text font)))
                               (height (2d-y (measure-text "" font))))
                          (make-graphics/line (+ location (make-2d offset 0))
                                              (+ location (make-2d offset height))
                                              :stroke-color *color/black*)))))))
              (output (make-graphics/canvas (as (optional-list (output-of content-iomap)
                                                               (computed-state-value* selection-graphics-cs)))
                                            (make-2d 0 0))))
         (make-iomap/compound projection recursion input input-reference output
                              (list (make-iomap/object projection recursion input input-reference output nil) content-iomap))))
      (((the sequence-box (text/subbox (the text/text ?a) ?b ?c)))
       (bind ((output (make-graphics/canvas (optional-list (iter (with top = nil)
                                                                 (with left = nil)
                                                                 (with right = nil)
                                                                 (for character-index :from ?b :to ?c)
                                                                 (bind ((graphics-reference nil))
                                                                   (map-forward content-iomap `((the sequence-position (text/pos (the text/text document) ,character-index)))
                                                                                (lambda (iomap reference)
                                                                                  (declare (ignore iomap))
                                                                                  (setf graphics-reference reference)))
                                                                   (pattern-case graphics-reference
                                                                     (((the sequence-position (pos (the string document) ?c))
                                                                       (the string (text-of (the graphics/text document)))
                                                                       (the graphics/text (elt (the sequence document) ?b))
                                                                       (the sequence (elements-of (the graphics/canvas document))))
                                                                      (bind ((text-graphics (elt (elements-of output-content) ?b))
                                                                             (text (subseq (text-of text-graphics) 0 ?c))
                                                                             (location (location-of text-graphics))
                                                                             (font (font-of text-graphics))
                                                                             (offset (2d-x (measure-text text font)))
                                                                             (height (2d-y (measure-text "" font))))
                                                                        (unless left
                                                                          (setf left (+ (2d-x location) offset)))
                                                                        (setf right (+ (2d-x location) offset))
                                                                        ;; TODO: to support tree
                                                                        #+nil
                                                                        (maximizing (+ (2d-x location) offset) :into right)
                                                                        (unless top
                                                                          (setf top (2d-y location)))
                                                                        (maximizing (+ (2d-y location) height) :into bottom)))))
                                                                 (finally
                                                                  (return (make-graphics/rectangle (make-2d left top) (make-2d (- right left) (- bottom top)) :fill-color *color/solarized/background/light*))))
                                                           (output-of content-iomap))
                                            (make-2d 0 0))))
         (make-iomap/compound projection recursion input input-reference output
                              (list (make-iomap/object projection recursion input input-reference output nil) content-iomap))))
      (?a
       (make-iomap/compound projection recursion input input-reference output-content
                            (list (make-iomap/object projection recursion input input-reference output-content nil) content-iomap))))))

;;;;;;
;;; Reader

(def function document/read-operation (document gesture)
  (gesture-case gesture
    ((gesture/keyboard/key-press :sdl-key-s :control)
     :domain "Document" :help "Saves the currently edited document to '/tmp/document.pred'"
     :operation (make-operation/save-document document))
    ((gesture/keyboard/key-press :sdl-key-l :control)
     :domain "Document" :help "Loads a previously saved document from '/tmp/document.pred'"
     :operation (make-operation/load-document document))
    ((gesture/keyboard/key-press :sdl-key-e :control)
     :domain "Document" :help "Exports the currently edited document to '/tmp/document.txt'"
     :operation (make-operation/export-document document))
    ((gesture/keyboard/key-press :sdl-key-escape)
     :domain "Document" :help "Quits from the editor"
     :operation (make-operation/quit))
    ((make-instance 'gesture/window/quit :modifiers nil)
     :domain "Document" :help "Quits from the editor"
     :operation (make-operation/quit))))

#+nil
(def reader document->graphics (projection recursion input printer-iomap)
  (declare (ignore projection document-iomap))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (document (input-of printer-iomap))
         (child-operation (recurse-reader recursion (elt (child-iomaps-of printer-iomap) 1) gesture-queue operation printer-iomap)))
    (or (merge-commands (gesture-case latest-gesture
                            ((gesture/keyboard/key-press :sdl-key-escape)
                             :domain "Document" :help "Quits from the editor"
                             :operation (make-operation/quit))
                            ((gesture/keyboard/key-press :sdl-key-s :control)
                             :domain "Document" :help "Saves the currently edited document to '/tmp/document.pred'"
                             :operation (make-operation/save-document document))
                            ((gesture/keyboard/key-press :sdl-key-l :control)
                             :domain "Document" :help "Loads a previously saved document from '/tmp/document.pred'"
                             :operation (make-operation/load-document document))
                            ((gesture/keyboard/key-press :sdl-key-e :control)
                             :domain "Document" :help "Exports the currently edited document to '/tmp/document.txt'"
                             :operation (make-operation/export-document document)))
                          child-operation)
        (and (typep latest-gesture 'gesture/window/quit)
             (make-operation/quit)))))
