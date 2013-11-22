;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document->graphics ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/document->graphics ()
  (make-projection 'document->graphics))

;;;;;;
;;; Construction

(def (macro e) document->graphics ()
  '(make-projection/document->graphics))

;;;;;;
;;; Printer

(def printer document->graphics (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (iomap-cs (as (recurse-printer recursion iomap (content-of input) `(content-of ,typed-input-reference) `(elt (the list (elements-of (the graphics/canvas ,output-reference))) 1))))
         (iomap (computed-state-value* iomap-cs))
         (output-content (output-of iomap))
         (input-selection (selection-of input)))
    (pattern-case input-selection
      ((?or (the character (elt (the string ?a) ?b))
            (the character (text/elt (the text/text ?a) ?b))
            (the sequence-position (pos (the string ?a) ?b))
            (the sequence-position (text/pos (the text/text ?a) ?b)))
       (bind ((selection-graphics-cs
               (as (bind ((graphics-reference nil))
                     (map-forward iomap (tree-replace (selection-of input) '(the document document) typed-input-reference)
                                  (lambda (iomap reference)
                                    (declare (ignore iomap))
                                    (setf graphics-reference reference)))
                     (pattern-case graphics-reference
                       ((the character (elt (the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ?a))) ?b)))) ?c))
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (offset-text (subseq (text-of text-graphics) 0 ?c))
                               (text (subseq (text-of text-graphics) ?c (1+ ?c)))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text offset-text font))))
                          (make-graphics/rectangle (+ location (make-2d offset 0)) (measure-text text font) :fill-color *color/solarized/background/light*)))
                       ((the sequence-position (pos (the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ?a))) ?b)))) ?c))
                        (bind ((text-graphics (elt (elements-of output-content) ?b))
                               (text (subseq (text-of text-graphics) 0 ?c))
                               (location (location-of text-graphics))
                               (font (font-of text-graphics))
                               (offset (2d-x (measure-text text font)))
                               (height (2d-y (measure-text "" font))))
                          (make-graphics/line (+ location (make-2d offset 0))
                                              (+ location (make-2d offset height))
                                              :stroke-color *color/black*)))))))
              (output (make-graphics/canvas (as (optional-list (computed-state-value* selection-graphics-cs)
                                                               (output-of (computed-state-value* iomap-cs))))
                                            (make-2d 0 0))))
         (make-iomap/compound projection recursion input input-reference output output-reference
                              (list (make-iomap/object projection recursion input input-reference output output-reference) iomap))))
      ((?or (the sequence-box (text/subbox (the text/text ?a) ?b ?c)))
       (bind ((output (make-graphics/canvas (optional-list (iter (with top = nil)
                                                                 (with left = nil)
                                                                 (for character-index :from ?b :to ?c)
                                                                 (bind ((graphics-reference nil))
                                                                   (map-forward iomap (tree-replace `(the sequence-position (text/pos (the text/text ,?a) ,character-index)) '(the document document) typed-input-reference)
                                                                                (lambda (iomap reference)
                                                                                  (declare (ignore iomap))
                                                                                  (setf graphics-reference reference)))
                                                                   (pattern-case graphics-reference
                                                                     ((the sequence-position (pos (the string (text-of (the graphics/text (elt (the list (elements-of (the graphics/canvas ?a))) ?b)))) ?c))
                                                                      (bind ((text-graphics (elt (elements-of output-content) ?b))
                                                                             (text (subseq (text-of text-graphics) 0 ?c))
                                                                             (location (location-of text-graphics))
                                                                             (font (font-of text-graphics))
                                                                             (offset (2d-x (measure-text text font)))
                                                                             (height (2d-y (measure-text "" font))))
                                                                        (unless left
                                                                          (setf left (+ (2d-x location) offset)))
                                                                        (maximizing (+ (2d-x location) offset) :into right)
                                                                        (unless top
                                                                          (setf top (2d-y location)))
                                                                        (maximizing (+ (2d-y location) height) :into bottom)))))
                                                                 (finally
                                                                  (return (make-graphics/rectangle (make-2d left top) (make-2d (- right left) (- bottom top)) :fill-color *color/solarized/background/light*))))
                                                           (output-of (computed-state-value* iomap-cs)))
                                            (make-2d 0 0))))
         (make-iomap/compound projection recursion input input-reference output output-reference
                              (list (make-iomap/object projection recursion input input-reference output output-reference) iomap))))
      (?a
       (make-iomap/compound projection recursion input input-reference output-content output-reference
                            (list (make-iomap/object projection recursion input input-reference output-content output-reference) iomap))))))

;;;;;;
;;; Reader

(def reader document->graphics (projection recursion printer-iomap projection-iomap gesture-queue operation document-iomap)
  (declare (ignore projection document-iomap))
  (bind ((latest-gesture (first (gestures-of gesture-queue)))
         (document (input-of projection-iomap))
         (child-operation (recurse-reader recursion printer-iomap (elt (child-iomaps-of projection-iomap) 1) gesture-queue operation projection-iomap)))
    (or (merge-operations (gesture-case latest-gesture
                            ((gesture/keyboard/key-press :sdl-key-escape)
                             :domain "Document" :help "Quits from the editor"
                             :operation (make-operation/quit))
                            ((gesture/keyboard/key-press :sdl-key-s :control)
                             :domain "Document" :help "Saves the currently edited document to '/tmp/document.pred'"
                             :operation (make-operation/save-document document))
                            ((gesture/keyboard/key-press :sdl-key-l :control)
                             :domain "Document" :help "Loads a previously saved document from '/tmp/document.pred'"
                             :operation (make-operation/load-document document)))
                          child-operation)
        (and (typep latest-gesture 'gesture/window/quit)
             (make-operation/quit)))))
