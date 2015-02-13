;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection document/search->widget/text ()
  ())

;;;;;;
;;; Construction

(def function make-projection/document/search->widget/text ()
  (make-projection 'document/search->widget/text))

;;;;;;
;;; Construction

(def macro document/search->widget/text ()
  '(make-projection/document/search->widget/text))

;;;;;;
;;; Forward mapper

(def function forward-mapper/document/search->widget/text (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the string (search-of (the document/search document)))
        (the string (subseq (the string document) ?start-index ?end-index)))
       (bind ((offset 8))
         `((the text/text (text/subseq (the text/text document) ,(+ ?start-index offset) ,(+ ?end-index offset)))
           (the text/text (content-of (the widget/text document))))))
      (((the widget/text (printer-output (the document/search document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion))
         (reverse ?rest))))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/document/search->widget/text (printer-iomap reference)
  (bind ((printer-input (input-of printer-iomap))
         (projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    (pattern-case (reverse reference)
      (((the text/text (content-of (the widget/text document)))
        (the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
       (bind ((offset 8))
         (if (and (not (string= (search-of printer-input) ""))
                  (>= ?start-index 8))
             `((the string (subseq (the string document) ,(- ?start-index offset) ,(- ?end-index offset)))
               (the string (search-of (the document/search document))))
             (append reference `((the widget/text (printer-output (the document/search document) ,projection ,recursion)))))))
      (?
       (append reference `((the widget/text (printer-output (the document/search document) ,projection ,recursion))))))))

;;;;;;
;;; Printer

(def printer document/search->widget/text (projection recursion input input-reference)
  (bind ((search-string (search-of input))
         (empty? (zerop (length search-string)))
         (output-selection (print-selection (make-iomap/object projection recursion input input-reference nil)
                                            (selection-of input)
                                            'forward-mapper/document/search->widget/text))
         (output (widget/text (:selection output-selection :location (make-2d 0 0) :margin (make-inset :all 5))
                   (text/text (:selection (butlast output-selection))
                     (text/string "Search: " :font *font/ubuntu/regular/24* :font-color *color/solarized/gray*)
                     (text/string (if empty?
                                      "enter search text"
                                      search-string)
                                  :font *font/ubuntu/regular/24*
                                  :font-color (if empty? (color/lighten *color/solarized/blue* 0.75) *color/solarized/blue*))))))
    (make-iomap/object projection recursion input input-reference output)))

;;;;;;
;;; Reader

(def reader document/search->widget/text (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap))
         (operation-mapper (lambda (operation selection child-selection child-iomap)
                             (declare (ignore child-selection child-iomap))
                             (typecase operation
                               (operation/text/replace-range
                                (pattern-case (reverse selection)
                                  (((the string (search-of (the document/search document)))
                                    (the string (subseq (the string document) ?start-index ?end-index)))
                                   (make-operation/sequence/replace-range printer-input selection (replacement-of operation)))
                                  (((the widget/text (printer-output (the document/search document) ?projection ?recursion))
                                    . ?rest)
                                   (make-operation/sequence/replace-range printer-input '((the string (subseq (the string document) 0 0))
                                                                                          (the string (search-of (the document/search document))))
                                                                          (replacement-of operation)))))))))
    (merge-commands (command/read-backward recursion input printer-iomap 'backward-mapper/document/search->widget/text operation-mapper)
                    (make-command/nothing (gesture-of input)))))
