;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def function initial-factory (name)
  (completion-prefix-switch name
    ("book" (book/book (:selection '((the string (title-of (the book/book document)))
                                     (the string (subseq (the string document) 0 0))))))
    ("chapter" (book/chapter (:selection '((the string (title-of (the book/chapter document)))
                                           (the string (subseq (the string document) 0 0))))))
    ("paragraph" (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                               (the text/text (text/subseq (the text/text document) 0 0))))
                   (text/text ()
                     (text/string "" :font *font/liberation/serif/regular/24* :font-color (color/darken *color/solarized/blue* 0.5)))))
    ("list" (book/list (:selection '((the sequence (elements-of (the book/list document)))
                                     (the book/paragraph (elt (the sequence document) 0))
                                     (the text/text (content-of (the book/paragraph document)))
                                     (the text/text (text/subseq (the text/text document) 0 0))))
              (book/paragraph (:selection '((the text/text (content-of (the book/paragraph document)))
                                            (the text/text (text/subseq (the text/text document) 0 0))))
                (text/text () (text/string "" :font *font/liberation/serif/regular/24* :font-color (color/darken *color/solarized/blue* 0.5))))))
    ("picture" (book/picture (:selection '((the image/file (content-of (the book/picture document)))
                                           (the string (filename-of (the image/file document)))
                                           (the string (subseq (the string document) 0 0))))
                 (image/file () "")))
    ("xml element" (xml/element ("" nil :selection '((the string (xml/start-tag (the xml/element document)))
                                                     (the string (subseq (the string document) 0 0))))))
    ("xml attribute" (xml/attribute (:selection '((the string (name-of (the xml/attribute document)))
                                                  (the string (subseq (the string document) 0 0)))) "" ""))
    ("xml text" (xml/text (:selection '((the string (value-of (the xml/text document)))
                                        (the string (subseq (the string document) 0 0)))) ""))
    ("css rule" (css/rule ("" :selection '((the string (selector-of (the css/rule document)))
                                           (the string (subseq (the string document) 0 0))))))
    ("css attribute" (css/attribute (:selection '((the string (name-of (the css/attribute document)))
                                                  (the string (subseq (the string document) 0 0)))) "" ""))
    ("json null" (json/null ()))
    ("json false" (json/boolean () #f))
    ("json true" (json/boolean ()#t))
    ("json number" (json/number () 0))
    ("json string" (json/string () ""))
    ("json array" (json/array (:selection '((the sequence (elements-of (the json/array document)))
                                            (the json/insertion (elt (the sequence document) 0))
                                            (the string (value-of (the json/insertion document)))
                                            (the string (subseq (the string document) 0 0))))
                    (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                  (the string (subseq (the string document) 0 0)))))))
    ("json object entry" (json/object-entry (:selection '((the string (key-of (the json/object-entry document)))
                                                          (the string (subseq (the string document) 0 0))))
                                            ""
                                            (json/insertion ())))
    ("json object" (make-json/object (make-document/sequence (list (json/insertion (:selection '((the string (value-of (the json/insertion document)))
                                                                                                 (the string (subseq (the string document) 0 0))))))
                                                             :selection '((the json/insertion (elt (the sequence document) 0))
                                                                          (the string (value-of (the json/insertion document)))
                                                                          (the string (subseq (the string document) 0 0))))
                                     :selection '((the sequence (entries-of (the json/object document)))
                                                  (the json/insertion (elt (the sequence document) 0))
                                                  (the string (value-of (the json/insertion document)))
                                                  (the string (subseq (the string document) 0 0)))))
    ("common lisp comment" (make-instance 'common-lisp/comment
                                          :content (text/text () (text/string ""))
                                          :selection '((the string (content-of (the common-lisp/comment document)))
                                                       (the string (subseq (the string document) 0 0)))))
    ("common lisp string" (make-instance 'common-lisp/constant
                                         :value ""
                                         :selection '((the string (value-of (the common-lisp/constant document)))
                                                      (the string (subseq (the string document) 0 0)))))
    ("common lisp number" (make-instance 'common-lisp/constant
                                         :value 0
                                         :selection '((the number (value-of (the common-lisp/constant document)))
                                                      (the string (write-to-string (the number document)))
                                                      (the string (subseq (the string document) 0 0)))))
    ("common lisp if" (make-instance 'common-lisp/if
                                     :condition (document/nothing)
                                     :then (document/nothing)
                                     :else (document/nothing)))
    ("common lisp progn" (make-instance 'common-lisp/progn :body nil))
    ("common lisp function application" (make-instance 'common-lisp/application
                                                       :operator (make-lisp-form/symbol "" "PROJECTURED.TEST")
                                                       :selection '((the lisp-form/symbol (operator-of (the common-lisp/application document)))
                                                                    (the string (name-of (the lisp-form/symbol document)))
                                                                    (the string (subseq (the string document) 0 0)))
                                                       :arguments nil))
    ("common lisp function definition" (make-instance 'common-lisp/function-definition
                                                      :name (make-lisp-form/symbol "" "PROJECTURED.TEST") :documentation "" :bindings nil :allow-other-keys #f :body nil
                                                      :selection '((the lisp-form/symbol (name-of (the common-lisp/function-definition document)))
                                                                   (the string (name-of (the lisp-form/symbol document)))
                                                                   (the string (subseq (the string document) 0 0)))))))

(def function initial-slot-provider (instance)
  (remove-if (lambda (slot) (member (slot-definition-name slot) '(raw selection))) (class-slots (class-of instance))))

(def function make-initial-projection ()
  (sequential
    (recursive
      (type-dispatching
        (workbench/document
          (nesting
            (workbench->widget)
            (sequential
              (recursive
                (type-dispatching
                  (book/book (copying))
                  (book/chapter (book/chapter->book/chapter))
                  (sequence (copying))
                  (t (preserving))))
              (focusing '(or book/base xml/base json/base common-lisp/base lisp-form/base text/base) nil)
              (sequential
                (recursive
                  (reference-dispatching
                    (alternative
                      (type-dispatching
                        (book/base (book->tree))
                        (xml/base (xml->tree))
                        (css/base (css->tree))
                        (json/base (json->tree))
                        (common-lisp/base (sequential
                                            (common-lisp->lisp-form)
                                            (lisp-form->tree)))
                        (lisp-form/base (lisp-form->tree))
                        (javascript/base (javascript->tree))
                        (document/base (document->t 'initial-factory))
                        (table/base (sequential
                                      (recursive
                                        (type-dispatching
                                          (table/base (table->text))
                                          (text/base (preserving))))
                                      (text/text->tree/leaf)))
                        (text/base #+nil (word-wrapping 1000)
                                   (preserving))
                        (tree/base (preserving))
                        (t (t->tree :slot-provider 'initial-slot-provider)))
                      (t->tree :slot-provider 'initial-slot-provider))))
                (recursive
                  (type-dispatching
                    (tree/base (tree->text))
                    (text/text (preserving))))
                #+nil (line-numbering)))))
        (workbench/base (workbench->widget))
        (file-system/base (sequential
                            (recursive (file-system->tree))
                            (recursive (type-dispatching
                                         (tree/base (tree->text))
                                         (text/text (preserving))))))
        (document/base (preserving))
        (text/base (preserving))))
    (recursive
      (type-dispatching
        (widget/base (widget->graphics))
        (document/base (document->t 'initial-factory))
        (text/text (text->graphics))))))

(def projection web-example->t ()
  ())

(def macro web-example->t ()
  '(make-projection 'web-example->t))

(def printer web-example->t (projection recursion input input-reference)
  (bind ((content-iomap (recurse-printer recursion (content-of input) `((content-of (the ,(form-type input) document))
                                                                        ,@(typed-reference (form-type input) input-reference)))))
    (make-iomap/compound projection recursion input input-reference (output-of content-iomap) (list content-iomap))))

(def reader web-example->t (projection recursion input printer-iomap)
  (declare (ignore projection))
  (bind ((printer-input (input-of printer-iomap)))
    (merge-commands (bind ((content-iomap (elt (child-iomaps-of printer-iomap) 0))
                           (content-command (recurse-reader recursion input content-iomap)))
                      (make-command (gesture-of input)
                                    (operation/extend printer-input `((the ,(form-type (content-of printer-input)) (content-of (the ,(form-type (input-of printer-iomap)) document)))) (operation-of content-command))
                                    :domain (domain-of content-command)
                                    :description (description-of content-command)))
                    (make-command/nothing (gesture-of input)))))
