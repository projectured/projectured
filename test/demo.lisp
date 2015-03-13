;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Demo definition API

(def class* demo ()
  ((name :type symbol)
   (document :type document)
   (document-factory :type function)
   (projection :type projection)
   (projection-factory :type function)))

(def namespace demo)

(def definer demo (name document projection)
  `(setf (find-demo ',name)
         (make-instance 'demo :name ',name
                        :document ,document :document-factory (lambda () ,document)
                        :projection ,projection :projection-factory (lambda () ,projection))))

(def macro with-demo ((&key name instance) &body forms)
  `(bind ((demo (find-demo ,name :otherwise nil))
          (document (or document
                        (ecase ,instance
                          ((nil :new) (funcall (document-factory-of demo)))
                          (:current (document-of demo)))))
          (projection (or projection
                          (ecase ,instance
                            ((nil :new) (funcall (projection-factory-of demo)))
                            (:current (projection-of demo))))))
     ,@forms))

;;;;;;
;;; Various predefined demos

(def demo graphics (make-test-document/graphics) (make-test-projection/graphics->graphics))

(def demo graphics/ll (make-test-document/graphics/ll 1000 500) (make-test-projection/graphics->graphics))

(def demo string (make-test-document/string) (make-test-projection/string->graphics))

(def demo string/empty (make-test-document/string/empty) (make-test-projection/string->graphics))

(def demo string/delimited (make-test-document/string) (make-test-projection/string->graphics/delimited))

(def demo string/removing (make-test-document/string) (make-test-projection/string->graphics/removing))

(def demo string/sorting (make-test-document/string) (make-test-projection/string->graphics/sorting))

(def demo text (make-test-document/text) (make-test-projection/text->graphics))

(def demo text/empty (make-test-document/text/empty) (make-test-projection/text->graphics))

(def demo text/ll (make-test-document/text/ll 1000 500) (make-test-projection/text->output))

(def demo text/searching/ll (document/search (:selection '((the text/text (content-of (the document/search document)))
                                                                         (the text/text (text/subseq (the text/text document) 0 0)))
                                                                       :search "0")
                                            (make-test-document/text/ll 1000 500))
  (sequential
    (nesting
      (document/search->text/text)
      (preserving))
    (make-test-projection/text->output)))

(def demo widget (make-test-document/widget/menu) (make-test-projection/widget->graphics))

(def demo list (make-test-document/list) (make-test-projection/list->graphics))

(def demo table (make-test-document/table) (make-test-projection/table->graphics))

(def demo table/nested (make-test-document/table/nested) (make-test-projection/table->graphics))

(def demo tree (make-test-document/tree) (make-test-projection/tree->graphics))

(def demo tree/ll (make-test-document/tree/ll 10 5 2) (make-test-projection/tree->graphics))

(def demo tree/leaf (make-test-document/tree/leaf) (make-test-projection/tree->graphics))

(def demo tree/node (make-test-document/tree/node) (make-test-projection/tree->graphics))

(def demo tree/empty (make-test-document/tree/empty) (make-test-projection/tree->graphics))

(def demo tree/removing (make-test-document/tree) (make-test-projection/tree->graphics/removing))

(def demo tree/sorting (make-test-document/tree) (make-test-projection/tree->graphics/sorting))

(def demo graph (make-test-document/graph) (make-test-projection/graph->graphics))

(def demo state-machine (make-test-document/state-machine) (make-test-projection/state-machine->graphics))

(def demo book (make-test-document/book) (make-test-projection/book->graphics))

(def demo book/sorting (make-test-document/book) (make-test-projection/book->graphics/sorting))

(def demo book/focusing (make-test-document/book) (make-test-projection/book->graphics/focusing))

(def demo xml (make-test-document/xml) (make-test-projection/xml->graphics))

(def demo xml/empty (make-test-document/xml/empty) (make-test-projection/xml->graphics))

(def demo xml/ll (make-test-document/xml/ll 200 100 3) (make-test-projection/xml->graphics))

(def demo json (make-test-document/json) (make-test-projection/json->graphics))

(def demo json/alternative (make-test-document/json) (make-test-projection/json->graphics/alternative))

(def demo json/focusing (make-test-document/json) (make-test-projection/json->graphics/focusing))

(def demo json/removing (make-test-document/json) (make-test-projection/json->graphics/removing))

(def demo json/reversing (make-test-document/json) (make-test-projection/json->graphics/reversing))

(def demo json/sorting (make-test-document/json) (make-test-projection/json->graphics/sorting))

(def demo css (make-test-document/css) (make-test-projection/css->graphics))

(def demo file-system (make-test-document/file-system) (make-test-projection/file-system->graphics))

(def demo java (make-test-document/java) (make-test-projection/java->graphics))

(def demo javascript (make-test-document/javascript) (make-test-projection/javascript->graphics))

(def demo lisp-form (make-test-document/lisp-form) (make-test-projection/lisp-form->graphics))

(def demo common-lisp (make-test-document/common-lisp) (make-test-projection/common-lisp->graphics))

(def demo common-lisp/inliner (make-test-document/common-lisp) (make-test-projection/common-lisp/inliner->graphics))

(def demo common-lisp/test (make-test-document/common-lisp/test) (make-test-projection/common-lisp->graphics/test))

(def demo evaluator (make-test-document/evaluator) (make-test-projection/evaluator))

(def demo sql (make-test-document/sql) (make-test-projection/sql->graphics))

(def demo t/null (make-test-document/t/null) (make-test-projection/t->graphics/table))

(def demo t/cons (make-test-document/t/cons) (make-test-projection/t->graphics/table))

(def demo t/object (make-test-document/t/object) (make-test-projection/t->graphics/table))

(def demo t/object/nested (make-test-document/t/object/nested) (make-test-projection/t->graphics/table))

(def demo t/text (make-test-document/text) (make-test-projection/t->graphics/table))

(def demo t/tree (make-test-document/tree) (make-test-projection/t->graphics/table))

(def demo t/xml (make-test-document/xml) (make-test-projection/t->graphics/tree))

(def demo t/json (make-test-document/json) (make-test-projection/t->graphics/tree))

(def demo inspector/object (make-test-document/inspector/object) (make-test-projection/inspector->graphics))

(def demo inspector/object/nested (make-test-document/inspector/object/nested) (make-test-projection/inspector->graphics))

(def demo demo (make-initial-document/web-example) (make-test-projection/demo->graphics))

(def demo documentation (make-test-document/documentation) (make-test-projection/documentation->graphics))

(def demo workbench (make-test-document/workbench) (make-test-projection/workbench->graphics))
