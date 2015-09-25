;; top level demo projection
(recursive ; applies a projection recursively
  (type-dispatching ; dispatches on the type of a document object
    (widget/base (widget->graphics)) ; does scrollling, tooltip handling (uses a type-dispatching compound projection)
    (document/base (document->t)) ; adds empty document, clipboard operations, insertion
    (document (demo/search-projection)))) ; applies demo search projection

; demo search projection
(alternative ; allows searching in two different modes
  (sequential ; applies a sequence of projections (one after the other)
    (demo/document-projection) ; applies demo document projection producing a text
    (text-searching) ; creates a subset of a text by doing a text search
    (demo/graphics-projection)) ; applies demo graphics projection
  (sequential
    (object-searching) ; creates a subset of a document object by doing a recursive object search
    (demo/document-projection) ; applies demo document projection producing a text
    (demo/graphics-projection))) ; applies demo graphics projection

;; demo document projection
(sequential
  (recursive
    (type-dispatching
      (book/book (copying)) ; copies the top level
      (book/chapter (chapter-numbering)) ; adds automatic chapter numbers
      (document (preserving)))) ; keeps intact other parts
  (focusing) ; allows focusing on arbitrary parts of the document
  (recursive
    (reference-dispatching ; allows notation customization of arbitrary parts
      (alternative ; allows editing in generic and predefined notation
        (type-dispatching
          (text/base (word-wrapping 1270)) ; does automatic word wrapping
          (book/paragraph (text-aligning 1270)) ; does automatic text alginment (left, right, center, or justified)
          (book/base (book->tree))
          (demo/natural (demo/natural-projection)) ; applies natural projection (allows different views of the same content)
          (demo/folded (demo/folded-projection))) ; applies folded projection (allows different views of the same content)
        (recursive ; allows altarnative generic notation
          (t->tree)))))
  (recursive ; creates flat text of tree nodes (indentation, opening/closing delimiters, separators)
    (type-dispatching
      (tree/base (tree->text))
      (text/text (preserving)))))

; demo graphics projection
(recursive ; allows combining text and images
  (type-dispatching
    (text/base (text->graphics))
    (image/base (image->graphics))
    (graphics/base (preserving))))

;; demo natural projection
(sequential
  (recursive
    (reference-dispatching ; allows notation customization of arbitrary parts
      (alternative ; allows editing in generic and predefined notation
        (type-dispatching ; combines different domains into a tree
          ;; these projections are type-dispatching compound projections
          (xml/base (xml->tree))
          (css/base (css->tree))
          (json/base (json->tree))
          (javascript/base (javascript->tree))
          (common-lisp/base (sequential
                              (common-lisp->lisp-form)
                              (lisp-form->tree)))
          (lisp-form/base (lisp-form->tree)))
        (recursive ; allows altarnative generic notation
          (t->tree)))))
  (recursive ; creates a flat text from a tree (with indentation, opening/closing delimiters, separators)
    (type-dispatching
      (tree/base (tree->text))
      (text/text (preserving))))
  (line-numbering)) ; adds automatic line numbers

;; demo folded projection: Common Lisp folded streaming notation
(sequential
  (recursive
    (type-dispatching ; combines different domains into a tree
      ;; these projections are type-dispatching compound projections
      (xml/base (xml->tree))
      (css/base (css->tree))
      (javascript/base (javascript->tree))
      (json/base (json->tree))
      ((or common-lisp/base lisp-form/base sequence) (copying))
      ((or number string text/text) (preserving))))
  (recursive ; creates a Common Lisp program from a tree (the program will write the tree into a stream)
    (type-dispatching
      (tree/base (tree->common-lisp))
      ((or common-lisp/base lisp-form/base sequence) (copying))
      ((or number string text/text) (preserving))))
  (recursive ; folds Common Lisp stream output operations while keeping the rest intact
    (type-dispatching
      (common-lisp/progn (write-stream-folding))
      ((or common-lisp/base lisp-form/base sequence) (copying))))
  (recursive ; creates a tree of text from a Common Lisp program
    (type-dispatching
      (common-lisp/base (sequential
                          (common-lisp->lisp-form)
                          (lisp-form->tree)))
      (text/text (preserving))))
  (recursive ; creates flat text of tree nodes (indentation, opening/closing delimiters, separators)
    (type-dispatching
      (tree/base (tree->text))
      (text/text (preserving))))
  (line-numbering)) ; adds automatic line numbers
