;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Widget

(def function make-test-document/document (content)
  (document/document ()
    (document/clipboard ()
      content)))

(def function make-test-document/shell (content)
  (widget/shell ()
    content))

(def function make-test-document/plain (content)
  (widget/shell (:size (make-2d 1280 720))
    (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
      content)))

(def function make-test-document/search (content)
  (bind ((document
          (document/search (:search "")
            content)))
    (widget/shell ()
      (widget/composite (:location (make-2d 0 0) :selection '((the sequence (elements-of (the widget/composite document)))
                                                              (the widget/scroll-pane (elt (the sequence document) 0))
                                                              (the document/search (content-of (the widget/scroll-pane document)))))
        (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 690) :margin (make-inset :all 5))
          document)
        (widget/composite (:location (make-2d 1000 0))
          document)))))

(def function make-test-document/selection (content)
  (widget/split-pane ()
    (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
      content)
    (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
      content)))

(def function make-test-document/split (content-1 content-2)
  (widget/shell ()
    (widget/split-pane ()
      (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
        content-1)
      (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
        content-2))))

(def function make-test-document/generic (content)
  (widget/shell ()
    (widget/split-pane ()
      (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
        content)
      (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 640 720) :margin (make-inset :all 5))
        content))))

(def function make-test-document/reflection (content projection)
  (widget/shell ()
    (widget/tabbed-pane ()
      ((widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
         (text/text ()
           (text/string "Document in Domain Specific Notation" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
       (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
         content))
      ((widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
         (text/text ()
           (text/string "Document in Generic Notation" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
       (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
         content))
      ((widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
         (text/text ()
           (text/string "Projection in Generic Notation" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
       (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
         projection))
      ((widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
         (text/text ()
           (text/string "Intermediate Documents in Generic Notation" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
       (make-widget/tabbed-pane
        (when (typep projection 'sequential)
          (iter (for element-projection :in-sequence (elements-of projection))
                ;; TODO:
                (repeat 1)
                (collect (list
                          (widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
                            (text/text ()
                              (text/string (object-class-symbol-name element-projection) :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
                          (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
                            content)))))
        0)))))

(def function make-test-document/ide (content)
  (widget/shell ()
    (widget/split-pane ()
      (make-file-system/pathname (resource-pathname "test/"))
      (widget/tabbed-pane ()
        ((widget/label (:location (make-2d 5 5) :margin (make-inset :all 5))
           (text/text ()
             (text/string "Document" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
         (widget/scroll-pane (:location (make-2d 0 0) :size (make-2d 1280 720) :margin (make-inset :all 5))
           content))))))

;;;;;;
;;; Debug

(def document test/debug ()
  ((content :type t)
   (last-commands :type sequence)))

(def macro test/debug (() &body content)
  `(make-instance 'test/debug :content ,(first content) :last-commands nil))

(def function make-test-document/debug (content)
  (test/debug ()
    content))

;;;;;;
;;; Graphics

(def function make-test-document/graphics/empty ()
  (make-graphics/canvas nil (make-2d 0 0)))

(def function make-test-document/graphics ()
  (make-graphics/canvas (list (make-graphics/viewport (make-graphics/canvas (list (make-graphics/point (make-2d 50 150) :stroke-color *color/red*)
                                                                                  (make-graphics/line (make-2d 150 300) (make-2d 50 400) :stroke-color *color/blue*)
                                                                                  (make-graphics/rectangle (make-2d 200 200) (make-2d 100 100) :stroke-color *color/green*)
                                                                                  (make-graphics/rounded-rectangle (make-2d 120 180) (make-2d 50 50) 10 :fill-color *color/dark-yellow*)
                                                                                  (make-graphics/polygon (list (make-2d 150 100) (make-2d 160 160) (make-2d 100 150)) :stroke-color *color/black*)
                                                                                  (make-graphics/circle (make-2d 50 250) 50 :stroke-color *color/black* :fill-color *color/blue*)
                                                                                  (make-graphics/ellipse (make-2d 50 50) (make-2d 100 50) :stroke-color *color/red*)
                                                                                  (make-graphics/text (make-2d 200 150) "hello world" :font *font/default* :font-color *color/default* :fill-color *color/light-cyan*)
                                                                                  (make-graphics/image (make-2d 300 0) (make-image/file (resource-pathname "image/projectured.png"))))
                                                                            (make-2d 0 0))
                                                      (make-2d 50 50)
                                                      (make-2d 700 400))
                              (make-graphics/rectangle (make-2d 50 50)
                                                       (make-2d 700 400)
                                                       :stroke-color *color/red*))
                        (make-2d 0 0)))


(def function make-test-document/graphics/ll (element-count origin-index)
  (make-graphics/canvas (elt-ll (ll (iter (for i :from 0 :below element-count)
                                          (collect (make-graphics/canvas (list (make-graphics/text (make-2d 0 0) "Hello " :font *font/default* :font-color *color/default* :fill-color nil)
                                                                               (make-graphics/text (make-2d 100 0) "World " :font *font/default* :font-color *color/default* :fill-color nil)
                                                                               (make-graphics/text (make-2d 200 0) (write-to-string i) :font *font/default* :font-color *color/default* :fill-color nil))
                                                                         (make-2d 0 (* (- i origin-index) 20))))))
                                origin-index)
                        (make-2d 0 0)))

;;;;;;
;;; String

(def function make-test-document/string/empty ()
  "")

(def function make-test-document/string ()
  "just a simple string")

(def function make-test-document/string/lorem-ipsum ()
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras eu nunc nibh. Cras imperdiet faucibus tortor ac dictum. Aliquam sit amet justo nec ligula lobortis ornare. Aenean a odio id dolor adipiscing interdum. Maecenas nec nisl neque. Suspendisse interdum rutrum neque, in volutpat orci varius in. Praesent a ipsum ac erat pulvinar adipiscing quis sit amet magna. Etiam semper vulputate mi ac interdum. Nunc a tortor non purus fringilla aliquam.")

;;;;;;
;;; Text

(def function make-test-document/text ()
  (text/text ()
    (text/string "First" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)
    ;;(text/spacing 10 :unit :space)
    (image/file () (resource-pathname "image/lisp-flag.jpg"))
    (text/string "line" :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/green*)
    (text/newline)
    (text/string "Second" :font *font/ubuntu/bold/24* :font-color *color/solarized/blue*)
    (text/string " " :font *font/ubuntu/bold/24*)
    (text/string "line" :font *font/ubuntu/bold/24* :font-color *color/solarized/red*)
    (text/newline)
    (text/string "Third" :font *font/ubuntu/bold/24* :font-color *color/solarized/red*)
    (text/string " " :font *font/ubuntu/bold/24*)
    (text/string "line" :font *font/ubuntu/bold/24* :font-color *color/solarized/green*)
    (text/newline)
    (text/string "Fourth" :font *font/ubuntu/bold/24* :font-color *color/solarized/green*)
    (text/string " " :font *font/ubuntu/bold/24*)
    (text/string "line" :font *font/ubuntu/bold/24* :font-color *color/solarized/blue*)))

(def function make-test-document/text/empty ()
  (text/text ()
    (text/string "")))

(def function make-test-document/text/ll (element-count origin-index)
  (text/make-text (elt-ll (ll (iter (for i :from 0 :below element-count)
                                    (unless (first-iteration-p)
                                      (collect (text/newline)))
                                    (appending (list (text/string "Hello " :font *font/liberation/serif/regular/18* :font-color *color/solarized/blue*)
                                                     (text/string "World " :font-color *color/black*)
                                                     (text/string (write-to-string i) :font *font/ubuntu/regular/18* :font-color *color/solarized/red*)
                                                     (text/string (string+ " " (make-string (1+ (mod i 10)) :initial-element #\*)) :font *font/ubuntu/regular/18* :font-color *color/solarized/gray*)))))
                          (* origin-index 4))
                  :selection '((the text/text (text/subseq (the text/text document) 0 0)))))

;;;;;;
;;; Widget

(def function make-test-document/widget/menu ()
  (widget/menu ()
    (widget/menu-item ()
      (text/text ()
        (text/string "Open")))
    (widget/menu-item ()
      (text/text ()
        (text/string "Save")))
    (widget/menu-item ()
      (text/text ()
        (text/string "Quit")))))

;;;;;;
;;; List

(def function make-test-document/list/empty ()
  (list/list ()))

(def function make-test-document/list ()
  (list/list ()
    (list/element ()
      (text/text ()
        (text/string "first element in a list")))
    (list/element ()
      (text/text ()
        (text/string "second element in a list")))))

;;;;;;
;;; Table

(def function make-test-document/table/empty ()
  (table/table ()))

(def function make-test-document/table ()
  (table/table ()
    (table/row ()
      (table/cell ()
        (text/text ()
          (text/string "1st row 1st cell 1st line (padding)" :font *font/default* :font-color *color/default*)
          (text/newline)
          (text/string "1st row 1st cell 2nd line" :font *font/default* :font-color *color/default*)))
      (table/cell ()
        (text/text ()
          (text/string "1st row 2nd cell 1st line" :font *font/default* :font-color *color/default*))))
    (table/row ()
      (table/cell ()
        (text/text ()
          (text/string "2nd row 1st cell 1st line" :font *font/default* :font-color *color/default*)))
      (table/cell ()
        (text/text ()
          (text/string "2nd row 2nd cell 1st line" :font *font/default* :font-color *color/default*)
          (text/newline)
          (text/string "2nd row 1st cell 2nd line (padding)" :font *font/default* :font-color *color/default*))))))

(def function make-test-document/table/nested ()
  (table/table ()
    (table/row ()
      (table/cell ()
        (table/table ()
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A1" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B1" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C1" :font *font/default* :font-color *color/default*))))
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A2" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B2" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C2" :font *font/default* :font-color *color/default*))))
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A3" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B3" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C3" :font *font/default* :font-color *color/default*))))))
      (table/cell ()
        (text/text ()
          (text/string "Outer top right cell" :font *font/default* :font-color *color/default*))))
    (table/row ()
      (table/cell ()
        (text/text ()
          (text/string "Outer bottom left cell" :font *font/default* :font-color *color/default*)))
      (table/cell ()
        (table/table ()
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A1" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B1" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C1" :font *font/default* :font-color *color/default*))))
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A2" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B2" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C2" :font *font/default* :font-color *color/default*))))
          (table/row ()
            (table/cell ()
              (text/text ()
                (text/string "Inner A3" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner B3" :font *font/default* :font-color *color/default*)))
            (table/cell ()
              (text/text ()
                (text/string "Inner C3" :font *font/default* :font-color *color/default*)))))))))

;;;;;;
;;; Tree

(def function make-test-document/tree/empty ()
  nil)

(def function make-test-document/tree/leaf ()
  (tree/leaf (:opening-delimiter (text/text () (text/string "\"")) :closing-delimiter (text/text () (text/string "\"")))
    (text/text ()
      (text/string "Hello"))))

(def function make-test-document/tree/node ()
  (tree/node ()))

(def function make-test-document/tree ()
  (tree/node (:opening-delimiter (text/text () (text/string "(")) :closing-delimiter (text/text () (text/string ")")) :separator (text/text () (text/string " ")))
    (tree/leaf ()
      (text/text ()
        (text/string "first")))
    (tree/node (:opening-delimiter (text/text () (text/string "(")) :closing-delimiter (text/text () (text/string ")")) :separator (text/text () (text/string " ")) :indentation 1)
      (tree/leaf ()
        (text/text ()
          (text/string "second")))
      (tree/leaf ()
        (text/text ()
          (text/string "third"))))
    (tree/leaf (:indentation 1)
      (text/text ()
        (text/string "fourth")))
    (tree/node (:opening-delimiter (text/text () (text/string "(")) :closing-delimiter (text/text () (text/string ")")) :separator (text/text () (text/string " ")))
      (tree/leaf ()
        (text/text ()
          (text/string "fifth")))
      (tree/node (:opening-delimiter (text/text () (text/string "(")) :closing-delimiter (text/text () (text/string ")")) :separator (text/text () (text/string " ")) :indentation 1)
        (tree/leaf ()
          (text/text ()
            (text/string "sixth")))
        (tree/leaf ()
          (text/text ()
            (text/string "seventh"))))
      (tree/leaf (:indentation 1)
        (text/text ()
          (text/string "eigth"))))
    (tree/leaf (:indentation 1)
      (text/text ()
        (text/string "nineth")))))

(def function make-test-document/tree/ll (element-count origin-index depth &key include-delimiters)
  (labels ((recurse (depth path)
             (if (zerop depth)
                 (tree/leaf (:indentation 2
                             :opening-delimiter (when include-delimiters
                                                  (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                             :closing-delimiter (when include-delimiters
                                                  (text/text () (text/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                             :selection '((the text/text (content-of (the tree/leaf document)))
                                          (the text/text (text/subseq (the text/text document) 0 0))))
                   (text/text (:selection '((the text/text (text/subseq (the text/text document) 0 0))))
                     (text/string (format nil "Hello World ~{~A.~}" (reverse path)) :font *font/liberation/serif/regular/18* :font-color *color/solarized/content/darker*)))
                 (bind ((elements (list* (make-computed-ll (as (tree/leaf ()
                                                                 (text/text ()
                                                                   (text/string (if path (format nil "Chapter ~{~A.~}" (reverse path)) "Book") :font *font/liberation/serif/regular/18* :font-color *color/solarized/blue*))))
                                                           (as nil) (as nil))
                                         (iter (for i :from 0 :below element-count)
                                               (collect (rebind (i) (make-computed-ll (as (recurse (1- depth) (cons (1+ i) path)))
                                                                                      (as nil) (as nil)))))))
                        (origin-element (elt elements origin-index)))
                   (iter (for i :from 0 :below (length elements))
                         (setf (previous-element-of (elt elements i)) (when (> i 0) (elt elements (1- i))))
                         (setf (next-element-of (elt elements i)) (when (< i (1- (length elements))) (elt elements (1+ i)))))
                   (make-tree/node origin-element
                                   :opening-delimiter (when include-delimiters
                                                        (text/text () (text/string "(" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                   :closing-delimiter (when include-delimiters
                                                        (text/text () (text/string ")" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                   :separator (text/text () (text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                   :indentation 2
                                   :selection (append (selection-of (value-of origin-element))
                                                      `((the sequence (children-of (the tree/node document)))
                                                        (the ,(form-type (value-of origin-element)) (elt (the sequence document) ,origin-index)))))))))
    (recurse depth nil)))

;;;;;;
;;; Graph

(def function make-test-document/graph ()
  (bind ((vertex1 (make-graph/vertex "A"))
         (vertex2 (make-graph/vertex "B"))
         (vertex3 (make-graph/vertex "C"))
         (edge12 (make-graph/edge vertex1 vertex2))
         (edge13 (make-graph/edge vertex1 vertex3))
         (edge23 (make-graph/edge vertex2 vertex3)))
    (make-graph/graph (list vertex1 vertex2 vertex3) (list edge12 edge13 edge23))))

;;;;;;
;;; State machine

(def function make-test-document/state-machine ()
  (bind ((start-state (make-state-machine/state "Start"))
         (a-state (make-state-machine/state "A"))
         (b-state (make-state-machine/state "B"))
         (fail-state (make-state-machine/state "Fail")))
    (make-state-machine/state-machine "AB"
                                      (list start-state a-state b-state fail-state)
                                      (list (make-state-machine/transition "Start->A" "a" start-state a-state)
                                            (make-state-machine/transition "Start->B" "b" start-state b-state)
                                            (make-state-machine/transition "A->B" "b" a-state b-state)
                                            (make-state-machine/transition "B->A" "a" b-state a-state)
                                            (make-state-machine/transition "A->Fail" "a" a-state fail-state)
                                            (make-state-machine/transition "B->Fail" "b" b-state fail-state)))))

;;;;;;
;;; Book

(def function make-test-document/book/empty ()
  (book/book (:title "")))

(def function make-test-document/book ()
  (book/book (:title "Lorem ipsum" :author "me")
    (book/chapter (:title "Chapter")
      (book/paragraph (:alignment :justified)
        (text/text ()
          (text/string (make-test-document/string/lorem-ipsum) :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*)))
      (book/list ()
        (book/paragraph ()
          (text/text ()
            (text/string "first" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*)))
        (book/paragraph ()
          (text/text ()
            (text/string "second" :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*)))))
    (book/chapter (:title "Chapter")
      (book/paragraph (:alignment :justified)
        (text/text ()
          (text/string "Morbi scelerisque, felis a viverra pharetra, arcu enim aliquet urna, mollis suscipit felis elit in neque. Aenean vel tempus nulla. Vestibulum magna nisi, cursus vel auctor eu, suscipit sit amet purus. Donec ligula purus, pulvinar id tristique ut, suscipit ornare diam. Maecenas sed justo turpis. Vivamus eu scelerisque dui. Pellentesque mollis rutrum est ac tempus. Sed venenatis, erat id elementum semper, nisl tortor malesuada orci, ac venenatis elit ipsum non augue. Praesent blandit purus est, id venenatis eros. Phasellus non dui dolor. Duis magna erat, pulvinar sed aliquam vitae, porta vel quam." :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))

(def function make-test-document/book/paragraph ()
  (book/paragraph (:alignment :justified)
    (text/text ()
      (text/string (make-test-document/string/lorem-ipsum)))))

;;;;;;
;;; XML

(def function make-test-document/xml/empty ()
  nil)

(def function make-test-document/xml/text ()
  (xml/text () "The beginning"))

(def function make-test-document/xml/attribute ()
  (xml/attribute () "name" "levy"))

(def function make-test-document/xml ()
  (xml/element ("person" ((xml/attribute () "name" "levy")
                          (xml/attribute () "sex" "male")))
    (xml/text () "The beginning")
    (xml/element ("children" ())
      (xml/element ("person" ((xml/attribute () "name" "John")
                              (xml/attribute () "sex" "female"))))
      (xml/element ("person" ((xml/attribute () "name" "Mary")
                              (xml/attribute () "sex" "male")))))
    (xml/element ("pets" ()))
    (xml/text () "The end")))

(def function make-test-document/xml/ll (element-count origin-index depth)
  (labels ((recurse (depth path)
             (if (zerop depth)
                 (xml/element ("person" ((xml/attribute () "name" (format nil "~{~A.~}" (reverse path)))
                                         (xml/attribute () "sex" (if (evenp (first path)) "male" "female")))))
                 (bind ((elements (iter (for i :from 0 :below element-count)
                                        (collect (rebind (i) (make-computed-ll (as (recurse (1- depth) (cons i path)))
                                                                               (as nil) (as nil)))))))
                   (iter (for i :from 0 :below (length elements))
                         (setf (previous-element-of (elt elements i)) (when (> i 0) (elt elements (1- i))))
                         (setf (next-element-of (elt elements i)) (when (< i (1- (length elements))) (elt elements (1+ i)))))
                   (make-xml/element "person"
                                     (list (xml/attribute () "name" (if path (format nil "~{~A.~}" (reverse path)) "."))
                                           (xml/attribute () "sex" (if (and path (evenp (first path))) "male" "female")))
                                     (elt elements origin-index))))))
    (recurse depth nil)))

;;;;;;
;;; JSON

(def function make-test-document/json/empty ()
  (json/insertion ()))

(def function make-test-document/json/null ()
  (json/null ()))

(def function make-test-document/json/boolean ()
  (json/boolean () #f))

(def function make-test-document/json/number ()
  (json/number () 42))

(def function make-test-document/json/string ()
  (json/string () "Hello World"))

(def function make-test-document/json/array ()
  (json/array ()
    (json/null ())
    (json/boolean () #f)
    (json/boolean ()#t)
    (json/number () 42)
    (json/string () "Hello World")))

(def function make-test-document/json/object ()
  (json/object ()
    ("null" (json/null ()))
    ("false" (json/boolean () #f))
    ("true" (json/boolean ()#t))
    ("number" (json/number () 42))
    ("string" (json/string () "Hello World"))))

(def function make-test-document/json ()
  (json/array ()
    (json/null ())
    (json/boolean () #f)
    (json/boolean ()#t)
    (json/number () 42)
    (json/string () "Hello World")
    (json/object ()
      ("null" (json/null ()))
      ("false" (json/boolean () #f))
      ("true" (json/boolean ()#t))
      ("number" (json/number () 42))
      ("string" (json/string () "Hello World"))
      ("array" (json/array ()
                 (json/null ())
                 (json/boolean () #f)
                 (json/boolean ()#t)
                 (json/number () 42)
                 (json/string () "Hello World"))))))

(def function make-test-document/json/ll ()
  (make-json/array (ll (iter (for i :from 0 :to 100)
                             (collect (json/object ()
                                        ("name" (json/string () (format nil "User ~A" i)))
                                        ("sex" (json/string () (if (evenp i) "male" "female")))))))))

;;;;;;
;;; CSS

(def function make-test-document/css ()
  (css/rule ("h1")
    (css/attribute () "color" "#DC322F")
    (css/attribute () "text-align" "center")))

;;;;;;
;;; File system

(def function make-test-document/file-system ()
  (make-file-system/pathname (resource-pathname "source/projection/")))

;;;;;;
;;; Java

(def function make-test-document/java/empty ()
  nil)

(def function make-test-document/java ()
  (make-java/definition/method (make-java/definition/qualifier "public")
                                (make-java/definition/type "int")
                                "factorial"
                                (list (make-java/definition/argument "n" (make-java/definition/type "int")))
                                (make-java/statement/block (list (make-java/statement/if (make-java/expression/infix-operator "==" (list (make-java/expression/variable-reference "n")
                                                                                                                                         (make-java/literal/number 0)))
                                                                                         (make-java/statement/return (make-java/literal/number 1))
                                                                                         (make-java/statement/return (make-java/expression/infix-operator "*" (list (make-java/expression/variable-reference "n")
                                                                                                                                                                    (make-java/expression/method-invocation "factorial"
                                                                                                                                                                                                            (list (make-java/expression/infix-operator "-" (list (make-java/expression/variable-reference "n")
                                                                                                                                                                                                                                                                 (make-java/literal/number 1)))))))))))))

;;;;;;
;;; Javascript

(def function make-test-document/javascript ()
  (make-javascript/statement/top-level
   (list (make-javascript/expression/method-invocation
          (make-javascript/expression/variable-reference "google")
          "load"
          (list (make-javascript/literal/string "visualization")
                (make-javascript/literal/string "1")))
         (make-javascript/expression/method-invocation
          (make-javascript/expression/variable-reference "google")
          "setOnLoadCallback"
          (list (make-javascript/expression/variable-reference "drawPieChart")))
         (make-javascript/definition/function
          "drawPieChart"
          nil
          (make-javascript/statement/block
           (list (make-javascript/definition/variable
                  "json"
                  (make-javascript/expression/property-access
                   (make-javascript/expression/method-invocation
                    (make-javascript/expression/variable-reference "$")
                    "ajax" nil)
                   "responseText"))
                 (make-javascript/definition/variable
                  "data"
                  (make-javascript/expression/constuctor-invocation
                   (make-javascript/expression/property-access
                    (make-javascript/expression/property-access
                     (make-javascript/expression/variable-reference "google")
                     "visualization")
                    "DataTable")
                   (list (make-javascript/expression/variable-reference "json"))))
                 (make-javascript/definition/variable
                  "chart"
                  (make-javascript/expression/constuctor-invocation
                   (make-javascript/expression/property-access
                    (make-javascript/expression/property-access
                     (make-javascript/expression/variable-reference "google")
                     "visualization")
                    "PieChart")
                   (list (make-javascript/expression/method-invocation
                          (make-javascript/expression/variable-reference "document")
                          "getElementById"
                          (list (make-javascript/literal/string "pie"))))))
                 (make-javascript/expression/method-invocation
                  (make-javascript/expression/variable-reference "chart")
                  "draw"
                  (list (make-javascript/expression/variable-reference "data")))))))))

;;;;;;
;;; Lisp form

(def function make-test-document/lisp-form/empty ()
  nil)

(def function make-test-document/lisp-form ()
  (make-lisp-form/list (list (make-lisp-form/string "quoted list")
                             (make-lisp-form/number 3.1415)
                             (make-lisp-form/list (list (make-lisp-form/symbol "sub" nil)
                                                        (make-lisp-form/list (list (make-lisp-form/symbol "deep" nil) (make-lisp-form/symbol "list" nil)))
                                                        (make-lisp-form/number 42)
                                                        (make-lisp-form/number 43)))
                             (make-lisp-form/object (make-string-output-stream)))))

;;;;;;
;;; Common lisp

(def function test-dynamic-environment-provider (thunk)
  (hu.dwim.stefil::with-new-global-context* (:debug-on-assertion-failure-p #f :debug-on-unexpected-error-p #f :print-test-run-progress-p #f :record-success-descriptions-p #t)
    (bind ((test-result (nth-value 1 (hu.dwim.stefil::run-test-body (make-instance 'hu.dwim.stefil::test :name (gensym) :package *package*) thunk nil t nil))))
      (make-test/result (map 'list 'hu.dwim.stefil::form-of (remove-if (of-type 'hu.dwim.stefil::unexpected-error) (hu.dwim.stefil::failure-descriptions-of test-result)))
                        (map 'list 'hu.dwim.stefil::form-of (hu.dwim.stefil::success-descriptions-of test-result))
                        (awhen (find-if (of-type 'hu.dwim.stefil::unexpected-error) (hu.dwim.stefil::failure-descriptions-of test-result))
                          (hu.dwim.stefil::condition-of it))))))

(def function make-test-document/common-lisp/empty ()
  nil)

(def function make-test-document/common-lisp (&optional faulty)
  (bind ((factorial-argument (make-common-lisp/required-function-argument (make-lisp-form/symbol* 'n)))
         (factorial-function (make-common-lisp/function-definition (make-lisp-form/symbol (if faulty "FACTORAL" "FACTORIAL") "PROJECTURED.TEST")
                                                                   (list factorial-argument)
                                                                   nil
                                                                   :allow-other-keys #f
                                                                   :documentation "Computes the factorial of N")))
    (setf (body-of factorial-function)
          (list (make-common-lisp/if (make-common-lisp/application (make-lisp-form/symbol* '<)
                                                                   (list (make-common-lisp/variable-reference factorial-argument)
                                                                         (make-common-lisp/constant (if faulty 3 2))))
                                     (make-common-lisp/constant 1)
                                     (make-common-lisp/application (make-lisp-form/symbol* (if faulty '+ '*))
                                                                   (list (make-common-lisp/variable-reference factorial-argument)
                                                                         (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                                                                                       (list (make-common-lisp/application
                                                                                                              (make-lisp-form/symbol* '-)
                                                                                                              (list (make-common-lisp/variable-reference factorial-argument)
                                                                                                                    (make-common-lisp/constant 1))))))))))
    factorial-function))

(def function make-test-document/common-lisp/test (&optional (faulty #t))
  (bind ((factorial-function (make-test-document/common-lisp faulty)))
    (book/book (:title "Literate programming with continuous testing")
      (book/chapter (:title "Specification")
        (book/paragraph ()
          (text/text ()
            (text/string "The factorial of a non-negative integer " :font *font/liberation/serif/regular/24*)
            (text/string "N" :font *font/liberation/serif/italic/24* :font-color *color/solarized/violet*)
            (text/string " is the product of all positive integers less than or equal to " :font *font/liberation/serif/regular/24*)
            (text/string "N" :font *font/liberation/serif/italic/24* :font-color *color/solarized/violet*)
            (text/string "." :font *font/liberation/serif/regular/24*))))
      (book/chapter (:title "Implementation")
        (book/paragraph ()
          (text/text ()
            (text/string "The " :font *font/liberation/serif/regular/24*)
            (text/string "Common Lisp" :font *font/liberation/serif/italic/24* :font-color *color/solarized/violet*)
            (text/string " factorial function computes the result using recursion." :font *font/liberation/serif/regular/24*)))
        (make-evaluator/evaluator factorial-function))
      (book/chapter (:title "Test")
        (book/paragraph ()
          (text/text ()
            (text/string "The " :font *font/liberation/serif/regular/24*)
            (text/string "Common Lisp" :font *font/liberation/serif/italic/24* :font-color *color/solarized/violet*)
            (text/string " test program checks the result for all integers between " :font *font/liberation/serif/regular/24*)
            (text/string "0" :font *font/liberation/serif/regular/24* :font-color *color/solarized/magenta*)
            (text/string " and " :font *font/liberation/serif/regular/24*)
            (text/string "10" :font *font/liberation/serif/regular/24* :font-color *color/solarized/magenta*)
            (text/string "." :font *font/liberation/serif/regular/24*)))
        (bind ((evaluator (make-evaluator/evaluator nil :dynamic-environment-provider 'test-dynamic-environment-provider)))
          (setf (form-of evaluator) (make-common-lisp/progn (append (iter (for i :from 0 :to 8)
                                                                          (collect (make-test/check (make-common-lisp/application (make-lisp-form/symbol* '=)
                                                                                                                                  (list (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                                                                                                                                                      (list (make-common-lisp/constant i)))
                                                                                                                                        (make-common-lisp/constant (alexandria::factorial i))))
                                                                                                    evaluator)))
                                                                    (list (make-test/check (make-common-lisp/application (make-lisp-form/symbol* '=)
                                                                                                                         (list (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                                                                                                                                             (list (make-common-lisp/constant 9)))
                                                                                                                               (make-common-lisp/constant 3628800)))
                                                                                           evaluator)
                                                                          (make-test/check (make-common-lisp/application (make-lisp-form/symbol* '=)
                                                                                                                         (list (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                                                                                                                                             (list (make-common-lisp/constant 10)))
                                                                                                                               (make-common-lisp/constant 3628800)))
                                                                                           evaluator)))))
          evaluator)))))

;;;;;;
;;; Evaluator

(def function make-test-document/evaluator ()
  ;; TODO:
  #+nil
  (hu.dwim.walker:walk-form '(* 2 (+ 3 4) (- 5 6))))

;;;;;;
;;; Test

(def function factorial (n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(def test test/factorial ()
  (is (= 1 (factorial 0)))
  (is (= 1 (factorial 1)))
  (is (= 2 (factorial 2)))
  (is (= 6 (factorial 3)))
  (is (= 24 (factorial 4))))

(def function make-test-document/test ()
  (find-test 'test/factorial))

;;;;;
;;; SQL

(def function make-test-document/sql ()
  (sql/select ()
    (list (sql/column-reference ()
            (sql/column () "name" "varchar"))
          (sql/column-reference ()
            (sql/column () "age" "integer")))
    (list (sql/table-reference ()
            (sql/table () "person")))))

;;;;;;
;;; T

(def function make-test-document/t/null ()
  nil)

(def function make-test-document/t/list ()
  (list "Hello World"))

(def function make-test-document/t/sequence ()
  (document/sequence () "Hello World"))

(def function make-test-document/t/color ()
  *color/solarized/blue*)

(def function make-test-document/t/font ()
  *font/ubuntu/monospace/regular/24*)

(def class* test-object ()
  ((test-instance-slot)
   (test-class-slot :allocation :class)))

(def function make-test-document/t/object ()
  (make-instance 'test-object :test-class-slot "class slot value" :test-instance-slot "instance slot value"))

(def function make-test-document/t/object/nested ()
  (make-instance 'test-object
                 :test-class-slot "class slot value"
                 :test-instance-slot (make-instance 'test-object :test-class-slot "class slot value" :test-instance-slot "instance slot value")))

;;;;;;
;;; Inspector

(def function make-test-document/inspector/object ()
  (make-inspector/object (make-test-document/t/object)))

(def function make-test-document/inspector/object/nested ()
  (make-inspector/object (make-test-document/t/object/nested)))

;;;;;;
;;; Documentation

(def function make-test-document/documentation ()
  (bind ((json-document (make-test-document/json))
         (xml-document (make-test-document/xml)))
    (book/book (:title "ProjecturEd, the Projectional Editor" :author "Levente Mészáros")
      (book/chapter (:title "Domains")
        #+nil
        (book/chapter (:title "Graphics" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some graphics")))
          (make-test-document/graphics))
        (book/chapter (:title "Text")
          (book/paragraph ()
            (text/text ()
              (text/string "This domain provides styled text with " :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
              (text/string "fonts" :font *font/liberation/serif/italic/24* :font-color *color/solarized/content/darker*)
              (text/string " and colors." :font *font/ubuntu/regular/18* :font-color *color/solarized/blue*))))
        #+nil
        (book/chapter (:title "List" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some list")))
          (make-test-document/list))
        #+nil
        (book/chapter (:title "Tree" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some tree")))
          (make-test-document/tree))
        #+nil
        (book/chapter (:title "Table")
          (book/paragraph ()
            (text/text ()
              (text/string "This domain provides tables with aligned columns and rows." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          (make-test-document/table/nested))
        (book/chapter (:title "JSON")
          (book/paragraph ()
            (text/text ()
              (text/string "This domain provides the elements of the well known JSON data format." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          json-document)
        (book/chapter (:title "XML")
          (book/paragraph ()
            (text/text ()
              (text/string "This domain provides the elements of well known XML data format." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          xml-document)
        #+nil
        (book/chapter (:title "Java" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some Java code")))
          (make-test-document/java))
        #+nil
        (book/chapter (:title "Javascript" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some Javascript code")))
          (make-test-document/javascript))
        (book/chapter (:title "S-expression" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some Lisp S-expression")))
          (make-test-document/lisp-form))
        #+nil
        (book/chapter (:title "Common Lisp" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some Common Lisp code")))
          (make-test-document/common-lisp))
        #+nil
        (book/chapter (:title "Object" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "Some object")))
          (make-test-document/t/object))
        (book/chapter (:title "Mixed")
          (book/paragraph ()
            (text/text ()
              (text/string "This feature allowes mixing documents in arbitrary ways." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          (xml/element ("mixed" ())
            (xml/element ("something" ((xml/attribute () "extra" "true"))))
            json-document
            xml-document)))
      (book/chapter (:title "Projections")
        (book/chapter (:title "Sorting" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "This projection allows sorting arbitrary document parts based on arbitrary criteria and editing the result." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          json-document)
        (book/chapter (:title "Filtering" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "This projection allows filtering out arbitrary document parts based on arbitrary criteria and editing the result." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          json-document)
        (book/chapter (:title "Selecting" :expanded #f)
          (book/paragraph ()
            (text/text ()
              (text/string "This projection allows selecting arbitrary document parts based on arbitrary criteria and editing the result." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)))
          json-document)))))

;;;;;;
;;; Workbench

(def function make-test-document/workbench ()
  (workbench/workbench ()
    (workbench/navigator ()
      (make-file-system/pathname (resource-pathname "example/")))
    (workbench/editor ()
      #+nil
      (workbench/document (:title "JSON")
        (make-test-document/json))
      (workbench/document (:title "Text")
        (make-test-document/text))
      (workbench/document (:title "Book")
        (make-test-document/book))
      (workbench/document (:title "XML")
        (make-test-document/xml))
      (workbench/document (:title "JSON")
        (make-test-document/json)))))
