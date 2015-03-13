;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Text domain test suite

(def suite* (test/text :in test))

(def special-variable *test/text/empty* (text/text () (text/string "")))

(def special-variable *test/text/one-word* (text/text () (text/string "Hello")))

(def special-variable *test/text/one-line* (text/text () (text/string "Hello") (text/string " ") (text/string "World")))

(def special-variable *test/text/two-lines* (text/text () (text/string "Hello") (text/newline) (text/string "World")))

(def special-variable *test/text/weird* (text/text () (text/string "Hello") (text/string "") (text/string "World")))

(def test test/text/position ()
  (is (= 0 (text/position-element (text/make-position :element 0 :index 1))))
  (is (= 1 (text/position-index (text/make-position :element 0 :index 1))))
  (is (text/position= (text/make-position :element 0 :index 1) (text/make-position :element 0 :index 1)))
  (is (not (text/position= (text/make-position :element 0 :index 1) (text/make-position :element 1 :index 0)))))

(def test test/text/origin-position ()
  (is (text/origin-position *test/text/empty*))
  (is (not (text/previous-position *test/text/empty* (text/origin-position *test/text/empty*))))
  (is (not (text/next-position *test/text/empty* (text/origin-position *test/text/empty*)))))

(def test test/text/first-position (&optional (text *test/text/two-lines*))
  (is (text/first-position text))
  (is (not (text/previous-position text (text/first-position text)))))

(def test test/text/last-position (&optional (text *test/text/two-lines*))
  (is (text/last-position text))
  (is (not (text/next-position text (text/last-position text)))))

(def test test/text/previous-position (&optional (text *test/text/two-lines*))
  (iter (for position :initially (text/first-position text) :then (text/next-position text position))
        (while position)
        (for previous-position :previous position)
        (when previous-position
          (is (text/position= previous-position (text/previous-position text position))))))

(def test test/text/next-position (&optional (text *test/text/two-lines*))
  (iter (for position :initially (text/last-position text) :then (text/previous-position text position))
        (while position)
        (for next-position :previous position)
        (when next-position
          (is (text/position= next-position (text/next-position text position))))))

(def test test/text/line-start-position (&optional (text *test/text/two-lines*))
  (is (text/position= (text/first-position text) (text/line-start-position text (text/first-position text)))))

(def test test/text/line-end-position (&optional (text *test/text/two-lines*))
  (is (text/position= (text/last-position text) (text/line-end-position text (text/last-position text)))))

(def test test/text/previous-character (&optional (text *test/text/two-lines*))
  (is (not (text/previous-character text (text/first-position text))))
  (iter (for position :initially (text/first-position text) :then (text/next-position text position))
        (while position)
        (for previous-position :previous position)
        (when previous-position
          (is (eql (text/next-character text previous-position) (text/previous-character text position))))))

(def test test/text/next-character (&optional (text *test/text/two-lines*))
  (is (not (text/next-character text (text/last-position text))))
  (iter (for position :initially (text/last-position text) :then (text/previous-position text position))
        (while position)
        (for next-position :previous position)
        (when next-position
          (is (eql (text/previous-character text next-position) (text/next-character text position))))))

(def test test/text/first-element (&optional (text *test/text/two-lines*))
  (is (not (text/previous-element text (text/first-element text)))))

(def test test/text/last-element (&optional (text *test/text/two-lines*))
  (is (not (text/next-element text (text/last-element text)))))

(def test test/text/element ()
  (is (eql (text/first-element *test/text/one-word*) (text/last-element *test/text/one-word*))))

(def test test/text/length ()
  (is (= 0 (text/length *test/text/empty*) (length (text/as-string *test/text/empty*))))
  (is (= 5 (text/length *test/text/one-word*) (length (text/as-string *test/text/one-word*))))
  (is (= 11 (text/length *test/text/one-line*) (length (text/as-string *test/text/one-line*))))
  (is (= 11 (text/length *test/text/two-lines*) (length (text/as-string *test/text/two-lines*))))
  (is (= 10 (text/length *test/text/weird*) (length (text/as-string *test/text/weird*)))))

(def test test/text/empty? ()
  (is (text/empty? *test/text/empty*))
  (is (not (text/empty? *test/text/one-word*)))
  (is (not (text/empty? *test/text/one-line*)))
  (is (not (text/empty? *test/text/two-lines*))))

(def test test/text/count ()
  (is (= 0 (text/count *test/text/empty* #\Space)))
  (is (= 0 (text/count *test/text/one-word* #\Space)))
  (is (= 1 (text/count *test/text/one-line* #\Space)))
  (is (= 0 (text/count *test/text/two-lines* #\Space))))

(def test test/text/count-lines ()
  (is (= 1 (text/count-lines *test/text/empty*)))
  (is (= 1 (text/count-lines *test/text/one-word*)))
  (is (= 1 (text/count-lines *test/text/one-line*)))
  (is (= 2 (text/count-lines *test/text/two-lines*))))

(def test test/text/as-string ()
  (is (string= "" (text/as-string *test/text/empty*)))
  (is (string= "Hello" (text/as-string *test/text/one-word*)))
  (is (string= "Hello World" (text/as-string *test/text/one-line*)))
  (is (string= "Hello
World" (text/as-string *test/text/two-lines*))))
