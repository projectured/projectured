;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Text test

(def suite* (test/text :in test))

(def test test/text/position ()
  (is (= 0 (text/position-element (text/make-position 0 1))))
  (is (= 1 (text/position-index (text/make-position 0 1))))
  (is (text/position= (text/make-position 0 1) (text/make-position 0 1)))
  (is (not (text/position= (text/make-position 0 1) (text/make-position 1 0)))))

(def test test/text/previous-position (&optional (text (make-test-document/text)))
  (iter (for position :initially (text/first-position text) :then (text/next-position text position))
        (while position)
        (for previous-position :previous position)
        (when previous-position
          (is (text/position= previous-position (text/previous-position text position))))))

(def test test/text/next-position (&optional (text (make-test-document/text)))
  (iter (for position :initially (text/last-position text) :then (text/previous-position text position))
        (while position)
        (for next-position :previous position)
        (when next-position
          (is (text/position= next-position (text/next-position text position))))))

(def test test/text/empty ()
  (bind ((empty-text (text/text ())))
    (is (text/empty? empty-text))
    (is (null (text/origin-position empty-text)))
    (is (null (text/first-position empty-text)))
    (is (null (text/last-position empty-text)))))
