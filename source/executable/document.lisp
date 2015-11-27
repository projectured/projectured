;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def function make-initial-document (filename)
  (widget/shell (:size (make-2d 1280 720))
    (widget/scroll-pane (:position 0 :size (make-2d 1280 720) :margin (make-inset :all 5))
      (document/clipboard ()
        (if filename
            (if (probe-file filename)
                (document/document (:filename filename)
                  (call-loader filename))
                (document/document (:filename filename)
                  (call-maker filename)))
            (document/document (:filename "document.pred")
              (funcall (find-maker 'pred))))))))

(def function make-mixed-example-document ()
  (book/book (:title "Example")
    (book/chapter (:title "Text")
      (book/paragraph (:alignment :justified)
        (call-loader (system-relative-pathname :projectured.executable "example/lorem-ipsum.txt"))))
    (book/chapter (:title "JSON")
      (call-loader (system-relative-pathname :projectured.executable "example/contact-list.json")))
    (book/chapter (:title "XML")
      (call-loader (system-relative-pathname :projectured.executable "example/hello-world.html")))))

(def function save-mixed-example-document ()
  (call-saver (system-relative-pathname :projectured.executable "example/mixed.pred") (make-mixed-example-document)))
