;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def function make-file-document (filename)
  (bind ((content (if filename
                      (if (probe-file filename)
                          (call-loader filename)
                          (call-maker filename))
                      (funcall (find-maker 'pred))))
         (title (if filename
                    (file-namestring filename)
                    "document.pred")))
    (workbench/document (:title title :filename (or filename "document.pred") :selection `((the ,(document-type content) (content-of (the workbench/document document)))))
      content)))

(def function make-output-document (document)
  (output/display ()
    (output/window (:position (make-2d 200 200) :size (make-2d 1280 720))
      document)))

(def function make-widget-document (document)
  (widget/shell (:size (make-2d 1280 720)
                 :border (make-inset :all 5)
                 :border-color *color/solarized/background/lighter*
                 :selection '((the widget/scroll-pane (content-of (the widget/shell document)))))
    (widget/scroll-pane (:size (make-2d 1260 700)
                         :padding (make-inset :all 5)
                         :padding-color *color/white*
                         :selection `((the ,(document-type document) (content-of (the widget/scroll-pane document)))))
      document)))

(def function make-workbench-document (document)
  (workbench/workbench (:selection '((the workbench/page (editing-page-of (the workbench/workbench document)))))
    (workbench/page (:selection '((the sequence (elements-of (the workbench/page document)))
                                  (the workbench/navigator (elt (the sequence document) 0))))
      (workbench/navigator (:selection '((the sequence (folders-of (the workbench/navigator document)))
                                         (the file-system/directory (elt (the sequence document) 0))))
        (make-file-system/pathname (system-relative-pathname :projectured.executable "example/"))))
    (workbench/page (:selection '((the sequence (elements-of (the workbench/page document)))
                                  (the workbench/document (elt (the sequence document) 0))))
      document)
    (workbench/page (:selection '((the sequence (elements-of (the workbench/page document)))
                                  (the workbench/navigator (elt (the sequence document) 0))))
      ;; TODO: kills laziness, because output starts to depend on selection, and selection is not lazy due to cons lists
      #+nil (workbench/descriptor () (as (document/reference () (get-selection document))))
      (workbench/console ()
        (text/text ()
          (text/string "Welcome to ProjecturEd!" :font *font/ubuntu/regular/14* :font-color *color/default*)))
      (workbench/operator ())
      (workbench/searcher ())
      (workbench/evaluator (:selection '((the evaluator/toplevel (content-of (the workbench/evaluator document)))))
        (evaluator/toplevel (:selection '((the sequence (elements-of (the evaluator/toplevel document)))
                                          (the evaluator/form (elt (the sequence document) 0))))
          (evaluator/form (:selection '((the common-lisp/insertion (form-of (the evaluator/form document)))))
            (make-common-lisp/insertion "" 'common-lisp/complete-document
                                        :default-value "enter form"
                                        :selection '((the string (value-of (the common-lisp/insertion document)))
                                                     (the string (subseq (the string document) 0 0))))))))))

(def function make-default-document (document)
  (make-workbench-document document))

;;;;;;
;;; Example

(def function make-graphics-example ()
  (make-graphics/canvas (list (make-graphics/viewport (make-graphics/canvas (list (make-graphics/point (make-2d 50 150) :stroke-color *color/red*)
                                                                                  (make-graphics/line (make-2d 150 300) (make-2d 50 400) :stroke-color *color/blue*)
                                                                                  (make-graphics/rectangle (make-2d 200 200) (make-2d 100 100) :stroke-color *color/green*)
                                                                                  (make-graphics/rounded-rectangle (make-2d 120 180) (make-2d 50 50) 10 :fill-color *color/dark-yellow*)
                                                                                  (make-graphics/polygon (list (make-2d 150 100) (make-2d 160 160) (make-2d 100 150)) :stroke-color *color/black*)
                                                                                  (make-graphics/bezier (list (make-2d 100 100) (make-2d 200 120) (make-2d 180 200) (make-2d 300 200)) :stroke-color *color/black*)
                                                                                  (make-graphics/circle (make-2d 50 250) 50 :stroke-color *color/black* :fill-color *color/blue*)
                                                                                  (make-graphics/ellipse (make-2d 50 50) (make-2d 100 50) :stroke-color *color/red*)
                                                                                  (make-graphics/text (make-2d 200 150) "hello world" :font *font/default* :font-color *color/default* :fill-color *color/light-cyan*)
                                                                                  (make-graphics/image (make-2d 300 0) (make-image/file (resource-pathname "image/projectured.png"))))
                                                                            0)
                                                      (make-2d 50 50)
                                                      (make-2d 700 400))
                              (make-graphics/rectangle (make-2d 50 50)
                                                       (make-2d 700 400)
                                                       :stroke-color *color/red*))
                        0))

(def function save-graphics-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/graphics.pred") (make-graphics-example)))

(def function make-mixed-example ()
  (book/book (:title "Example")
    (book/chapter (:title "Text")
      (book/paragraph (:alignment :justified)
        (call-loader (system-relative-pathname :projectured.executable "example/lorem-ipsum.txt"))))
    (book/chapter (:title "JSON")
      (call-loader (system-relative-pathname :projectured.executable "example/contact-list.json")))
    (book/chapter (:title "XML")
      (call-loader (system-relative-pathname :projectured.executable "example/hello-world.html")))))

(def function save-mixed-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/mixed.pred") (make-mixed-example)))

(def function make-factorial-example ()
  (bind ((factorial-argument (make-common-lisp/required-function-argument (make-s-expression/symbol* 'num)))
         (factorial-function (make-common-lisp/function-definition (make-s-expression/symbol "FACTORIAL" "COMMON-LISP-USER")
                                                                   (list-ll factorial-argument)
                                                                   nil
                                                                   :allow-other-keys #f
                                                                   :documentation "The FACTORIAL function computes the product of all integers between 1 and NUM.")))
    (setf (body-of factorial-function)
          (list-ll (make-common-lisp/if (make-common-lisp/application (make-s-expression/symbol* '<)
                                                                      (list-ll (make-common-lisp/variable-reference factorial-argument)
                                                                               (make-common-lisp/constant* 2)))
                                        (make-common-lisp/constant* 1)
                                        (make-common-lisp/application (make-s-expression/symbol* '*)
                                                                      (list-ll (make-common-lisp/variable-reference factorial-argument)
                                                                               (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                                                                                             (list-ll (make-common-lisp/application
                                                                                                                       (make-s-expression/symbol* '-)
                                                                                                                       (list-ll (make-common-lisp/variable-reference factorial-argument)
                                                                                                                                (make-common-lisp/constant* 1))))))))))
    factorial-function))

(def function save-factorial-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/factorial.pred") (make-factorial-example)))

(def function make-fibonacci-example (&optional suffix)
  (bind ((fibonacci-argument (make-common-lisp/required-function-argument (make-s-expression/symbol* 'num)))
         (fibonacci-function (make-common-lisp/function-definition (make-s-expression/symbol (string+ "FIBONACCI" suffix) "COMMON-LISP-USER")
                                                                   (list-ll fibonacci-argument)
                                                                   nil
                                                                   :allow-other-keys #f
                                                                   :documentation "The FIBONACCI function computes an element of the Fibonacci series at the index determined by NUM.")))
    (setf (body-of fibonacci-function)
          (list-ll (make-common-lisp/if (make-common-lisp/application (make-s-expression/symbol* '<)
                                                                      (list-ll (make-common-lisp/variable-reference fibonacci-argument)
                                                                               (make-common-lisp/constant* 2)))
                                        (make-common-lisp/variable-reference fibonacci-argument)
                                        (make-common-lisp/application (make-s-expression/symbol* '+)
                                                                      (list-ll (make-common-lisp/application (make-common-lisp/function-reference fibonacci-function)
                                                                                                             (list-ll (make-common-lisp/application
                                                                                                                       (make-s-expression/symbol* '-)
                                                                                                                       (list-ll (make-common-lisp/variable-reference fibonacci-argument)
                                                                                                                                (make-common-lisp/constant* 1)))))
                                                                               (make-common-lisp/application (make-common-lisp/function-reference fibonacci-function)
                                                                                                             (list-ll (make-common-lisp/application
                                                                                                                       (make-s-expression/symbol* '-)
                                                                                                                       (list-ll (make-common-lisp/variable-reference fibonacci-argument)
                                                                                                                                (make-common-lisp/constant* 2))))))))))
    fibonacci-function))

(def function save-fibonacci-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/fibonacci.pred") (make-fibonacci-example)))

(def function make-odd-even-example ()
  (bind ((odd-argument (make-common-lisp/required-function-argument (make-s-expression/symbol* 'num)))
         (odd-function (make-common-lisp/function-definition (make-s-expression/symbol "ODD?" "COMMON-LISP-USER")
                                                                   (list-ll odd-argument)
                                                                   nil
                                                                   :allow-other-keys #f
                                                                   :documentation "The ODD? function eturns T if NUM is not divisible by 2, returns NIL otherwise."))
         (even-argument (make-common-lisp/required-function-argument (make-s-expression/symbol* 'num)))
         (even-function (make-common-lisp/function-definition (make-s-expression/symbol "EVEN?" "COMMON-LISP-USER")
                                                                   (list-ll even-argument)
                                                                   nil
                                                                   :allow-other-keys #f
                                                                   :documentation "The EVEN? function returns T if NUM is divisible by 2, returns NIL otherwise.")))
    (setf (body-of odd-function)
          (list-ll (make-common-lisp/if (make-common-lisp/application (make-s-expression/symbol* '=)
                                                                      (list-ll (make-common-lisp/variable-reference odd-argument)
                                                                               (make-common-lisp/constant* 0)))
                                        (make-common-lisp/constant* nil)
                                        (make-common-lisp/application (make-common-lisp/function-reference even-function)
                                                                      (list-ll (make-common-lisp/application
                                                                                (make-s-expression/symbol* '-)
                                                                                (list-ll (make-common-lisp/variable-reference odd-argument)
                                                                                         (make-common-lisp/constant* 1))))))))
    (setf (body-of even-function)
          (list-ll (make-common-lisp/if (make-common-lisp/application (make-s-expression/symbol* '=)
                                                                      (list-ll (make-common-lisp/variable-reference even-argument)
                                                                               (make-common-lisp/constant* 0)))
                                        (make-common-lisp/constant* nil)
                                        (make-common-lisp/application (make-common-lisp/function-reference odd-function)
                                                                      (list-ll (make-common-lisp/application
                                                                                (make-s-expression/symbol* '-)
                                                                                (list-ll (make-common-lisp/variable-reference even-argument)
                                                                                         (make-common-lisp/constant* 1))))))))
    even-function))

(def function save-odd-even-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/odd-even.pred") (make-odd-even-example)))

(def function make-evaluator-example ()
  (bind ((factorial-function (make-factorial-example)))
    (evaluator/toplevel ()
      (evaluator/form ()
        factorial-function)
      (evaluator/form ()
        (make-common-lisp/application (make-common-lisp/function-reference factorial-function)
                                      (list-ll (make-common-lisp/constant* 12)))))))

(def function save-evaluator-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/evaluator.pred") (make-evaluator-example)))

(def function make-scaling-example (&optional (function-count 100))
  (bind ((fibonacci-functions-map (make-hash-table))
         (fibonacci-functions (iter (for i :from 0 :below function-count)
                                    (for fibonacci-function = (make-fibonacci-example (format nil "~4,'0,d" i)))
                                    (setf (gethash i fibonacci-functions-map) fibonacci-function)
                                    (collect fibonacci-function))))
    (iter (for fibonacci-function :in fibonacci-functions)
          (setf (function-of (operator-of (elt (arguments-of (else-of (elt (body-of fibonacci-function) 0))) 0)))
                (gethash (random function-count) fibonacci-functions-map))
          (setf (function-of (operator-of (elt (arguments-of (else-of (elt (body-of fibonacci-function) 0))) 1)))
                (gethash (random function-count) fibonacci-functions-map)))
    (make-common-lisp/toplevel (ll fibonacci-functions))))

(def function save-scaling-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/scaling.pred") (make-scaling-example)))

(def function make-user-guide-example ()
  (book/book (:title "User Guide")
    (book/chapter (:title "Introduction")
      (book/paragraph ()
        (text/text ()
          (text/string "Welcome to ProjecturEd, a generic purpose projectional editor." :font *font/liberation/serif/regular/24* :font-color *color/solarized/content/darker*))))))

(def function save-user-guide-example ()
  (call-saver (system-relative-pathname :projectured.executable "example/user-guide.pred") (make-user-guide-example)))
