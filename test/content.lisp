;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Graphics

(def function make-test-content/graphics/empty ()
  (make-graphics/canvas nil (make-2d 0 0)))

(def function make-test-content/graphics ()
  (make-graphics/canvas (list (make-graphics/viewport (make-graphics/canvas (list (make-graphics/point (make-2d 50 150) :stroke-color *color/red*)
                                                                                  (make-graphics/line (make-2d 150 300) (make-2d 50 400) :stroke-color *color/blue*)
                                                                                  (make-graphics/rectangle (make-2d 200 200) (make-2d 100 100) :stroke-color *color/green*)
                                                                                  (make-graphics/rounded-rectangle (make-2d 120 180) (make-2d 50 50) 10 :fill-color *color/dark-yellow*)
                                                                                  (make-graphics/polygon (list (make-2d 150 100) (make-2d 160 160) (make-2d 100 150)) :stroke-color *color/black*)
                                                                                  (make-graphics/circle (make-2d 50 250) 50 :stroke-color *color/black* :fill-color *color/blue*)
                                                                                  (make-graphics/ellipse (make-2d 50 50) (make-2d 100 50) :stroke-color *color/red*)
                                                                                  (make-graphics/text (make-2d 200 150) "hello world" :font *font/default* :font-color *color/default* :fill-color *color/light-cyan*)
                                                                                  (make-graphics/image (make-2d 300 0) (make-image/image (asdf:system-relative-pathname :projectured "etc/projectured.png"))))
                                                                            (make-2d 0 0))
                                                      (make-2d 50 50)
                                                      (make-2d 700 400))
                              (make-graphics/rectangle (make-2d 50 50)
                                                       (make-2d 700 400)
                                                       :stroke-color *color/red*))
                        (make-2d 0 0)))

;;;;;;
;;; String

(def function make-test-content/string/empty ()
  "")

(def function make-test-content/string ()
  "just a simple string")

;;;;;;
;;; Styled string

(def function make-test-content/styled-string ()
  (text/text ()
    (text/string "Hello" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)
    (image/image (asdf:system-relative-pathname :projectured "etc/lisp-flag.jpg"))
    (text/string "World" :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/green*)
    (text/newline)
    (text/string "New line" :font *font/ubuntu/bold/24* :font-color *color/solarized/blue*)))

;;;;;;
;;; Text

(def function make-test-content/text/empty ()
  (text/text ()
    (text/paragraph ()
      (text/string ""))))

(def function make-test-content/text ()
  (text/text ()
    (text/paragraph ()
      (text/string "first paragraph in a text document"))
    (text/paragraph ()
      (text/string "second paragraph in a text document"))))

;;;;;;
;;; List

(def function make-test-content/list/empty ()
  (list/list ()))

(def function make-test-content/list ()
  (list/list ()
    (list/element ()
      "first element in a list")
    (list/element ()
      "second element in a list")))

;;;;;;
;;; Table

(def function make-test-content/table/empty ()
  (table/table ()))

(def function make-test-content/table ()
  (table/table ()
    (table/row ()
      (table/cell ()
        "first cell of first row in a table")
      (table/cell ()
        "second cell of first row in a table"))
    (table/row ()
      (table/cell ()
        "first cell of second row in a table (padding)")
      (table/cell ()
        "second cell of second row in a table"))))

;;;;;;
;;; Tree

(def function make-test-content/tree/empty ()
  nil)

(def function make-test-content/tree/leaf ()
  (make-tree/leaf "Hello"))

(def function make-test-content/tree/node ()
  (make-tree/node nil))

(def function make-test-content/tree ()
  (make-tree/node (list (make-tree/leaf "first")
                        (make-tree/node (list (make-tree/leaf "second") (make-tree/leaf "third")))
                        (make-tree/leaf "fourth")
                        (make-tree/node (list (make-tree/leaf "fifth")
                                              (make-tree/node (list (make-tree/leaf "sixth") (make-tree/leaf "seventh")))
                                              (make-tree/leaf "eigth"))))))

;;;;;;
;;; Graph

(def function make-test-content/graph ()
  (bind ((vertex1 (make-graph/vertex "A"))
         (vertex2 (make-graph/vertex "B"))
         (vertex3 (make-graph/vertex "C"))
         (edge12 (make-graph/edge vertex1 vertex2))
         (edge13 (make-graph/edge vertex1 vertex3))
         (edge23 (make-graph/edge vertex2 vertex3)))
    (make-graph/graph (list vertex1 vertex2 vertex3) (list edge12 edge13 edge23))))

;;;;;;
;;; State machine

(def function make-test-content/state-machine ()
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

(def function make-test-content/book/empty ()
  (book/book (:title"")))

(def function make-test-content/book ()
  (book/book (:title "Lorem ipsum" :authors (list "me"))
    (book/chapter (:title "Chapter 1")
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras eu nunc nibh. Cras imperdiet faucibus tortor ac dictum. Aliquam sit amet justo nec ligula lobortis ornare. Aenean a odio id dolor adipiscing interdum. Maecenas nec nisl neque. Suspendisse interdum rutrum neque, in volutpat orci varius in. Praesent a ipsum ac erat pulvinar adipiscing quis sit amet magna. Etiam semper vulputate mi ac interdum. Nunc a tortor non purus fringilla aliquam.")
    (book/chapter (:title "Chapter 2")
      "Morbi scelerisque, felis a viverra pharetra, arcu enim aliquet urna, mollis suscipit felis elit in neque. Aenean vel tempus nulla. Vestibulum magna nisi, cursus vel auctor eu, suscipit sit amet purus. Donec ligula purus, pulvinar id tristique ut, suscipit ornare diam. Maecenas sed justo turpis. Vivamus eu scelerisque dui. Pellentesque mollis rutrum est ac tempus. Sed venenatis, erat id elementum semper, nisl tortor malesuada orci, ac venenatis elit ipsum non augue. Praesent blandit purus est, id venenatis eros. Phasellus non dui dolor. Duis magna erat, pulvinar sed aliquam vitae, porta vel quam.")))

;;;;;;
;;; XML

(def function make-test-content/xml/empty ()
  nil)

(def function make-test-content/xml ()
  (xml/element "person" ((xml/attribute "name" "levy")
                         (xml/attribute "sex" "male"))
    (xml/text "The beginning")
    (xml/element "children" ()
      (xml/element "person" ((xml/attribute "name" "John")
                             (xml/attribute "sex" "female")))
      (xml/element "person" ((xml/attribute "name" "Mary")
                             (xml/attribute "sex" "male"))))
    (xml/element "pets" ())
    (xml/text "The end")))

;;;;;;
;;; JSON

(def function make-test-content/json/empty ()
  (json/nothing))

(def function make-test-content/json ()
  (json/array
    (json/null)
    (json/boolean #f)
    (json/boolean #t)
    (json/number 42)
    (json/string "Hello World")
    (json/object
      ("foo" (json/number 43))
      ("bar" (json/string "Hello World"))
      ("baz" (json/array
               (json/number 44)
               (json/string "Welcome Home"))))))

;;;;;;
;;; Java

(def function make-test-content/java/empty ()
  nil)

(def function make-test-content/java ()
  (make-java/declaration/method (make-java/declaration/qualifier "public")
                                (make-java/declaration/type "int")
                                "factorial"
                                (list (make-java/declaration/argument "n" (make-java/declaration/type "int")))
                                (make-java/statement/block (list (make-java/statement/if (make-java/expression/infix-operator "==" (list (make-java/expression/variable-reference "n")
                                                                                                                                         (make-java/literal/number 0)))
                                                                                         (make-java/statement/return (make-java/literal/number 1))
                                                                                         (make-java/statement/return (make-java/expression/infix-operator "*" (list (make-java/expression/variable-reference "n")
                                                                                                                                                                    (make-java/expression/method-invocation "factorial"
                                                                                                                                                                                                            (list (make-java/expression/infix-operator "-" (list (make-java/expression/variable-reference "n")
                                                                                                                                                                                                                                                                 (make-java/literal/number 1)))))))))))))

;;;;;;
;;; Javascript

(def function make-test-content/javascript ()
  (make-javascript/statement/block nil))

;;;;;;
;;; Lisp form

(def function make-test-content/lisp-form/function (&optional (name 'factorial))
  (ecase name
    (factorial '(defun factorial (n)
                 "Computes the factorial of N"
                 (if (= n 0)
                     1
                     (* n (factorial (- n 1))))))
    (fibonacci '(defun fibonacci (n)
                 "Compute the Nth Fibonacci number"
                 (if (< n 1)
                     1
                     (+ (fibonacci (- n 1))
                        (fibonacci (- n 2))))))
    (quine '(funcall (lambda (lambda) `(,lambda ',lambda))
             '(lambda (lambda) `(,lambda ',lambda))))))

(def function make-test-content/lisp-form/empty ()
  nil)

(def function make-test-content/lisp-form ()
  (make-lisp-form/list (list (make-lisp-form/string "quoted list")
                             (make-lisp-form/number 3.1315)
                             (make-lisp-form/list (list (make-lisp-form/symbol 'sub)
                                                        (make-lisp-form/list (list (make-lisp-form/symbol 'deep) (make-lisp-form/symbol 'list)))
                                                        (make-lisp-form/number 42)
                                                        (make-lisp-form/number 43)))
                             (make-lisp-form/object (make-string-output-stream)))))

;;;;;;
;;; Common lisp

(def function make-test-content/common-lisp/empty ()
  nil)

(def function make-test-content/common-lisp ()
  (make-instance 'common-lisp/function-definition
                 :name 'factorial
                 :bindings (list (make-instance 'common-lisp/required-function-argument :name 'n))
                 :allow-other-keys #f
                 :documentation "Computes the factorial of N"
                 :body nil))

;;;;;;
;;; Evaluator

(def function make-test-content/evaluator ()
  ;; TODO:
  #+nil
  (hu.dwim.walker:walk-form '(* 2 (+ 3 4) (- 5 6))))

;;;;;;
;;; Test

(def test test/factorial ()
  (is (= 1 (factorial 0)))
  (is (= 1 (factorial 1)))
  (is (= 2 (factorial 2)))
  (is (= 6 (factorial 3)))
  (is (= 24 (factorial 4))))

(def function make-test-content/test ()
  (find-test 'test/factorial))

;;;;;;
;;; T

(def function make-test-content/t/empty ()
  nil)

(def function make-test-content/t ()
  (make-xml/element "person"
                    (list (make-xml/attribute "name" "John")
                          (make-xml/attribute "sex" "female"))
                    nil))

;;;;;;
;;; Nested

(def function make-test-content/nested ()
  ;; TODO:
  #+nil
  (bind ((common-lisp (hu.dwim.walker:walk-form '(lambda (name) (if (string= "json" name) nil nil))))
         (if-form (elt (hu.dwim.walker:body-of common-lisp) 0)))
    (setf (hu.dwim.walker:then-of if-form) (make-test-content/json))
    (setf (hu.dwim.walker:else-of if-form) (make-test-content/xml))
    common-lisp))

;;;;;;
;;; Demo

;; KLUDGE:
(def function path-of (request)
  (declare (ignore request)))

;; KLUDGE:
(def function make-http-response (content)
  (declare (ignore content)))

(def function make-test-content/demo ()
  (bind ((page-path (make-adjustable-string "/page"))
         (data-path (make-adjustable-string "/data"))
         (trace-amounts (make-common-lisp/comment
                         (text/text ()
                           (text/string "This part contains trace amounts of " :font projectured::*font/ubuntu/regular/18* :font-color *color/solarized/gray*)
                           (image/image (asdf:system-relative-pathname :projectured "etc/lisp-flag.jpg")))))
         (chart-script (make-javascript/statement/top-level
                        (list (make-javascript/expression/method-invocation
                               (make-javascript/expression/variable-reference "google")
                               "load"
                               (list (make-javascript/literal/string "visualization")
                                     (make-javascript/literal/string "1")
                                     (json/object
                                       ("packages" (json/array (json/string "corechart"))))))
                              (make-javascript/expression/method-invocation
                               (make-javascript/expression/variable-reference "google")
                               "setOnLoadCallback"
                               (list (make-javascript/expression/variable-reference "drawPieChart")))
                              (make-javascript/declaration/function
                               "drawPieChart"
                               nil
                               (make-javascript/statement/block
                                (list (make-javascript/declaration/variable
                                       "json"
                                       (make-javascript/expression/property-access
                                        (make-javascript/expression/method-invocation
                                         (make-javascript/expression/variable-reference "$")
                                         "ajax"
                                         (list (json/object
                                                 ("async" (json/boolean #f))
                                                 ("url" (json/string data-path))
                                                 ("dataType" (json/string "json")))))
                                        "responseText"))
                                      (make-javascript/declaration/variable
                                       "data"
                                       (make-javascript/expression/constuctor-invocation
                                        (make-javascript/expression/property-access
                                         (make-javascript/expression/property-access
                                          (make-javascript/expression/variable-reference "google")
                                          "visualization")
                                         "DataTable")
                                        (list (make-javascript/expression/variable-reference "json"))))
                                      (make-javascript/declaration/variable
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
                                       (list (make-javascript/expression/variable-reference "data")
                                             (json/object
                                               ("title" (json/string "Daily Activities")))))))))))
         (dispatch-table (table/table ()
                           (table/row ()
                             (table/cell ()
                               (text/text ()
                                 (text/string "HTTP request" :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/gray*)))
                             (table/cell ()
                               (text/text ()
                                 (text/string "HTTP response" :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/gray*))))
                           (table/row ()
                             (table/cell ()
                               (text/text ()
                                 (text/string page-path :font *font/default* :font-color *color/solarized/blue*)))
                             (table/cell ()
                               (text/text ()
                                 (text/string "HTML page that contains a pie chart" :font *font/default* :font-color *color/solarized/gray*))))
                           (table/row ()
                             (table/cell ()
                               (text/text ()
                                 (text/string data-path :font *font/default* :font-color *color/solarized/blue*)))
                             (table/cell ()
                               (text/text ()
                                 (text/string "JSON data describing daily activities" :font *font/default* :font-color *color/solarized/gray*))))
                           (table/row ()
                             (table/cell ()
                               (text/text ()
                                 (text/string "<otherwise>" :font *font/default* :font-color *color/solarized/blue*)))
                             (table/cell ()
                               (text/text ()
                                 (text/string "HTML error page" :font *font/default* :font-color *color/solarized/gray*))))))
         (chart-page (xml/element "html" ()
                       (xml/element "head" ()
                         (xml/element "script" ((xml/attribute "type" "text/javascript") (xml/attribute "src" "https://www.google.com/jsapi"))
                           (xml/text ""))
                         (xml/element "script" ((xml/attribute "type" "text/javascript"))
                           chart-script))
                       (xml/element "body" ()
                         (xml/element "h1" ()
                           (xml/text "Pie Chart Example"))
                         (xml/element "div" ((xml/attribute "id" "pie") (xml/attribute "style" "width: 800px; height: 600px;"))
                           (xml/text ""))
                         (xml/element "p" ()
                           (xml/text "Last refresh: ")
                           (make-common-lisp/top-level
                            (list trace-amounts
                                  (make-instance 'common-lisp/application :operator 'local-time:format-timestring
                                                 :arguments (list (make-instance 'common-lisp/constant :value t)
                                                                  (make-instance 'common-lisp/application :operator 'local-time:now :arguments nil)
                                                                  (make-instance 'common-lisp/constant :value :format)
                                                                  (make-instance 'common-lisp/constant :value 'local-time:+asctime-format+)))))))))
         (chart-data (json/array
                       (json/array (json/string "Task") (json/string "Hours per Day"))
                       (json/array (json/string "Work") (json/number 11))
                       (json/array (json/string "Eat") (json/number 2))
                       (json/array (json/string "Commute") (json/number 2))
                       (json/array (json/string "Watch TV") (json/number 2))
                       (json/array (json/string "Sleep")
                                   (make-common-lisp/top-level
                                    (list trace-amounts
                                          (make-instance 'common-lisp/application :operator '-
                                                         :arguments (list (make-instance 'common-lisp/constant :value 24)
                                                                          (make-instance 'common-lisp/application :operator '+
                                                                                         :arguments (list (make-instance 'common-lisp/constant :value 11)
                                                                                                          (make-instance 'common-lisp/constant :value 2)
                                                                                                          (make-instance 'common-lisp/constant :value 2)
                                                                                                          (make-instance 'common-lisp/constant :value 2))))))))))
         (error-page (xml/element "html" ()
                       (xml/element "head" ()
                         (xml/element "title" ()
                           (xml/text "Error 404 (Not Found): ")))
                       (xml/element "body" ()
                         (xml/element "p" ()
                           (xml/text "We are sorry, page ")
                           (make-common-lisp/top-level
                            (list trace-amounts
                                  (make-instance 'common-lisp/variable-reference :name 'path)))
                           (xml/text " cannot be found.")))))
         (lisp-function (make-instance 'common-lisp/function-definition
                                       :name 'process-http-request
                                       :bindings (list (make-instance 'common-lisp/required-function-argument :name 'request))
                                       :allow-other-keys #f
                                       :documentation nil
                                       :body (list (make-common-lisp/comment
                                                    (text/text ()
                                                      (text/string "dispatch on the path of the incoming HTTP request according to the following table" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                      (text/newline)
                                                      dispatch-table))
                                                   (make-instance 'common-lisp/let
                                                                  :bindings (list (make-instance 'common-lisp/lexical-variable-binding :name 'path
                                                                                                 :initial-value (make-instance 'common-lisp/application :operator 'path-of
                                                                                                                               :arguments (list (make-instance 'common-lisp/variable-reference :name 'request)))))
                                                                  :body (list (make-instance 'common-lisp/application :operator 'make-http-response
                                                                                             :arguments (list (make-instance 'common-lisp/if
                                                                                                                             :condition (make-instance 'common-lisp/application :operator 'string=
                                                                                                                                                       :arguments (list (make-instance 'common-lisp/constant :value page-path)
                                                                                                                                                                        (make-instance 'common-lisp/variable-reference :name 'path)))
                                                                                                                             :then chart-page
                                                                                                                             :else (make-instance 'common-lisp/if
                                                                                                                                                  :condition (make-instance 'common-lisp/application :operator 'string=
                                                                                                                                                                            :arguments (list (make-instance 'common-lisp/constant :value data-path)
                                                                                                                                                                                             (make-instance 'common-lisp/variable-reference :name 'path)))
                                                                                                                                                  :then chart-data
                                                                                                                                                  :else error-page))))))))))
    (book/book (:title "ProjecturEd" :authors (list "Levente Mészáros"))
      (book/chapter (:title "Introduction")
        (text/text ()
          (text/string "ProjecturEd" :font *font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string " is a generic purpose projectional editor written in Common Lisp. It provides editing for different problem domains represented in unrestricted arbitrary data structures. It uses multiple bidirectional projections providing different notations varying from textual to graphics." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/newline)
          (image/image (asdf:system-relative-pathname :projectured "etc/lisp-boxed-alien.jpg"))))
      (book/chapter (:title "Literate Programming")
        (text/text ()
          (text/string "This example demonstrates mixing multiple different problem domains in the same document. The document contains" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string " Common Lisp, HTML, JavaScript, JSON, table, image and styled text" :font projectured::*font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string " nested into each other. It describes a web service implemented with a single Common Lisp function that processes HTTP requests. When the function receives a request to the '" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string page-path :font *font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string "' path it sends an HTML page in response. This page contains a pie chart that utilizes the Google Charts JavaScript API. When the pie chart is shown in the browser it sends another request to the '" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string data-path :font *font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string "' path using JavaScript. For this request the web service returns another document in JSON format that provides the data for the pie chart. For all other unknown requests the web service sends an HTML error page. The following screenshot shows how the page looks like." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/newline)
          (image/image (asdf:system-relative-pathname :projectured "etc/pie.png"))
          (text/newline)
          (text/string "This example uses a compound projection that displays all used domains in their natural notation. Proper indentation and syntax highlight are automatically provided without inserting escape sequences that would make reading harder. Note that the edited document" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string " is not text" :font projectured::*font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string " even though it looks like. It's a complex domain specific data structure that precisely captures the intentions. The projections keep track of what is what to make navigation and editing possible." :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*))
        lisp-function)
      (book/chapter (:title "Resources")
        (text/text ()
          (text/string "You can read more about" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string " ProjecturEd" :font *font/ubuntu/italic/18* :font-color *color/solarized/violet*)
          (text/string " at" :font *font/ubuntu/regular/18* :font-color *color/solarized/content/darker*)
          (text/string " http://projectured.org" :font *font/ubuntu/regular/18* :font-color *color/solarized/violet*))))))

;;;;;;
;;; Complex

(def function make-test-content/complex ()
  (make-table/table (list (make-table/row (list (make-table/cell (make-test-content/xml))
                                                (make-table/cell (make-test-content/json))))
                          (make-table/row (list (make-table/cell (make-test-content/java))
                                                (make-table/cell (make-test-content/common-lisp)))))))

;;;;;;
;;; Wow

(def function make-test-content/wow ()
  (book/book (:title "ProjecturEd the Projectional Editor" :authors (list "Levente Mészáros"))
    (book/chapter (:title "Graphics Domain")
      "Some graphics"
      #+nil (make-test-content/graphics))
    (book/chapter (:title "Text Domain")
      "Some text"
      #+nil
      (make-test-content/text))
    (book/chapter (:title "List Domain")
      "Some list"
      #+nil
      (make-test-content/list))
    (book/chapter (:title "Tree Domain")
      "Some tree"
      (make-test-content/tree))
    (book/chapter (:title "Table Domain")
      "Some table"
      #+nil
      (make-test-content/table))
    (book/chapter (:title "JSON Domain")
      "Some JSON"
      (make-test-content/json))
    (book/chapter (:title "XML Domain")
      "Some XML"
      (make-test-content/xml))
    (book/chapter (:title "Java code Domain")
      "Some Java code"
      (make-test-content/java))
    (book/chapter (:title "Javascript code Domain")
      "Some Javascript code"
      (make-test-content/javascript))
    (book/chapter (:title "S-expression Domain")
      "Some Lisp S-expression"
      (make-test-content/lisp-form))
    (book/chapter (:title "Common Lisp code Domain")
      "Some Common Lisp code"
      (make-test-content/common-lisp))
    (book/chapter (:title "Object Domain")
      "Some object"
      #+nil (make-test-content/t))))

;;;;;;
;;; Test

(def suite* (test/content :in test))

(def test test/content/print-document (document)
  (finishes (print-document document (make-string-output-stream))))

(def test test/content/graphics ()
  (test/content/print-document (make-test-content/graphics)))

(def test test/content/string ()
  (test/content/print-document (make-test-content/string)))

(def test test/content/text ()
  (test/content/print-document (make-test-content/text)))

(def test test/content/list ()
  (test/content/print-document (make-test-content/list)))

(def test test/content/table ()
  (test/content/print-document (make-test-content/table)))

(def test test/content/tree ()
  (test/content/print-document (make-test-content/tree)))

(def test test/content/book ()
  (test/content/print-document (make-test-content/book)))

(def test test/content/xml ()
  (test/content/print-document (make-test-content/xml)))

(def test test/content/json ()
  (test/content/print-document (make-test-content/json)))

(def test test/content/java ()
  (test/content/print-document (make-test-content/java)))

(def test test/content/javascript ()
  (test/content/print-document (make-test-content/javascript)))

(def test test/content/lisp-form ()
  (test/content/print-document (make-test-content/lisp-form)))

(def test test/content/common-lisp ()
  (test/content/print-document (make-test-content/common-lisp)))

(def test test/content/evaluator ()
  (test/content/print-document (make-test-content/evaluator)))

(def test test/content/test ()
  (test/content/print-document (make-test-content/test)))
