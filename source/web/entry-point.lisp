;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;; TODO: use font from CSS
(def file-serving-entry-points *projectional-editor-application*
  ("static/" (asdf:system-relative-pathname :projectured "etc/")))

(def entry-point (*projectional-editor-application* :path "projectional-editor")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-buffered-functional-html-response ((+header/status+ +http-ok+))
      (render-examples))))

(def entry-point (*projectional-editor-application* :path "projectional-editor/example/demo")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-buffered-functional-html-response ((+header/status+ +http-ok+))
      (bind (((editor document projection) (or (root-component-of *frame*)
                                               (setf (root-component-of *frame*)
                                                     (bind ((name (string-upcase (first *entry-point-relative-path*)))
                                                            (editor (make-editor))
                                                            (document (funcall (format-symbol :projectured.test "MAKE-TEST-DOCUMENT/~A" name)))
                                                            (projection (funcall (format-symbol :projectured.test "MAKE-TEST-PROJECTION/~A->GRAPHICS" name))))
                                                       (list editor document projection))))))
        (catch :quit-editor
          (setf (printer-iomap-of editor) (apply-printer document projection))
          (funcall (read-from-devices editor document projection)))
        (render-document (printer-iomap-of editor))))))

(def entry-point (*projectional-editor-application* :path "projectional-editor/example/document")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-buffered-functional-html-response ((+header/status+ +http-ok+))
      (bind (((document projection) (bind ((name (string-upcase (first *entry-point-relative-path*)))
                                           (document (funcall (format-symbol :projectured.test "MAKE-TEST-DOCUMENT/~A" name)))
                                           (projection (funcall (find-symbol "MAKE-TEST-PROJECTION/T->GRAPHICS" :projectured.test))))
                                      (list document projection))))
        (render-document (apply-printer document projection))))))

(def class* example ()
  ((name :type string)
   (description :type string)
   (document :type document)
   (projection :type projection)))

(def function make-example (name description)
  (make-instance 'example
                 :name name :description description
                 :document (funcall (format-symbol :projectured.test "MAKE-TEST-DOCUMENT/~A" (string-upcase name)))
                 :projection (funcall (format-symbol :projectured.test "MAKE-TEST-PROJECTION/~A->GRAPHICS" (string-upcase name)))))

(def function collect-examples ()
  (list (make-example "String" "The most trivial example is a simple string. In this example the document is directly projected to graphics.")
        (make-example "Graphics" "This example demonstrates the graphics data structure. The graphics document does not need to be projected, becasuse it is already in the language of the output device.")
        (make-example "Text" "This example demonstrates the styled text data structure. Characters may have foreground color, background color and font. The text document is directly projected to graphics.")
        (make-example "List" "TODO")
        (make-example "Table" "TODO")
        (make-example "Tree" "TODO")
        (make-example "State machine" "TODO")
        (make-example "Book" "TODO")
        (make-example "JSON" "The JSON format is a widely used and well documented syntax for a non trivial data structure.")
        (make-example "XML" "TODO")
        (make-example "Java" "TODO")
        (make-example "Javascript" "TODO")
        (make-example "lisp-form" "TODO")
        (make-example "common-lisp" "TODO")
        #+nil (make-example "nested" "TODO")
        #+nil (make-example "complex" "TODO")
        #+nil (make-example "t" "TODO")))

(def function render-examples ()
  <html <body <h1 "Examples">
              <p "The following chapters demonstrate the projectional editor through a couple of examples. The first few of them are trivial, but later on things get more complicated. This organization helps understanding how the projectional editor works.">
              <p "Each example has a predefined input document that is a domain specific data structure. They also have a predefined projection that is used to display and edit the document. The projection of the input document is done in multiple steps, where each step creates a new document. These documents are instances of different domain specific data structures. The last document is in the domain specific language of the output device, in this case the screen. Links are provided to view the input document, all intermediate documents, and the document for the output device.">
              <p "There is also a link to an online demo that shows how the projectional editor works. These demos actually run almost completely on the server. The only part that runs on the client is the reading the input, and printing the output. This part is written in javascript, and it handles reading keyboard and mouse events, and drawing on a 2D canvas.">
              ,(iter (for index :from 1)
                     (for example :in (collect-examples))
                     (for name = (name-of example))
                     (for projection = (projection-of example))
                     <h2 ,(format nil "Example ~A: ~A" index name)>
                     <p ,(description-of example)>
                     <p <a (:href ,(format nil "/projectional-editor/example/document/~A" (string-downcase name))) ,(string-downcase name) >
                        ,@(iter (for element :in (labels ((collect-atomic-projections (projection)
                                                            (if (typep projection 'sequential)
                                                                (mappend #'collect-atomic-projections (elements-of projection))
                                                                (list projection))))
                                                   (collect-atomic-projections projection)))
                                (for projection-name = (string-downcase (class-name (class-of element))))
                                (for document-name = (subseq projection-name (+ (search "->" projection-name) 4)))
                                `str(" ")
                                <a (:href "") ,document-name>)
                        " "
                        <br>
                        <a (:href ,(format nil "/projectional-editor/example/demo/~A" (string-downcase name))) "demo">>)>>)

(def function render-document (printer-iomap)
  <html <head <script (:type "text/javascript")
                      "function onKeyDown(e) {
                          var form = document.getElementById('form');
                          form.elements['event-type'].value = 'keyDown';
                          form.elements['key-code'].value = e.keyCode;
                          form.submit();
                          return false;
                       }
                      function onKeyUp(e) {
                          var form = document.getElementById('form');
                          form.elements['event-type'].value = 'keyUp';
                          form.elements['key-code'].value = e.keyCode;
                          // TODO: form.submit();
                          return false;
                       }
                       function onLoad() {
                          var canvas = document.getElementById('canvas');
                          canvas.addEventListener('keydown', onKeyDown, false);
                          canvas.addEventListener('keyup', onKeyUp, false);
                          canvas.focus();
                          var context = canvas.getContext('2d');
                          context.textBaseline = 'top';
                          context.font = '18px Inconsolata';"
                      ,(print-to-device (output-of printer-iomap) (make-device/display/web 1024 768))
                      "}">>
        <body (:onload "onLoad()")
              <form (:id "form" :method "POST")
                    <input (:id "event-type" :type "hidden" :name "even-type")>
                    <input (:id "key-code" :type "hidden" :name "key-code")>>
              <canvas (:id "canvas" :width "1000" :height "1000" :tabindex 0)>>>)
