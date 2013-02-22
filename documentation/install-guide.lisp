;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.documentation)

;;;;;;
;;; Install guide

(def book install-guide (:title "Install guide")
  (chapter (:title "Downloading")
    (paragraph ()
      "Use " (hyperlink "http://www.quicklisp.org/" "Quicklisp") " to download and install the :projectured project on your computer. You will also need to download and install the open source " (hyperlink "http://www.libsdl.org/" "Simple DirectMedia Layer") " libraries with your package manager."))
  (chapter (:title "Compiling")
    (paragraph ()
      "Compile and load the :projectured.sdl " (hyperlink "http://common-lisp.net/project/asdf/" "ASDF") " system with the usual commands."))
  (chapter (:title "Testing")
    (paragraph ()
      "Run the functions from the :projectured.test package that have 'test' in their name as a prefix."))
  (chapter (:title "Common Lisp Implementation")
    (paragraph ()
      "Tested only with SBCL x86-64."))
  (chapter (:title "Operating System")
    (paragraph ()
      "Tested only under Ubuntu Linux.")))
