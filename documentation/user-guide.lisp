;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.documentation)

;;;;;;
;;; User guide

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "A " (hyperlink/wikipedia "Text_editor" "text editor") " is a great tool when it comes to editing a text document. The basic data structure of a text document is pretty simple: it is an ordered sequence of characters including line breaks. A somewhat more sophisticated text editor extends this data structure with text style such as colors and various font properties. A word processing editor further extends the data structure with chapters, bulleted lists, tables and so on. Such a text document data structure is a very powerful one, because it can express many kind of data, and it is also very convenient for human processing and human communication. Nevertheless, there is a very important problem with a text document. It is either very difficult to process it by a program or to follow the syntax rules by a human.")
    (paragraph ()
      "A " (hyperlink/wikipedia "Structured_editor" "projectional editor") " differs from a text editor in that it is a general purpose editor with respect to the data structure being edited. The text editor is basically a special purpose projectional editor for a text document. A projectional editor removes the difficulty of following the syntax rules from the human. It allows to present the document in multiple different views while editing. A structured document is also easy to process programmatically. The difficulty lies in the implementation of the projectional editor, because it needs to be in par with the very high usability of a text editor."))
  (chapter (:title "Concepts")
    (paragraph ()
      "")))
