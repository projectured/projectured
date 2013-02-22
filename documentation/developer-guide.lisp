;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.documentation)

;;;;;;
;;; Developer guide

#|
Projectional Editor

Q: What is a projectional editor?
A: It is a general purpose editor that provides multiple ways to edit arbitrary data structures.

Q: Why is the editor called projectional?
A: Because it provides its editing capabilities using bidirectional projections between domains.

Q: How does it work?
A: It communicates with the user in a pretty similar way to the well known read-eval-print loop.

Q: What is a read-eval-print loop?
A: Wikipedia explains it better than me: http://en.wikipedia.org/wiki/Read-eval-print_loop

Q: How does the editor utilize a REPL?
A: The main difference from the original REPL is that it has to handle different kind of input and output devices at once. These devices include among others a keyboard, a mouse and a graphical display.

Q: What does the REPL print?
A: The original REPL prints characters to the single output device. The generalized REPL prints (projects) the editors state to all output devices at the same time.

Q: Can you tell some print examples?
A: Printing to a graphical display needs a projection from the domain being edited to the graphics domain.

Q: What does the REPL read?
A: The original REPL reads characters from the single input device. The generalized REPL reads (projects) from the state of all input devices at the same time.

Q: Can you tell some read examples?
A: Reading from a mouse needs a projection from the mouse domain to the domain being edited.

Q: What does the REPL evaluate?
A: TODO: The document?

Q: What is a domain?
A: It describes the data structures of documents, selections and operations that belong to a problem domain.

Q: Why domains are important?
A: Because they can be analyzed and described independently.

Q: Can you tell some domain exapmles?
A: boolean, integer, number, string, pair, tuple, list, tree, graph, map, etc.
A: XML, JSON, ASN.1, etc.
A: C code, Java code, Lisp code, etc.
A: styled text, word processing, spreadsheet, presentation, graphics, etc.
A: relational data, object oriented data, etc.

Q: How can integer be a domain?
A: Integer is also a compound data structure consisting of digits or bits if you like, and it can be edited.

Q: What is a document data structure?
A: It describes the possible states of domain specific documents.

Q: Can you tell some document data structure examples?
A: integer domain: an integer consists of a sequence of bits.
A: string domain: a string consists of a sequence of characters.
A: list domain: a list consists of a sequence of arbitrary elements.
A: XML domain: an XML element consists of a list of XML attributes and a list of XML child elements, an XML attribute consists of a name string and a value string, etc.
A: graphics domain: a point consists of 2D coordinate and a color, a line segment consists of two 2D coordinates, a color and a thickness, etc.

Q: What is a document?
A: It is a piece of data that is being edited.

Q: Can you tell some document examples?
A: true, 12, "", "hello", "hello world", (1, 1, 2, 3, 5), etc.
A: <person name="levy"/>, {"name": "levy" birthday: "08/05"}, etc.
A: public static void main(String[] args), etc.

Q: What is a selection data structure?
A: It describes the possible states of domain specific selections.

Q: Can you tell some selection data structure examples?
A: string domain: a character position selection consists of a string reference and an integer in the range [0, length].
A: XML domain: an element range selection consists of a parent element reference and two integers in the range [0, child count).
A: Java code domain: a method selection consists of a method reference.

Q: What is a selection?
A: It specifies some part of a document that is being edited.

Q: Can you tell some selection examples?
A: integer domain: a bit selection referring to the 1st bit of the integer 42.
A: string domain: a character position selection referring to the 3rd position (between the 'l' characters) of "hello world".
A: graph domain: a subgraph selection referring to vertices A, B and C including the edges between them.

Q: What is an operation data structure?
A: It describes the possible states of domain specific operations.

Q: Can you tell some operation data structure examples?
A: integer domain: bit negation operation consists of a bit selection.
A: string domain: delete character operation consists of a character selection.
A: list domain: replace element range operation consists of an element range selection and a replacement list document

Q: What is an operation?
A: It specifies a way of modifying a document using selections and other documents.

Q: Can you tell some operation examples?
A: integer domain: a negate bit operation of a bit referred by a bit selection.
A: graph domain: an add edge operation between two vertices referred by two vertex selections.

Q: TODO: What is a projection?
A: It is a transformation from one domain to another.

Q: TODO: What does a projection transform?
A: The domain specific data structures of documents, selections and operations.

THE MAIN ASPECTS OF THE DOCUMENT VISUAL REPRESENTATION
 - content!
 - position/size/thickness/layout/scale/rotation
 - solid/dotted/dashed/pattern
 - font-family/bold/italic/underline
 - color/foreground-color/background-color/gradient
 - icon/image/animation/video

hardware/device/input/output
mouse/keyboard/focus
display/character-display/pixel-display
event/mouse-event/keyboard-event
gesture/mouse-gesture/keyboard-gesture
operation/undo/redo
graphics/drawing
selection/primary/secondary/highlight/named
outline/expand/collapse
domain/tree/text/graphics

model/printer/reader/repl

model notifies the printer about changes (model -> printer)
printer speaks to hardware (printer -> output hardware devices)
hardware speaks to reader (input hardware devices -> reader]
reader notifies model about operations (reader -> model)

internal flow of information: input hardware devices -> reader -> model -> printer -> output hardware devices
external flow of information: output hardware devices -> senses -> brain -> hands -> input hardware devices

model API:
 - constructor functions for data (unique parameterization for all valid states)
 - a function that checks if a model data is valid or not
 - traversal functions for data
 - constructor functions for operations (e.g. reverse operation)
 - execute operations
 - handle invalid states during editing, represent intermediate states

printer API:
 - a function that writes the model data to an output hardware device
 - may recurse into the model data and further filter, transform the result
 - the final result can be directly output to the hardware device

reader API:
 - a function that reads operations from an input hardware device
 - may recurse into the model data and further filter, transform the result
 - the final result is an operation that can be executed on the model data
   it can be a compound operation or even an operation that is inserting ambigously
   identified operations into the model data for further user interaction

a domain consists of the following domain specific data structures:
 - a document represents the primary data
 - a selection represents an argument for operations
 - an operation represents a way of modifying the domain specific data structures

domains can be mixed by nesting, interleaving and other means of referencing


the state of the editor:
 - the thing that is being edited - the document (any kind of data, we have no influence on this part)
 - the description of how the document is being edited - the projection parameters (the printer and the reader)
 - the state of the editing - the selections, the pendings, etc.


;; simple reference
'(foo-of (bar-of document))

;; simple projection
`(project-document document ,projection)

;; first projection, then reference
`(foo-of (bar-of (project-document document ,projection)))

;; first reference, then projection
`(project-reference ,document ,projection '(foo-of (bar-of document)))
|#

(def book developer-guide (:title "Developer guide")
  (chapter (:title "Editor")
    (paragraph ()
      "The generic purpose projectional editor provides multiple ways of editing arbitrary data structures. The editor communicates with the user in a pretty similary way to the well known " (hyperlink/wikipedia "Read-eval-print_loop" "read-eval-print loop") ". First, it prints the document to the output devices, then it reads an operation from the input devices, then it evaluates the operation that may have side effects on the document that is being edited, and finally it starts the whole loop over. The difference to a standard REPL is that it has to handle different kind of input and output devices such as a keyboard, a mouse, a timer and a graphical display at once. Surprisingly the REPL algorithm is so general that it is not very different from what a human does when she communicates with a computer or just another human.")
    (paragraph ()
      "A structure editor is also often called a projectional editor. This aspect is realized by the printer and the reader parts of the REPL as a multi step bidirectional projection. In the output direction information flows from the document towards the output devices. In the input direction information flows from the input devices towards the operation.")
    (paragraph ()
      "Being general purpose also means that the editor must be able to handle multiple " (hyperlink/wikipedia "Domain-specific_language" "domains") ". Having support for multiple builtin domains and being a projectional editor allows a nice way of extending the editor with a new domain. The extension will have to provide a bidirectional projection to an already supported domain. That means it will provide a printer and a reader to and from an already supported domain."))
  (chapter (:title "Printer")
    (paragraph ()
      "The printer is responsible for outputting the document to the output devices at once. It is done in multiple transformation steps where each step is a transformation from one domain to another. The transformation chain starts from the document that is being edited in the form of its own domain and goes towards the native domains of the output devices. The final transformation step is a straightforward mapping from each native document to the native API calls of each output device. The transformation steps may be done parallel or sequentially with respect to the output devices. Some transformations such as a filter, or a sort have the same input and output domains."))
  (chapter (:title "Reader")
    (paragraph ()
      "The reader is responsible for inputting an operation from the input devices at once. It is done in multiple transformation steps where each step is a transformation from one domain to another. The transformation chain starts from the native domains of the input devices and goes towards the domain of the document that is being edited. The first transformation step is a straightforward mapping from the native API calls of each input device to the native documents. The transformation steps may be done parallel or sequentially with respect to the output devices. Some transformations such as a filter, or a sort have the same input and output domains."))
  (chapter (:title "Device")
    (paragraph ()
      "A device from the point of view of the projectional editor is not necessarily a hardware device. The most important attribute of a device is that it provides a way to communicate with the outside world. There are a few basic input and output devices that must be supported in order to have a usable projectional editor.")
    (paragraph ()
      "The basic input devices are the keyboard, the mouse and the timer. Each device has its own domain specific language that describes the events that they provide.")
    (paragraph ()
      "There is only one basic ouput device that is the graphical display. The display device understands the graphics domain that describes simple graphical primitives such as points, lines, polylines, beziers, rectangles, polygons, circles, ellipses, arcs, texts, images, etc."))
  (chapter (:title "Domain")
    (paragraph ()
      "A domain puts together multiple data structures.")
    (paragraph ()
      "A domain in the context of the projectional editor does not only mean the domain specific language that is used to describe the document. It also includes the possible selections and available operations on them.")
    (chapter (:title "Graphics")
      (paragraph ()
        ""))
    (chapter (:title "String Domain")
      (chapter (:title "String Document")
        (paragraph ()
          "A string document is a sequence of unicode characters."))
      (chapter (:title "Character Range Selection")
        (paragraph ()
          "A character range selection specifies a continuous range of characters within a string document."))
      (chapter (:title "Replace Character Range Operation")
        (paragraph ()
          "A replace character range operation takes a ")))
    (chapter (:title "Text")
      (paragraph ()
        ""))
    (chapter (:title "Boolean")
      (paragraph ()
        ""))
    (chapter (:title "Number")
      (paragraph ()
        "")))
  (chapter (:title "Document")
    (paragraph ()
      "The projectional editor intentionally does not expose any requirements on the document data structures."))
  (chapter (:title "Event")
    (paragraph ()
      ""))
  (chapter (:title "Gesture")
    (paragraph ()
      ""))
  (chapter (:title "Operation")
    (paragraph ()
      ""))
  (chapter (:title "Reference")
    (paragraph ()
      ""))
  (chapter (:title "Selection")
    (paragraph ()
      ""))
  (chapter (:title "Projection")
    (paragraph ()
      "Simultaneously describes how the document is printed to the output devices and how operations are read from the input devices."))
  (chapter (:title "Pending")
    (paragraph ()
      "Changes that would leave the document in an invalid state are represented as changes in one or more documents down the transformation chain."))
  (chapter (:title "Undo")
    (paragraph ()
      "Any operation that has side effects on the document may be undone. The undo history supports nonlinear undo with automatic operation dependency tracking.")))
