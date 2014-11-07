;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection state-machine/state-machine->tree/node ()
  ())

(def projection state-machine/state->tree/node ()
  ())

(def projection state-machine/transition->tree/node ()
  ())

;;;;;;
;;; Construction

(def function make-projection/state-machine/state-machine->tree/node ()
  (make-projection 'state-machine/state-machine->tree/node))

(def function make-projection/state-machine/state->tree/node ()
  (make-projection 'state-machine/state->tree/node))

(def function make-projection/state-machine/transition->tree/node ()
  (make-projection 'state-machine/transition->tree/node))

;;;;;;
;;; Printer

(def printer state-machine/state-machine->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "state machine"))
                                         (make-tree/leaf (text/make-string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (make-tree/leaf (text/make-string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)))
                                       (make-tree/node (list (make-tree/node (list* (bind ((label "states:"))
                                                                                      (make-tree/leaf (text/make-string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))
                                                                                    (iter (for index :from 0)
                                                                                          (for state :in (states-of input))
                                                                                          (for iomap = (recurse-printer recursion iomap state
                                                                                                                        `(elt (the sequence (states-of ,typed-input-reference)) ,index)))
                                                                                          (push iomap child-iomaps)
                                                                                          ;; KLUDGE:
                                                                                          (setf (indentation-of (output-of iomap)) 2)
                                                                                          (collect (output-of iomap))))
                                                                             :indentation 2)
                                                             (make-tree/node (list* (bind ((label "transitions:"))
                                                                                      (make-tree/leaf (text/make-string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))
                                                                                    (iter (for index :from 0)
                                                                                          (for transition :in (transitions-of input))
                                                                                          (for iomap = (recurse-printer recursion iomap transition
                                                                                                                        `(elt (the sequence (transitions-of ,typed-input-reference)) ,index)))
                                                                                          (push iomap child-iomaps)
                                                                                          ;; KLUDGE:
                                                                                          (setf (indentation-of (output-of iomap)) 2)
                                                                                          (collect (output-of iomap))))
                                                                             :indentation 2))
                                                       :opening-delimiter (text/make-string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       ;; KLUDGE:
                                                       :closing-delimiter (text/make-string "
}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :indentation 0))
                                 :separator (text/make-string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer state-machine/state->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "state"))
                                         (make-tree/leaf (text/make-string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (make-tree/leaf (text/make-string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*))))
                                 :closing-delimiter (text/make-string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (text/make-string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer state-machine/transition->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "transition"))
                                         (make-tree/leaf (text/make-string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (make-tree/leaf (text/make-string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)))
                                       (make-tree/node (list (bind ((label "event:"))
                                                               (make-tree/leaf (text/make-string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((event (event-of input)))
                                                               (make-tree/leaf (text/make-string event :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (text/make-string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                             (bind ((label "source:"))
                                                               (make-tree/leaf (text/make-string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((name (name-of (source-of input))))
                                                               (make-tree/leaf (text/make-string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (text/make-string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                             (bind ((label "target:"))
                                                               (make-tree/leaf (text/make-string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((name (name-of (target-of input))))
                                                               (make-tree/leaf (text/make-string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (text/make-string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
                                                       :opening-delimiter (text/make-string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       ;; KLUDGE:
                                                       :closing-delimiter (text/make-string "
    }" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :indentation 0))
                                 :separator (text/make-string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader state-machine/state-machine->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader state-machine/state->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)

(def reader state-machine/transition->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion printer-iomap))
  input)
