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

(def (function e) make-projection/state-machine/state-machine->tree/node ()
  (make-projection 'state-machine/state-machine->tree/node))

(def (function e) make-projection/state-machine/state->tree/node ()
  (make-projection 'state-machine/state->tree/node))

(def (function e) make-projection/state-machine/transition->tree/node ()
  (make-projection 'state-machine/transition->tree/node))

;;;;;;
;;; Printer

(def printer state-machine/state-machine->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "state machine"))
                                         (push (make-iomap/string keyword `(keyword ,typed-input-reference) 0
                                                                  keyword `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 0))) 0
                                                                  (length keyword))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (push (make-iomap/string name `(name-of ,typed-input-reference) 0
                                                                  name `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 1))) 0
                                                                  (length name))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)))
                                       (make-tree/node (list (make-tree/node (list* (bind ((label "states:"))
                                                                                      (push (make-iomap/string label `(label (the sequence (states-of ,typed-input-reference))) 0
                                                                                                               label `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 0)))) 0))) 0
                                                                                                               (length label))
                                                                                            child-iomaps)
                                                                                      (make-tree/leaf (make-text/string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))
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
                                                                                      (push (make-iomap/string label `(label (the sequence (transitions-of ,typed-input-reference))) 0
                                                                                                               label `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 1)))) 0))) 0
                                                                                                               (length label))
                                                                                            child-iomaps)
                                                                                      (make-tree/leaf (make-text/string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)))
                                                                                    (iter (for index :from 0)
                                                                                          (for transition :in (transitions-of input))
                                                                                          (for iomap = (recurse-printer recursion iomap transition
                                                                                                                        `(elt (the sequence (transitions-of ,typed-input-reference)) ,index)))
                                                                                          (push iomap child-iomaps)
                                                                                          ;; KLUDGE:
                                                                                          (setf (indentation-of (output-of iomap)) 2)
                                                                                          (collect (output-of iomap))))
                                                                             :indentation 2))
                                                       :opening-delimiter (make-text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       ;; KLUDGE:
                                                       :closing-delimiter (make-text/string "
}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :indentation 0))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer state-machine/state->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "state"))
                                         (push (make-iomap/string keyword `(keyword ,typed-input-reference) 0
                                                                  keyword `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 0))) 0
                                                                  (length keyword))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (push (make-iomap/string name `(name-of ,typed-input-reference) 0
                                                                  name `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 1))) 0
                                                                  (length name))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*))))
                                 :closing-delimiter (make-text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

(def printer state-machine/transition->tree/node (projection recursion iomap input input-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (list (bind ((keyword "transition"))
                                         (push (make-iomap/string keyword `(keyword ,typed-input-reference) 0
                                                                  keyword `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 0))) 0
                                                                  (length keyword))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string keyword :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
                                       (bind ((name (name-of input)))
                                         (push (make-iomap/string name `(name-of ,typed-input-reference) 0
                                                                  name `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node ,output-reference))) 1))) 0
                                                                  (length name))
                                               child-iomaps)
                                         (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)))
                                       (make-tree/node (list (bind ((label "event:"))
                                                               (push (make-iomap/string label `(label ,typed-input-reference) 0
                                                                                        label `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 0))) 0
                                                                                        (length label))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((event (event-of input)))
                                                               (push (make-iomap/string event `(event-of ,typed-input-reference) 0
                                                                                        event `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 1))) 0
                                                                                        (length event))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string event :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (make-text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                             (bind ((label "source:"))
                                                               (push (make-iomap/string label `(label ,typed-input-reference) 0
                                                                                        label `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 2))) 0
                                                                                        (length label))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((name (name-of (source-of input))))
                                                               (push (make-iomap/string name `(name-of (the state-machine/state (source-of ,typed-input-reference))) 0
                                                                                        name `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 3))) 0
                                                                                        (length name))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (make-text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
                                                             (bind ((label "target:"))
                                                               (push (make-iomap/string label `(label ,typed-input-reference) 0
                                                                                        label `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 4))) 0
                                                                                        (length label))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string label :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*) :indentation 2))
                                                             (bind ((name (name-of (target-of input))))
                                                               (push (make-iomap/string name `(name-of (the state-machine/state (target-of ,typed-input-reference))) 0
                                                                                        name `(content-of (the tree/leaf (elt (the sequence (children-of (the tree/node (elt (the sequence (children-of (the tree/node ,output-reference))) 2)))) 5))) 0
                                                                                        (length name))
                                                                     child-iomaps)
                                                               (make-tree/leaf (make-text/string name :font *font/ubuntu/monospace/bold/18* :font-color *color/solarized/red*)
                                                                               :closing-delimiter (make-text/string ";" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
                                                       :opening-delimiter (make-text/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       ;; KLUDGE:
                                                       :closing-delimiter (make-text/string "
    }" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :indentation 0))
                                 :separator (make-text/string " " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/compound projection recursion input input-reference output
                         (list* (make-iomap/object projection recursion input input-reference output) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader state-machine/state-machine->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (operation/read-backward (operation-of input) printer-iomap))

(def reader state-machine/state->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (operation/read-backward (operation-of input) printer-iomap))

(def reader state-machine/transition->tree/node (projection recursion input printer-iomap)
  (declare (ignore projection recursion))
  (operation/read-backward (operation-of input) printer-iomap))
