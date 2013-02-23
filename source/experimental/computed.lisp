(require :hu.dwim.computed-class+swank)

(require :hu.dwim.computed-class+hu.dwim.defclass-star)

(in-package :hu.dwim.computed-class)

(define-computed-universe projectured ()
  ()
  (:computed-state-factory-name as))

(defcclass* ccons ()
  ((head :computed-in projectured)
   (tail :computed-in projectured)))

(defcclass* clist ()
  ((first-cons :computed-in projectured)))

(defcclass* cstring (clist)
  ())

(defun make-ccons (head tail)
  ;; (print "Cons")
  (make-instance 'ccons :head head :tail tail))

(defun %make-clist (type elements)
  (make-instance type
                 :first-cons (iter (for tail :initially nil :then cons)
                                   (for element :in-sequence (reverse elements))
                                   (for cons = (rebind (element tail)
                                                 (make-ccons (as element) (as tail))))
                                   (finally (return cons)))))

(defun make-clist (elements)
  (%make-clist 'clist elements))

(defun make-cstring (content)
  (%make-clist 'cstring content))

(defmethod print-object ((list clist) stream)
  (princ "(" stream)
  (iter (for cons :initially (first-cons-of list) :then (tail-of cons))
        (while cons)
        (unless (first-iteration-p)
          (princ " " stream))
        (princ (head-of cons) stream))
  (princ ")" stream))

(defmethod print-object ((list cstring) stream)
  (princ "\"" stream)
  (iter (for cons :initially (first-cons-of list) :then (tail-of cons))
        (while cons)
        (princ (head-of cons) stream))
  (princ "\"" stream))

(defun copy-clist (list)
  (make-instance 'clist
                 :first-cons (labels ((recurse (cons)
                                        ;; (print "Recurse")
                                        (if (null cons)
                                            nil
                                            (make-ccons (as (head-of cons))
                                                        (as (recurse (tail-of cons)))))))
                               (as (recurse (first-cons-of list))))))

(defun flatten-clist (list)
  (make-instance 'clist
                 :first-cons (labels ((recurse (tree list)
                                        (cond ((null tree)
                                               list)
                                              ((typep tree 'clist)
                                               (recurse (first-cons-of tree) list))
                                              ((not (typep tree 'ccons))
                                               (make-ccons (as tree) (as list)))
                                              (t
                                               (recurse
                                                (head-of tree) (recurse (tail-of tree) list))))))
                               (as (recurse (first-cons-of list) nil)))))

(defun clist-at (list index)
  (iter (repeat index)
        (for cons :initially (first-cons-of list) :then (tail-of cons))
        (finally (return (head-of cons)))))

(defun (setf clist-at) (new-value list index)
  (iter (repeat index)
        (for cons :initially (first-cons-of list) :then (tail-of cons))
        (finally (return (setf (head-of cons) new-value)))))

(defun insert-at (list index value)
  (if (zerop index)
      (setf (first-cons-of list) (make-ccons (as value) (as (first-cons-of list))))
      (iter (repeat (1- index))
            (for cons :initially (first-cons-of list) :then (tail-of cons))
            (finally (bind ((tail (tail-of cons)))
                       (setf (tail-of cons) (make-ccons (as value) (as tail)))))))
  list)

(defun remove-at (list index)
  (if (zerop index)
      (setf (first-cons-of list) (tail-of (first-cons-of list)))
      (iter (for cons :initially (first-cons-of list) :then (tail-of cons))
            (repeat (1- index))
            (finally (setf (tail-of cons) (tail-of (tail-of cons)))))))

(defun word-wrap (string width)
  (make-instance 'clist
                 :first-cons (labels ((%word-length (remaining)
                                        (if (or (null remaining)
                                                (whitespace? (head-of remaining)))
                                            0
                                            (1+ (%word-length (tail-of remaining)))))
                                      (%word-wrap (collected remaining)
                                        (if (> (+ (length collected)
                                                  (%word-length remaining)) width)
                                            (make-ccons (as (make-cstring (reverse (rest collected))))
                                                        (%word-wrap nil remaining))
                                            (if (null remaining)
                                                nil
                                                (%word-wrap (cons (head-of remaining) collected)
                                                            (tail-of remaining))))))
                               (as (%word-wrap nil (first-cons-of string))))))

(defun test1 ()
  (bind ((l (make-clist '(1 2 3)))
         (c (copy-clist l)))
    (print l)
    (print c)
    (setf (clist-at l 1) 10)
    (print l)
    (print c)
    (insert-at l 1 20)
    (print l)
    (print c)
    (remove-at l 1)
    (print l)
    (print c)
    (values)))

(defun test2 ()
  (bind ((s (make-clist (list (make-clist '(1 2 3)) (make-clist '(4 5 6)) (make-clist '(7 8 9)))))
         (f (flatten-clist s)))
    (print s)
    (print f)
    (insert-at s 1 10)
    (print s)
    (print f)
    (insert-at (clist-at s 2) 1 20)
    (print s)
    (print f)
    (values)))

(defun test3 ()
  (bind ((s (make-cstring "The reader is responsible for inputting an operation from the input devices at once. It is done in multiple transformation steps where each step is a transformation from one domain to another. The transformation chain starts from the native domains of the input devices and goes towards the domain of the document that is being edited. The first transformation step is a straightforward mapping from the native API calls of each input device to the native documents. The transformation steps may be done parallel or sequentially with respect to the output devices. Some transformations such as a filter, or a sort have the same input and output domains."))
         (l (word-wrap s 20)))
    (print s)
    (print l)
    (iter (repeat 8)
          (insert-at s 1 #\X))
    (print s)
    (print l)
    (values)))

(defun test4 ()
  (bind ((tr (make-clist (list (make-clist "The") (make-clist "reader") (make-clist "is") (make-clist "responsible"))))
         (f (flatten-clist tr))
         (l (word-wrap f 20)))
    (print tr)
    (print f)
    (print l)
    (iter (repeat 8)
          (insert-at (clist-at tr 0) 1 #\X))
    (print tr)
    (print f)
    (print l)
    (values)))











(defcclass* iomap ()
  ((input :computed-in projectured)
   (output :computed-in projectured)
   (children :computed-in projectured)))

(defcclass* document ()
  ((content :computed-in projectured)))

(defun test-document ()
  (bind ((document (make-instance 'document :content (make-instance 'document :content 42))))
    (labels ((recurse (input)
               (etypecase input
                 (document
                  (print "copying document")
                  (make-instance 'document :content (as (recurse (content-of input)))))
                 (integer
                  (print "copying integer")
                  input))))
      (bind ((document-copy (recurse document)))
        (setf (content-of (content-of document)) 43)
        (print (content-of (content-of document-copy)))
        (values document document-copy)))))

(defun test-iomap ()
  (bind ((document (make-instance 'document :content (make-instance 'document :content 42))))
    (labels ((recurse (input)
               (etypecase input
                 (document
                     (print "copying document")
                   (bind ((iomap-cs (as (recurse (content-of input)))))
                     (make-instance 'iomap
                                    :input document
                                    :output (as (make-instance 'document :content (as (output-of (computed-state-value iomap-cs)))))
                                    :children (as (list (computed-state-value iomap-cs))))))
                 (integer
                  (print "copying integer")
                  (make-instance 'iomap
                                 :input input
                                 :output input
                                 :children nil))))
             (rprint (iomap)
               (print (input-of iomap))
               (print (output-of iomap))
               (map nil #'rprint (children-of iomap))))
      (bind ((iomap-1 (recurse document))
             (iomap-2 (recurse (output-of iomap-1))))
        (rprint iomap-1)
        (rprint iomap-2)
        (setf (content-of (content-of document)) 43)
        (rprint iomap-1)
        (rprint iomap-2)
        (values document iomap-1 iomap-2)))))
